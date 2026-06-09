# =============================================================================
# Build CLEAN Subcorpus: Harry Potter × Liebesgeschichte
# =============================================================================
# Source:  subcorpus_HP_Liebesgeschichte/  (output of previous script)
# Output:  subcorpus_HP_Liebesgeschichte_CLEAN/
#            └── <author_id>/           # numeric ID, not real name
#                    ├── <text_id>.txt          # full original text
#                    └── <text_id>_chunk001.txt # 1000-word chunks (sentence boundary)
#                        <text_id>_chunk002.txt
#                        ...
#
# Inclusion rule: author must have >1 text OR a single text of ≥2000 words
# Chunking:       target 1000 words, split at sentence boundary, drop final
#                 chunk if <750 words
# =============================================================================

library(dplyr)
library(stringr)
library(fs)
library(purrr)

# -----------------------------------------------------------------------------
# 0. Paths
# -----------------------------------------------------------------------------

source_dir <- "./subcorpus_HP_Liebesgeschichte"
clean_dir  <- "./subcorpus_HP_Liebesgeschichte_CLEAN"

manifest_path <- path(source_dir, "_manifest_HP_Liebesgeschichte.csv")

CHUNK_TARGET   <- 1000L   # target words per chunk
CHUNK_MIN      <- 750L    # drop final chunk if below this
MIN_WORDS_SOLO <- 2000L   # word threshold for single-text authors

# -----------------------------------------------------------------------------
# 1. Load manifest and apply inclusion filter
# -----------------------------------------------------------------------------

manifest <- read.csv(manifest_path, stringsAsFactors = FALSE, encoding = "UTF-8") %>%
  filter(copied == TRUE)   # only rows that were actually copied last time

# Per-author text count and check
author_stats <- manifest %>%
  group_by(author_username) %>%
  summarise(
    text_count  = n(),
    max_words   = max(wordcount, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  mutate(
    include = text_count > 1 | max_words >= MIN_WORDS_SOLO
  )

# Quick pre-flight report
cat("=== Author inclusion check ===\n")
cat(sprintf("Total authors in manifest:     %d\n", nrow(author_stats)))
cat(sprintf("Authors with >1 text:          %d\n", sum(author_stats$text_count > 1)))
cat(sprintf("Authors with 1 text ≥2000 w:   %d\n",
            sum(author_stats$text_count == 1 & author_stats$max_words >= MIN_WORDS_SOLO)))
cat(sprintf("Authors EXCLUDED:              %d\n", sum(!author_stats$include)))
cat(sprintf("Authors INCLUDED:              %d\n", sum(author_stats$include)))

excluded_authors <- author_stats %>% filter(!include)
if (nrow(excluded_authors) > 0) {
  cat("\nExcluded authors (username | texts | max_words):\n")
  excluded_authors %>%
    select(author_username, text_count, max_words) %>%
    print(n = Inf)
}

# Filter manifest to included authors only
clean_meta <- manifest %>%
  semi_join(filter(author_stats, include), by = "author_username")

cat(sprintf("\nTexts carried forward: %d\n\n", nrow(clean_meta)))

# -----------------------------------------------------------------------------
# 2. Build author ID lookup table
# -----------------------------------------------------------------------------

author_lookup <- author_stats %>%
  filter(include) %>%
  arrange(author_username) %>%
  mutate(author_id = sprintf("author%04d", row_number()))  # e.g. author0001

# Save lookup so you can always recover who is who
lookup_path <- path(clean_dir, "author_id_lookup.csv")

# Attach author_id to clean_meta
clean_meta <- clean_meta %>%
  left_join(select(author_lookup, author_username, author_id), by = "author_username")

# -----------------------------------------------------------------------------
# 3. Chunking helper functions
# -----------------------------------------------------------------------------

# Split text into ~1000-word chunks ending at a sentence boundary.
# Returns a character vector of chunk texts.
chunk_text <- function(text, target = CHUNK_TARGET, min_final = CHUNK_MIN) {
  
  # Tokenise into sentences using a simple regex sentence splitter.
  # Splits after . ! ? followed by whitespace or end-of-string,
  # keeping the delimiter attached to the preceding sentence.
  sentences <- unlist(str_split(text, "(?<=[.!?])\\s+"))
  sentences <- sentences[nchar(trimws(sentences)) > 0]
  
  if (length(sentences) == 0) return(character(0))
  
  chunks     <- list()
  current    <- character(0)
  word_count <- 0L
  
  for (sent in sentences) {
    sent_words  <- str_count(sent, "\\S+")
    word_count  <- word_count + sent_words
    current     <- c(current, sent)
    
    if (word_count >= target) {
      chunks     <- c(chunks, list(paste(current, collapse = " ")))
      current    <- character(0)
      word_count <- 0L
    }
  }
  
  # Handle remaining sentences (potential final chunk)
  if (length(current) > 0) {
    remaining_words <- str_count(paste(current, collapse = " "), "\\S+")
    if (remaining_words >= min_final) {
      chunks <- c(chunks, list(paste(current, collapse = " ")))
    }
    # else: silently drop — original full text is preserved anyway
  }
  
  unlist(chunks)
}

# Write chunks to disk; returns number of chunks written
write_chunks <- function(text_id, text, author_dir) {
  chunks <- chunk_text(text)
  if (length(chunks) == 0) return(0L)
  iwalk(chunks, function(chunk_text, idx) {
    chunk_file <- path(author_dir, sprintf("%s_chunk%03d.txt", text_id, idx))
    writeLines(chunk_text, chunk_file, useBytes = FALSE)
  })
  length(chunks)
}

# -----------------------------------------------------------------------------
# 4. Create clean subcorpus folder structure
# -----------------------------------------------------------------------------

dir_create(clean_dir)

# Create author ID folders
walk(author_lookup$author_id, ~ dir_create(path(clean_dir, .x)))

# -----------------------------------------------------------------------------
# 5. Copy originals + write chunks
# -----------------------------------------------------------------------------

cat("Processing texts...\n")

results <- clean_meta %>%
  rowwise() %>%
  mutate(
    author_dir_clean = path(clean_dir, author_id),
    dest_original    = path(author_dir_clean, paste0(text_id, ".txt")),
    
    process_result = list(tryCatch({        # <-- wrap in list()
      
      # --- copy original ---
      file_copy(dest_file, dest_original, overwrite = TRUE)
      
      # --- read and chunk ---
      raw_text <- paste(readLines(dest_original, warn = FALSE), collapse = "\n")
      n_chunks <- write_chunks(text_id, raw_text, author_dir_clean)
      
      list(ok = TRUE, n_chunks = n_chunks, error = NA_character_)
      
    }, error = function(e) {
      list(ok = FALSE, n_chunks = 0L, error = e$message)
    })),                                    # <-- closing list() here
    
    ok       = process_result$ok,
    n_chunks = process_result$n_chunks,
    error    = process_result$error
  ) %>%
  ungroup()

# -----------------------------------------------------------------------------
# 6. Report
# -----------------------------------------------------------------------------

cat(sprintf("\n=== Clean subcorpus summary ===\n"))
cat(sprintf("Root:              %s\n", clean_dir))
cat(sprintf("Authors:           %d\n", n_distinct(results$author_id)))
cat(sprintf("Texts processed:   %d / %d\n", sum(results$ok), nrow(results)))
cat(sprintf("Total chunks:      %d\n", sum(results$n_chunks)))
cat(sprintf("Avg chunks/text:   %.1f\n", mean(results$n_chunks[results$ok])))

if (any(!results$ok)) {
  cat("\nErrors:\n")
  results %>%
    filter(!ok) %>%
    select(text_id, author_username, error) %>%
    print(n = Inf)
}

# -----------------------------------------------------------------------------
# 7. Save manifest + author lookup
# -----------------------------------------------------------------------------

clean_manifest <- results %>%
  select(author_id, author_username, text_id, text_title,
         wordcount, n_chunks, ok, error)

write.csv(clean_manifest,
          path(clean_dir, "_manifest_CLEAN.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

write.csv(select(author_lookup, author_id, author_username, text_count, max_words),
          lookup_path,
          row.names = FALSE, fileEncoding = "UTF-8")

cat(sprintf("\nManifest saved:      %s\n", path(clean_dir, "_manifest_CLEAN.csv")))
cat(sprintf("Author lookup saved: %s\n", lookup_path))



 #----------------------------------------


# =============================================================================
# Further filter CLEAN subcorpus: keep authors with ≥10 chunks total
# =============================================================================
# Reads _manifest_CLEAN.csv, identifies authors below threshold,
# deletes their folders from subcorpus_HP_Liebesgeschichte_CLEAN,
# and updates the manifest and author lookup in place.
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Paths
# -----------------------------------------------------------------------------

clean_dir     <- "./subcorpus_HP_Liebesgeschichte_CLEAN"
manifest_path <- path(clean_dir, "_manifest_CLEAN.csv")
lookup_path   <- path(clean_dir, "author_id_lookup.csv")

MIN_CHUNKS <- 10L

# -----------------------------------------------------------------------------
# 1. Load manifest and summarise chunks per author
# -----------------------------------------------------------------------------

manifest <- read.csv(manifest_path, stringsAsFactors = FALSE, encoding = "UTF-8")

author_chunks <- manifest %>%
  filter(ok == TRUE) %>%
  group_by(author_id, author_username) %>%
  summarise(
    total_chunks = sum(n_chunks, na.rm = TRUE),
    total_texts  = n(),
    .groups      = "drop"
  ) %>%
  mutate(keep = total_chunks >= MIN_CHUNKS)

# --- Pre-flight report -------------------------------------------------------
cat("=== Chunk filter check ===\n")
cat(sprintf("Authors currently in clean corpus: %d\n", nrow(author_chunks)))
cat(sprintf("Authors with ≥%d chunks (KEEP):    %d\n", MIN_CHUNKS, sum(author_chunks$keep)))
cat(sprintf("Authors with <%d chunks (DROP):    %d\n", MIN_CHUNKS, sum(!author_chunks$keep)))
cat(sprintf("Texts affected by drop:            %d\n",
            sum(manifest$author_id %in% filter(author_chunks, !keep)$author_id)))

cat("\nAuthors to be dropped (author_id | username | chunks | texts):\n")
author_chunks %>%
  filter(!keep) %>%
  arrange(total_chunks) %>%
  print(n = Inf)

# -----------------------------------------------------------------------------
# 2. Confirm before deleting  (comment out if running non-interactively)
# -----------------------------------------------------------------------------

cat("\nProceed with deletion? Type YES to continue: ")
confirm <- readLines(con = stdin(), n = 1)
if (trimws(confirm) != "YES") {
  stop("Aborted by user.")
}

# -----------------------------------------------------------------------------
# 3. Delete author folders for dropped authors
# -----------------------------------------------------------------------------

to_drop <- author_chunks %>% filter(!keep)

walk(to_drop$author_id, function(aid) {
  author_folder <- path(clean_dir, aid)
  if (dir_exists(author_folder)) {
    dir_delete(author_folder)
    cat(sprintf("Deleted: %s\n", author_folder))
  } else {
    cat(sprintf("Folder not found (skipped): %s\n", author_folder))
  }
})

# -----------------------------------------------------------------------------
# 4. Update manifest and lookup in place
# -----------------------------------------------------------------------------

manifest_updated <- manifest %>%
  filter(author_id %in% filter(author_chunks, keep)$author_id)

lookup <- read.csv(lookup_path, stringsAsFactors = FALSE, encoding = "UTF-8")

lookup_updated <- lookup %>%
  filter(author_id %in% filter(author_chunks, keep)$author_id)

write.csv(manifest_updated, manifest_path, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(lookup_updated,   lookup_path,   row.names = FALSE, fileEncoding = "UTF-8")

# -----------------------------------------------------------------------------
# 5. Final summary
# -----------------------------------------------------------------------------

cat("\n=== Updated corpus summary ===\n")
cat(sprintf("Authors retained:  %d\n",   n_distinct(manifest_updated$author_id)))
cat(sprintf("Texts retained:    %d\n",   nrow(manifest_updated)))
cat(sprintf("Total chunks:      %d\n",   sum(manifest_updated$n_chunks, na.rm = TRUE)))
cat(sprintf("Avg chunks/author: %.1f\n",
            sum(manifest_updated$n_chunks, na.rm = TRUE) / n_distinct(manifest_updated$author_id)))




