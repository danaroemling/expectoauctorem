lib_path <- "./r_libs"
.libPaths(c(lib_path, .libPaths()))

library(tidyverse)
library(fs)

# ── Paths ──────────────────────────────────────────────────────────────────────
base     <- "."
src_root <- file.path(base, "train_15k_tagged")
dst_root <- file.path(base, "train_15k_ngrams")

# ── SLURM array task ───────────────────────────────────────────────────────────
author_dirs <- list.dirs(src_root, full.names = FALSE, recursive = FALSE)
task_id     <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
author      <- author_dirs[task_id]

cat(sprintf("[Task %d] %s\n", task_id, author))

# ── N-gram helper ──────────────────────────────────────────────────────────────
get_ngrams <- function(pos_sequence, n) {
  len <- length(pos_sequence)
  if (len < n) return(tibble(ngram = character(), n = integer(), count = integer()))
  
  ngrams <- sapply(1:(len - n + 1), function(i) {
    paste(pos_sequence[i:(i + n - 1)], collapse = "-")
  })
  
  tibble(ngram = ngrams, n = n) %>%
    count(ngram, n, name = "count")
}

# ── Process one chunk file ─────────────────────────────────────────────────────
process_chunk <- function(tsv_path) {
  df <- read_tsv(tsv_path, col_types = cols(.default = "c"))
  
  pos_seq <- df$upos[!is.na(df$upos)]
  
  map_dfr(1:10, ~get_ngrams(pos_seq, .x)) %>%
    mutate(
      total_tokens = length(pos_seq),
      rel_freq     = count / total_tokens,
      chunk        = basename(tsv_path)
    )
}

# ── Process all chunks for this author ────────────────────────────────────────
src_author <- file.path(src_root, author)
dst_author <- file.path(dst_root, author)
dir_create(dst_author)

files <- list.files(src_author, full.names = TRUE)

chunk_results <- map(files, function(f) {
  out_path <- file.path(dst_author, str_replace(basename(f), "\\.tsv$", "_ngrams.tsv"))
  if (file_exists(out_path)) {
    return(read_tsv(out_path, col_types = cols(.default = "c")) %>%
             mutate(count = as.integer(count), n = as.integer(n),
                    rel_freq = as.double(rel_freq), total_tokens = as.integer(total_tokens)))
  }
  tryCatch({
    result <- process_chunk(f)
    write_tsv(result, out_path)
    result
  }, error = function(e) {
    cat(sprintf("ERROR in %s: %s\n", basename(f), e$message))
    NULL
  })
}) %>%
  compact()

# ── Aggregate to author level ──────────────────────────────────────────────────
# ── Aggregate to author level ──────────────────────────────────────────────────
all_chunks <- bind_rows(chunk_results)

# Total tokens is the same for all rows within a chunk, so get it once per chunk
total_tokens_author <- all_chunks %>%
  distinct(chunk, total_tokens) %>%
  summarise(total_tokens = sum(total_tokens)) %>%
  pull(total_tokens)

author_counts <- all_chunks %>%
  group_by(ngram, n) %>%
  summarise(
    count = sum(count),
    .groups = "drop"
  ) %>%
  mutate(
    total_tokens = total_tokens_author,
    rel_freq     = count / total_tokens
  )

# Top 50 per n
author_top <- author_counts %>%
  group_by(n) %>%
  slice_max(order_by = count, n = 50) %>%
  ungroup()

write_tsv(author_top, file.path(dst_author, paste0(author, "_top_ngrams.tsv")))

cat(sprintf("[Task %d] Done: %s\n", task_id, author))