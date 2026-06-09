library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(fs)

df <- read.csv(file = "./metadata_merged.csv", header = TRUE, fileEncoding = "latin1")



# =============================================================================
# Build Subcorpus: Harry Potter × Liebesgeschichte
# =============================================================================
# Creates: subcorpus_root/
#            └── <author_username>/
#                    └── <text_id>_<text_title>.txt
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Paths – adjust if needed
# -----------------------------------------------------------------------------

corpus_dir   <- "./Texte_txt"
subcorpus_dir <- "./subcorpus_HP_Liebesgeschichte"

# Target values
TARGET_FANDOM <- "Harry Potter"
TARGET_GENRE  <- "Liebesgeschichte"

# -----------------------------------------------------------------------------
# 1. Filter metadata
# -----------------------------------------------------------------------------

subcorpus_meta <- df %>%
  # Normalise multi-value genre column (one row per genre tag)
  separate_rows(genre, sep = ",\\s*") %>%
  mutate(
    genre = trimws(genre),
    # Harmonise genre labels (same logic as your existing code)
    genre = case_when(
      genre %in% c("Romanze", "Romance") ~ "Liebesgeschichte",
      TRUE ~ genre
    ),
    # Harmonise fandom labels
    fandom_categories = case_when(
      fandom_categories == "Haikyuu!! / FF"                              ~ "Haikyuu!!",
      fandom_categories == "My Hero Academia / FF"                       ~ "My Hero Academia",
      fandom_categories == "J.R.R. Tolkien / Mittelerde / Der Hobbit"   ~ "J.R.R. Tolkien / Mittelerde / Der Herr der Ringe",
      TRUE ~ fandom_categories
    )
  ) %>%
  filter(
    str_detect(fandom_categories, fixed(TARGET_FANDOM)),
    genre == TARGET_GENRE
  ) %>%
  # Drop duplicates that may arise after separate_rows if a text had the
  # genre listed multiple times
  distinct(text_id, .keep_all = TRUE)

cat(sprintf("Texts matching filter: %d\n", nrow(subcorpus_meta)))
cat(sprintf("Authors:               %d\n", n_distinct(subcorpus_meta$author_username)))

# -----------------------------------------------------------------------------
# 2. Locate source files
# -----------------------------------------------------------------------------

# List all .txt files in the corpus directory
all_files <- tibble(
  filepath = dir_ls(corpus_dir, glob = "*.txt")
) %>%
  mutate(
    # Extract the text_id: everything before the first underscore or space
    # File name pattern: <text_id>_<title>.txt  or  <text_id> <title>.txt
    filename = path_file(filepath),
    text_id  = str_extract(filename, "^[^_\\s]+")
  )

# Join to find actual paths for our subcorpus texts
matched <- subcorpus_meta %>%
  left_join(all_files, by = "text_id")

n_found   <- sum(!is.na(matched$filepath))
n_missing <- sum( is.na(matched$filepath))

cat(sprintf("Source files found:    %d\n", n_found))
cat(sprintf("Source files missing:  %d\n", n_missing))

if (n_missing > 0) {
  cat("\nMissing text_ids:\n")
  print(filter(matched, is.na(filepath)) %>% select(text_id, text_title, author_username))
}

# -----------------------------------------------------------------------------
# 3. Build folder structure and copy files
# -----------------------------------------------------------------------------

dir_create(subcorpus_dir)   # create root if it doesn't exist

matched_valid <- matched %>% filter(!is.na(filepath))

# Helper: sanitise a string for use as a directory/file name
safe_name <- function(x) {
  x %>%
    str_replace_all('[\\\\/:*?"<>|]', "_") %>%  # remove forbidden chars
    str_trim()
}

results <- matched_valid %>%
  mutate(
    author_safe = safe_name(author_username),
    title_safe  = safe_name(text_title),
    # Destination folder: subcorpus_root/<author>/
    author_dir  = path(subcorpus_dir, author_safe),
    # Destination file:   <text_id>_<title>.txt  (mirrors source naming)
    dest_file   = path(author_dir, paste0(text_id, "_", title_safe, ".txt"))
  )

# Create one folder per author
results %>%
  distinct(author_dir) %>%
  pull(author_dir) %>%
  walk(dir_create)

# Copy files
copy_status <- results %>%
  rowwise() %>%
  mutate(
    copied = tryCatch({
      file_copy(filepath, dest_file, overwrite = TRUE)
      TRUE
    }, error = function(e) {
      message(sprintf("ERROR copying %s: %s", text_id, e$message))
      FALSE
    })
  ) %>%
  ungroup()

cat(sprintf("\nFiles copied successfully: %d / %d\n",
            sum(copy_status$copied), nrow(copy_status)))

# -----------------------------------------------------------------------------
# 4. Save a manifest (optional but recommended)
# -----------------------------------------------------------------------------

manifest <- copy_status %>%
  select(text_id, text_title, author_username, fandom_categories, genre,
         wordcount, creation_date, copied, dest_file)

manifest_path <- path(subcorpus_dir, "_manifest_HP_Liebesgeschichte.csv")
write.csv(manifest, manifest_path, row.names = FALSE, fileEncoding = "UTF-8")
cat(sprintf("Manifest saved to: %s\n", manifest_path))

# -----------------------------------------------------------------------------
# 5. Quick summary
# -----------------------------------------------------------------------------

cat("\n--- Subcorpus summary ---\n")
cat(sprintf("Root:    %s\n", subcorpus_dir))
cat(sprintf("Authors: %d\n", n_distinct(copy_status$author_username)))
cat(sprintf("Texts:   %d\n", sum(copy_status$copied)))
cat(sprintf("Words:   %s\n", format(sum(copy_status$wordcount, na.rm = TRUE), big.mark = ",")))