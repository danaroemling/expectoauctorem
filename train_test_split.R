# =============================================================================
# Train / Test split — HP Liebesgeschichte clean corpus
# =============================================================================
# For each author: shuffle their chunks, put 70% in train, 30% in test.
# Each author is present in both splits.
#
# Source:  subcorpus_HP_Liebesgeschichte_CLEAN/<author_id>/<text_id>_chunkNNN.txt
# Output:  subcorpus_HP_Liebesgeschichte_TRAIN-TEST/
#            ├── train/<author_id>/<text_id>_chunkNNN.txt
#            └── test/<author_id>/<text_id>_chunkNNN.txt
# =============================================================================

library(dplyr)
library(fs)
library(purrr)

set.seed(42)   # reproducibility — change or remove if you want a fresh draw

# -----------------------------------------------------------------------------
# 0. Paths
# -----------------------------------------------------------------------------

clean_dir    <- "./subcorpus_HP_Liebesgeschichte_CLEAN"
split_dir    <- "./subcorpus_HP_Liebesgeschichte_TRAIN-TEST"
train_dir    <- path(split_dir, "train")
test_dir     <- path(split_dir, "test")

manifest_path <- path(clean_dir, "_manifest_CLEAN.csv")

TRAIN_RATIO  <- 0.70

# -----------------------------------------------------------------------------
# 1. Collect all chunk files per author
# -----------------------------------------------------------------------------

# List only chunk files (exclude full originals — those have no _chunkNNN suffix)
all_chunks <- tibble(
  filepath = dir_ls(clean_dir, recurse = TRUE, glob = "*_chunk*.txt")
) %>%
  mutate(
    author_id = path_file(path_dir(filepath)),   # parent folder name
    filename  = path_file(filepath)
  )

cat(sprintf("Total chunk files found: %d\n",   nrow(all_chunks)))
cat(sprintf("Authors found:           %d\n\n", n_distinct(all_chunks$author_id)))

# -----------------------------------------------------------------------------
# 2. Split per author: 70% train, 30% test
# -----------------------------------------------------------------------------

split_index <- all_chunks %>%
  group_by(author_id) %>%
  # Shuffle within author
  slice_sample(prop = 1) %>%
  mutate(
    n_total = n(),
    n_train = round(n_total * TRAIN_RATIO),
    rank    = row_number(),
    split   = if_else(rank <= n_train, "train", "test")
  ) %>%
  ungroup()

# Sanity check per author
author_summary <- split_index %>%
  group_by(author_id) %>%
  summarise(
    total = n(),
    train = sum(split == "train"),
    test  = sum(split == "test"),
    .groups = "drop"
  )

cat("Split summary (first 10 authors):\n")
print(head(author_summary, 10))

cat(sprintf("\nTotal train chunks: %d (%.1f%%)\n",
            sum(author_summary$train),
            100 * sum(author_summary$train) / nrow(split_index)))
cat(sprintf("Total test chunks:  %d (%.1f%%)\n",
            sum(author_summary$test),
            100 * sum(author_summary$test) / nrow(split_index)))

# Check every author is in both splits
authors_in_both <- author_summary %>%
  filter(train > 0, test > 0) %>%
  nrow()
cat(sprintf("Authors present in both splits: %d / %d\n\n",
            authors_in_both, n_distinct(split_index$author_id)))

# Flag any author who ended up with 0 in either split (edge case: very few chunks)
problem_authors <- author_summary %>% filter(train == 0 | test == 0)
if (nrow(problem_authors) > 0) {
  cat("WARNING — these authors are missing from one split:\n")
  print(problem_authors)
}

# -----------------------------------------------------------------------------
# 3. Create folder structure
# -----------------------------------------------------------------------------

dir_create(train_dir)
dir_create(test_dir)

# Author folders in both splits
walk(unique(split_index$author_id), function(aid) {
  dir_create(path(train_dir, aid))
  dir_create(path(test_dir,  aid))
})

# -----------------------------------------------------------------------------
# 4. Copy chunks to train / test
# -----------------------------------------------------------------------------

cat("Copying files...\n")

results <- split_index %>%
  mutate(
    dest = path(split_dir, split, author_id, filename),
    ok   = map2_lgl(filepath, dest, function(src, dst) {
      tryCatch({
        file_copy(src, dst, overwrite = TRUE)
        TRUE
      }, error = function(e) {
        message(sprintf("ERROR: %s — %s", path_file(src), e$message))
        FALSE
      })
    })
  )

cat(sprintf("Files copied successfully: %d / %d\n", sum(results$ok), nrow(results)))

# -----------------------------------------------------------------------------
# 5. Save split manifest
# -----------------------------------------------------------------------------

split_manifest <- results %>%
  select(author_id, filename, split, ok) %>%
  mutate(text_id = str_extract(filename, "^[^_]+(?:_[^c][^h][^u][^n][^k])*"))

# Cleaner: extract text_id as everything before _chunkNNN
split_manifest <- results %>%
  select(author_id, filename, split, ok) %>%
  mutate(text_id = str_remove(filename, "_chunk\\d+\\.txt$"))

write.csv(split_manifest,
          path(split_dir, "_manifest_TRAIN-TEST.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cat(sprintf("\nManifest saved to: %s\n", path(split_dir, "_manifest_TRAIN-TEST.csv")))

# -----------------------------------------------------------------------------
# 6. Final summary
# -----------------------------------------------------------------------------

cat("\n=== Train/Test split summary ===\n")
cat(sprintf("Root:          %s\n", split_dir))
cat(sprintf("Authors:       %d\n", n_distinct(results$author_id)))
cat(sprintf("Train chunks:  %d\n", sum(results$split == "train" & results$ok)))
cat(sprintf("Test chunks:   %d\n", sum(results$split == "test"  & results$ok)))



