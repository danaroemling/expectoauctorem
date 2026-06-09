library(tidyverse)
library(fs)

# ── Paths ──────────────────────────────────────────────────────────────────────
manifest_path <- "./subcorpus_HP_Liebesgeschichte_TRAIN-TEST/_manifest_TRAIN-TEST.csv"
train_src     <- "./subcorpus_HP_Liebesgeschichte_TRAIN-TEST/train"
test_src      <- "./subcorpus_HP_Liebesgeschichte_TRAIN-TEST/test"
train_dst     <- "./train_15k"
test_dst      <- "./test_15k"

# ── Load manifest ──────────────────────────────────────────────────────────────
manifest <- read_csv(manifest_path)

# Peek at structure so you can adjust column names below if needed
# glimpse(manifest)

author_dirs <- list.dirs(train_src, full.names = FALSE, recursive = FALSE)

file_counts <- sapply(author_dirs, function(a) {
  length(list.files(file.path(train_src, a)))
})

qualifying_authors <- names(file_counts[file_counts >= 15])

cat(sprintf("Authors with ≥15 chunks: %d / %d\n",
            length(qualifying_authors), length(author_dirs)))

# ── Copy qualifying authors: train ────────────────────────────────────────────
dir_create(train_dst)

walk(qualifying_authors, function(author) {
  dir_copy(file.path(train_src, author),
           file.path(train_dst, author),
           overwrite = TRUE)
})



dir_create(test_dst)

walk(qualifying_authors, function(author) {
  src <- file.path(test_src, author)
  if (dir_exists(src)) {
    dir_copy(src, file.path(test_dst, author), overwrite = TRUE)
  }
})

# ── Summary ───────────────────────────────────────────────────────────────────
cat(sprintf("train_15k authors: %d\n", length(list.dirs(train_dst, recursive = FALSE))))
cat(sprintf("test_15k  authors: %d\n", length(list.dirs(test_dst,  recursive = FALSE))))




