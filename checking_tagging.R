lib_path <- "./r_libs"
.libPaths(c(lib_path, .libPaths()))

library(tidyverse)
library(fs)

base   <- "./FANFIC_CORPUS"
tagged <- file.path(base, "train_15k_tagged")

# ── Sample a handful of chunk files ───────────────────────────────────────────
author_dirs <- list.dirs(tagged, full.names = TRUE, recursive = FALSE)
set.seed(1)
sample_dirs <- sample(author_dirs, 10)

sample_files <- map(sample_dirs, function(d) {
  files <- list.files(d, full.names = TRUE)
  sample(files, min(2, length(files)))
}) %>% unlist()

cat(sprintf("Sampling from %d files\n", length(sample_files)))

# ── Build token + POS n-grams together, for n = 1 to 10 ──────────────────────
build_ngram_examples <- function(tsv_path, n_per_chunk = 5) {
  df <- read_tsv(tsv_path, col_types = cols(.default = "c"))
  
  tokens <- df$token
  upos   <- df$upos
  len    <- length(tokens)
  
  map_dfr(1:10, function(n) {
    if (len < n) return(NULL)
    
    starts <- 1:(len - n + 1)
    # sample a few starting positions per chunk per n
    sampled_starts <- sample(starts, min(n_per_chunk, length(starts)))
    
    map_dfr(sampled_starts, function(i) {
      tibble(
        n          = n,
        pos_ngram  = paste(upos[i:(i + n - 1)], collapse = "-"),
        word_ngram = paste(tokens[i:(i + n - 1)], collapse = " "),
        chunk      = basename(tsv_path)
      )
    })
  })
}

examples <- map_dfr(sample_files, build_ngram_examples)

cat("Total examples:", nrow(examples), "\n")

# ── Export ─────────────────────────────────────────────────────────────────
out_file <- file.path(base, "ngram_word_examples.tsv")
write_tsv(examples, out_file)
cat("Saved to:", out_file, "\n")

# Quick look
examples %>% group_by(n) %>% slice_head(n = 3) %>% print(n = 30)