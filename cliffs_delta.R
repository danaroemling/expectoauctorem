lib_path <- "./r_libs"
.libPaths(c(lib_path, .libPaths()))

library(tidyverse)
library(fs)

base       <- "./FANFIC_CORPUS"
ngram_root <- file.path(base, "train_15k_ngrams")
out_dir    <- file.path(base, "pos_profiles", "cliffs_delta_batches")
dir_create(out_dir)

CHUNK_CAP <- 50
N_TASKS   <- 1000
set.seed(42)

# ── Feature list ──────────────────────────────────────────────────────────────
feature_matrix <- read_rds(file.path(base, "pos_profiles", "feature_matrix.rds"))
features <- tibble(feature = setdiff(names(feature_matrix), "author")) %>%
  mutate(n     = as.integer(str_extract(feature, "(?<=^n)\\d+")),
         ngram = str_replace(feature, "^n\\d+_", ""))

# ── Author list and pairs ─────────────────────────────────────────────────────
author_dirs <- list.dirs(ngram_root, full.names = FALSE, recursive = FALSE)
n_authors   <- length(author_dirs)
all_pairs   <- combn(n_authors, 2, simplify = FALSE)
n_pairs     <- length(all_pairs)

task_id     <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
batch_idx   <- split(seq_len(n_pairs), cut(seq_len(n_pairs), N_TASKS, labels = FALSE))
my_pairs    <- all_pairs[batch_idx[[task_id]]]

cat(sprintf("[Task %d] Processing %d pairs\n", task_id, length(my_pairs)))

# ── Helper: load one author's chunk data ──────────────────────────────────────
load_author <- function(author) {
  d     <- file.path(ngram_root, author)
  files <- list.files(d, full.names = TRUE, pattern = "_ngrams\\.tsv$")
  files <- files[!grepl("_top_ngrams", files)]
  if (length(files) > CHUNK_CAP) files <- sample(files, CHUNK_CAP)
  map(files, function(f) {
    read_tsv(f, col_types = cols(.default = "c")) %>%
      mutate(rel_freq = as.double(rel_freq), n = as.integer(n),
             chunk = basename(f))
  }) %>% bind_rows()
}

# ── Cliff's delta function ────────────────────────────────────────────────────
cliffs_delta <- function(x, y) {
  dominance <- sum(outer(x, y, ">")) - sum(outer(x, y, "<"))
  dominance / (length(x) * length(y))
}

# ── Process pairs ─────────────────────────────────────────────────────────────
results <- map_dfr(my_pairs, function(pair) {
  author_a <- author_dirs[pair[1]]
  author_b <- author_dirs[pair[2]]
  
  chunks_a <- load_author(author_a)
  chunks_b <- load_author(author_b)
  
  n_chunks_a <- n_distinct(chunks_a$chunk)
  n_chunks_b <- n_distinct(chunks_b$chunk)
  
  map_dfr(1:nrow(features), function(i) {
    ng <- features$ngram[i]
    n  <- features$n[i]
    
    x <- chunks_a %>% filter(ngram == ng, .data$n == n) %>% pull(rel_freq)
    y <- chunks_b %>% filter(ngram == ng, .data$n == n) %>% pull(rel_freq)
    
    if (length(x) == 0) x <- rep(0, n_chunks_a)
    if (length(y) == 0) y <- rep(0, n_chunks_b)
    
    tibble(author_a = author_a, author_b = author_b,
           ngram = ng, n = n, delta = cliffs_delta(x, y))
  })
})

write_tsv(results, file.path(out_dir, sprintf("batch_%04d.tsv", task_id)))
cat(sprintf("[Task %d] Done\n", task_id))