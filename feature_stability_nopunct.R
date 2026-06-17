lib_path <- "./r_libs"
.libPaths(c(lib_path, .libPaths()))

library(tidyverse)
library(fs)

base       <- "./FANFIC_CORPUS"
ngram_root <- file.path(base, "train_15k_ngrams_nopunct")
out_dir    <- file.path(base, "pos_profiles_nopunct")
tmp_dir    <- file.path(out_dir, "stability_tmp")
dir_create(tmp_dir)

feature_matrix <- read_rds(file.path(out_dir, "feature_matrix.rds"))
features <- tibble(feature = setdiff(names(feature_matrix), "author")) %>%
  mutate(n     = as.integer(str_extract(feature, "(?<=^n)\\d+")),
         ngram = str_replace(feature, "^n\\d+_", ""))

cat(sprintf("Filtering to %d curated features\n", nrow(features)))

author_dirs <- list.dirs(ngram_root, full.names = TRUE, recursive = FALSE)
cat(sprintf("Processing %d authors...\n", length(author_dirs)))

walk(author_dirs, function(d) {
  author   <- basename(d)
  out_file <- file.path(tmp_dir, paste0(author, "_stability.tsv"))
  if (file_exists(out_file)) return(invisible(NULL))
  
  files <- list.files(d, full.names = TRUE, pattern = "_ngrams\\.tsv$")
  files <- files[!grepl("_top_ngrams", files)]
  if (length(files) == 0) return(invisible(NULL))
  
  chunk_data <- map(files, function(f) {
    read_tsv(f, col_types = cols(.default = "c")) %>%
      mutate(rel_freq = as.double(rel_freq), n = as.integer(n))
  }) %>% bind_rows() %>%
    semi_join(features, by = c("ngram", "n"))   # filter to curated features early
  
  stability <- chunk_data %>%
    group_by(ngram, n) %>%
    summarise(
      mean_rf  = mean(rel_freq),
      sd_rf    = sd(rel_freq),
      n_chunks = n(),
      .groups  = "drop"
    ) %>%
    mutate(cv = sd_rf / mean_rf, author = author)
  
  write_tsv(stability, out_file)
  rm(chunk_data, stability)
  gc()
})

cat("Aggregating...\n")

tmp_files <- list.files(tmp_dir, full.names = TRUE, pattern = "_stability\\.tsv$")
cat(sprintf("Reading %d files...\n", length(tmp_files)))

all_stability <- map(tmp_files, function(f) {
  read_tsv(f, col_types = cols(.default = "c")) %>%
    select(ngram, n, cv) %>%
    mutate(cv = as.double(cv), n = as.integer(n)) %>%
    filter(!is.na(cv) & is.finite(cv))
}) %>%
  bind_rows()

cat("Combined rows:", nrow(all_stability), "\n")

feature_stability <- all_stability %>%
  group_by(ngram, n) %>%
  summarise(
    mean_cv      = mean(cv),
    median_cv    = median(cv),
    author_count = n(),
    .groups      = "drop"
  ) %>%
  arrange(mean_cv)

write_tsv(feature_stability, file.path(out_dir, "feature_stability.tsv"))
cat("Done.\n")

print(summary(feature_stability$mean_cv))

feature_stability %>%
  group_by(n) %>%
  summarise(mean_cv = mean(mean_cv, na.rm = TRUE), n_features = n()) %>%
  print()