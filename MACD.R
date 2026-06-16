lib_path <- "./r_libs"
.libPaths(c(lib_path, .libPaths()))

library(tidyverse)
library(fs)

base      <- "./FANFIC_CORPUS"
out_dir   <- file.path(base, "pos_profiles")
batch_dir <- file.path(out_dir, "cliffs_delta_batches")

batch_files <- list.files(batch_dir, full.names = TRUE, pattern = "batch_.*\\.tsv$")
cat(sprintf("Reading %d batch files...\n", length(batch_files)))

all_deltas <- map(batch_files, function(f) {
  read_tsv(f, col_types = cols(.default = "c")) %>%
    select(ngram, n, delta) %>%
    mutate(delta = as.double(delta), n = as.integer(n)) %>%
    filter(!is.na(delta) & is.finite(delta))
}) %>%
  bind_rows()

cat("Combined rows:", nrow(all_deltas), "\n")

feature_delta <- all_deltas %>%
  mutate(abs_delta = abs(delta)) %>%
  group_by(ngram, n) %>%
  summarise(
    mean_abs_delta = mean(abs_delta),
    mean_delta     = mean(delta),
    n_pairs        = n(),
    .groups        = "drop"
  ) %>%
  arrange(desc(mean_abs_delta))

write_tsv(feature_delta, file.path(out_dir, "feature_cliffs_delta.tsv"))
cat("Done.\n")

cat("\nTop 20 most discriminative features (highest mean |delta|):\n")
print(head(feature_delta, 20))

cat("\nBottom 20 least discriminative features:\n")
print(tail(feature_delta, 20))

cat("\nMean |delta| by n-gram size:\n")
feature_delta %>%
  group_by(n) %>%
  summarise(mean_abs_delta = mean(mean_abs_delta), n_features = n()) %>%
  print()