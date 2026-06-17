lib_path <- "./r_libs"
.libPaths(c(lib_path, .libPaths()))

library(tidyverse)

base    <- "./FANFIC_CORPUS"
out_dir <- file.path(base, "pos_profiles")

delta     <- read_tsv(file.path(out_dir, "feature_cliffs_delta.tsv"), show_col_types = FALSE)
stability <- read_tsv(file.path(out_dir, "feature_stability.tsv"), show_col_types = FALSE)

combined <- delta %>%
  inner_join(stability, by = c("ngram", "n")) %>%
  select(ngram, n, mean_abs_delta, mean_cv, median_cv, n_pairs, author_count)

cat("Combined features:", nrow(combined), "\n")

# ── Inspect the relationship before deciding cutoffs ──────────────────────────
cat("\nCorrelation between mean_abs_delta and mean_cv:\n")
print(cor(combined$mean_abs_delta, combined$mean_cv))

cat("\nSummary by n:\n")
combined %>%
  group_by(n) %>%
  summarise(mean_delta = mean(mean_abs_delta), mean_cv = mean(mean_cv), n_features = n()) %>%
  print()

# ── Composite ranking score ────────────────────────────────────────────────────
# Higher delta = more discriminative (good), lower CV = more stable (good)
# Normalise both to 0-1 range so they're comparable, then combine
combined <- combined %>%
  mutate(
    delta_norm = (mean_abs_delta - min(mean_abs_delta)) / (max(mean_abs_delta) - min(mean_abs_delta)),
    cv_norm    = (mean_cv - min(mean_cv)) / (max(mean_cv) - min(mean_cv)),
    # Stability score is inverted CV (1 - cv_norm), so higher = more stable
    stability_score = 1 - cv_norm,
    composite_score = (delta_norm + stability_score) / 2
  ) %>%
  arrange(desc(composite_score))

# ── Restrict to the trustworthy n-gram range (n = 1 to 5) given the sparsity issue ──
filtered_features <- combined %>%
  filter(n <= 5) %>%
  arrange(desc(composite_score))

write_tsv(combined, file.path(out_dir, "feature_combined_all.tsv"))
write_tsv(filtered_features, file.path(out_dir, "feature_combined_filtered_n1to5.tsv"))

cat("\nTop 20 features overall (composite score):\n")
print(head(combined, 20))

cat("\nTop 20 features, restricted to n=1-5:\n")
print(head(filtered_features, 20))

cat("\nFeature counts by n (filtered set):\n")
filtered_features %>% count(n) %>% print()