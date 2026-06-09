options(bitmapType='cairo')

lib_path <- "./r_libs"
.libPaths(c(lib_path, .libPaths()))

library(tidyverse)
library(fs)

# ── Paths ──────────────────────────────────────────────────────────────────────
base       <- "."
ngram_root <- file.path(base, "train_15k_ngrams")
out_dir    <- file.path(base, "pos_profiles")
dir_create(out_dir)

# ── Load all author top-ngram files ───────────────────────────────────────────
author_dirs <- list.dirs(ngram_root, full.names = TRUE, recursive = FALSE)

cat("Loading author ngram files...\n")

all_authors <- map(author_dirs, function(d) {
  author <- basename(d)
  f      <- file.path(d, paste0(author, "_top_ngrams.tsv"))
  if (!file_exists(f)) return(NULL)
  read_tsv(f, col_types = cols(.default = "c")) %>%
    mutate(author = author, rel_freq = as.double(rel_freq))
}) %>%
  compact() %>%
  bind_rows()

n_authors <- n_distinct(all_authors$author)
cat(sprintf("Loaded %d authors\n", n_authors))

# ── Build author x ngram matrix, filtered to ≥25% of authors ─────────────────
cat("Building feature matrix...\n")

keep_ngrams <- all_authors %>%
  group_by(ngram, n) %>%
  summarise(author_count = n_distinct(author), .groups = "drop") %>%
  filter(author_count >= 0.25 * n_authors) %>%
  mutate(feature = paste0("n", n, "_", ngram))

cat(sprintf("Features after 25%% filter: %d\n", nrow(keep_ngrams)))

feature_matrix <- all_authors %>%
  mutate(feature = paste0("n", n, "_", ngram)) %>%
  filter(feature %in% keep_ngrams$feature) %>%
  select(author, feature, rel_freq) %>%
  pivot_wider(names_from = feature, values_from = rel_freq, values_fill = 0)

mat <- feature_matrix %>%
  column_to_rownames("author") %>%
  as.matrix()

write_rds(feature_matrix, file.path(out_dir, "feature_matrix.rds"))
cat("Feature matrix saved\n")

# ── PCA ────────────────────────────────────────────────────────────────────────
cat("Running PCA...\n")

pca <- prcomp(mat, scale. = TRUE, center = TRUE)

pca_df <- as_tibble(pca$x[, 1:10], rownames = "author")

write_tsv(pca_df, file.path(out_dir, "pca_scores.tsv"))

var_explained <- summary(pca)$importance[2, 1:10]
cat("Variance explained by PC1-10:\n")
print(round(var_explained, 3))

pca_plot <- ggplot(pca_df, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.4, size = 1) +
  labs(
    title = "PCA of author POS n-gram profiles",
    x     = sprintf("PC1 (%.1f%%)", var_explained[1] * 100),
    y     = sprintf("PC2 (%.1f%%)", var_explained[2] * 100)
  ) +
  theme_minimal()

ggsave(file.path(out_dir, "pca_plot.png"), pca_plot, width = 8, height = 6, dpi = 150)

# ── Pairwise distances ─────────────────────────────────────────────────────────
cat("Computing pairwise distances...\n")

cosine_sim <- function(m) {
  norm <- sqrt(rowSums(m^2))
  sim  <- (m / norm) %*% t(m / norm)
  1 - sim
}

cos_dist     <- cosine_sim(mat)
mat_scaled   <- scale(mat)
delta_dist   <- as.matrix(dist(mat_scaled, method = "manhattan")) / ncol(mat)
mat_bin      <- (mat > 0) * 1
jaccard_dist <- as.matrix(dist(mat_bin, method = "binary"))

write_rds(cos_dist,     file.path(out_dir, "cosine_dist.rds"))
write_rds(delta_dist,   file.path(out_dir, "burrows_delta.rds"))
write_rds(jaccard_dist, file.path(out_dir, "jaccard_dist.rds"))

# ── Summary statistics of distances ───────────────────────────────────────────
summarise_dist <- function(d, name) {
  vals <- d[upper.tri(d)]
  tibble(
    metric = name,
    mean   = mean(vals),
    median = median(vals),
    sd     = sd(vals),
    min    = min(vals),
    max    = max(vals)
  )
}

dist_summary <- bind_rows(
  summarise_dist(cos_dist,     "cosine"),
  summarise_dist(delta_dist,   "burrows_delta"),
  summarise_dist(jaccard_dist, "jaccard")
)

write_tsv(dist_summary, file.path(out_dir, "distance_summary.tsv"))
print(dist_summary)

# ── Heatmap (sample of 100 authors for readability) ───────────────────────────
cat("Generating heatmap...\n")

set.seed(42)
sample_authors <- sample(rownames(cos_dist), min(100, nrow(cos_dist)))
cos_sample     <- cos_dist[sample_authors, sample_authors]

heatmap_df <- as_tibble(cos_sample, rownames = "author1") %>%
  pivot_longer(-author1, names_to = "author2", values_to = "distance")

heatmap_plot <- ggplot(heatmap_df, aes(x = author1, y = author2, fill = distance)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Pairwise cosine distance (sample of 100 authors)", fill = "Distance") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

ggsave(file.path(out_dir, "heatmap_cosine.png"), heatmap_plot, width = 8, height = 7, dpi = 150)

cat("All done.\n")