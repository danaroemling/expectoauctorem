lib_path <- "./r_libs"
.libPaths(c(lib_path, .libPaths()))

library(tidyverse)

out_dir <- "./FANFIC_CORPUS/pos_profiles"

delta     <- read_tsv(file.path(out_dir, "feature_cliffs_delta.tsv"), show_col_types = FALSE)
stability <- read_tsv(file.path(out_dir, "feature_stability.tsv"), show_col_types = FALSE)
pca       <- read_tsv(file.path(out_dir, "pca_scores.tsv"), show_col_types = FALSE)
feat_mat  <- read_rds(file.path(out_dir, "feature_matrix.rds"))