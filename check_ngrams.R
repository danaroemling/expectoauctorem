lib_path <- "./r_libs"
.libPaths(c(lib_path, .libPaths()))

library(tidyverse)

base    <- "."
src     <- file.path(base, "train_15k_tagged")

# ── Pick one random author and one random chunk ────────────────────────────────
authors <- list.dirs(src, full.names = FALSE, recursive = FALSE)
author  <- sample(authors, 1)
files   <- list.files(file.path(src, author), full.names = TRUE)
f       <- sample(files, 1)

cat("Author:", author, "\n")
cat("File:  ", basename(f), "\n\n")

df <- read_tsv(f, show_col_types = FALSE)

# ── Basic checks ───────────────────────────────────────────────────────────────
cat("Dimensions:", nrow(df), "tokens x", ncol(df), "columns\n")
cat("Columns:", paste(names(df), collapse = ", "), "\n\n")

cat("Missing values per column:\n")
print(colSums(is.na(df)))

cat("\nUPOS distribution:\n")
print(sort(table(df$upos), decreasing = TRUE))

cat("\nFirst 20 tokens:\n")
print(select(df, token, lemma, upos, xpos, dep_rel) %>% head(20))