lib_path <- "./r_libs"
.libPaths(c(lib_path, .libPaths()))

library(tidyverse)
library(fs)

base   <- "./FANFIC_CORPUS"
tagged <- file.path(base, "train_15k_tagged")

# ── Common Harry Potter spell words to search for ────────────────────────────
spells <- c("Expelliarmus", "Avada", "Kedavra", "Expecto", "Patronum",
            "Lumos", "Nox", "Wingardium", "Leviosa", "Accio",
            "Crucio", "Imperio", "Stupor", "Petrificus", "Totalus",
            "Alohomora", "Riddikulus", "Obliviate", "Protego", "Sectumsempra")

author_dirs <- list.dirs(tagged, full.names = TRUE, recursive = FALSE)

# Sample a handful of authors/files rather than scanning everything
set.seed(1)
sample_dirs <- sample(author_dirs, 20)

results <- map_dfr(sample_dirs, function(d) {
  files <- list.files(d, full.names = TRUE)
  map_dfr(files, function(f) {
    df <- read_tsv(f, col_types = cols(.default = "c"))
    df %>% filter(token %in% spells | lemma %in% spells)
  })
})

cat("Rows found:", nrow(results), "\n")
print(results %>% select(token, lemma, upos, xpos, dep_rel))

cat("\nUPOS distribution for spell words found:\n")
print(table(results$upos))

write_tsv(results, file.path(base, "spell_tagging_check.tsv"))
cat("Saved to:", file.path(base, "spell_tagging_check.tsv"), "\n")

