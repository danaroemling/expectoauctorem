lib_path <- "./r_libs"
.libPaths(c(lib_path, .libPaths()))

library(tidyverse)
library(udpipe)
library(fs)

# ── Download German model (only needed once) ───────────────────────────────────
# udpipe_download_model(language = "german")
# udpipe_download_model("german-hdt")
# Model choice: german-gsd-ud-2.5-191206.udpipe is the standard one, but there's also german-hdt which is trained on a larger treebank and tends to be better for informal/web text — likely a better fit for fanfic. 
#model <- udpipe_load_model("german-gsd-ud-2.5-191206.udpipe")
#model <- udpipe_load_model("german-hdt-ud-2.5-191206.udpipe")



# ── Paths ──────────────────────────────────────────────────────────────────────
base       <- "."
model_path <- file.path(base, "german-hdt-ud-2.5-191206.udpipe")

train_src <- file.path(base, "train_15k")
test_src  <- file.path(base, "test_15k")
train_dst <- file.path(base, "train_15k_tagged")
test_dst  <- file.path(base, "test_15k_tagged")

model <- udpipe_load_model(model_path)

# ── SLURM array: task ID determines which author ───────────────────────────────
# Authors are pooled across train and test, each gets a unique index
train_authors <- list.dirs(train_src, full.names = FALSE, recursive = FALSE)
test_authors  <- list.dirs(test_src,  full.names = FALSE, recursive = FALSE)

# Build a flat table of all (author, split) combinations
tasks <- bind_rows(
  tibble(author = train_authors, split = "train"),
  tibble(author = test_authors,  split = "test")
)

task_id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
author  <- tasks$author[task_id]
split   <- tasks$split[task_id]

src_root <- if (split == "train") train_src else test_src
dst_root <- if (split == "train") train_dst else test_dst

cat(sprintf("[Task %d] %s / %s\n", task_id, split, author))

# ── Annotation function ────────────────────────────────────────────────────────
annotate_file <- function(input_path, output_path, model) {
  text <- tryCatch(
    paste(readLines(input_path, warn = FALSE, encoding = "UTF-8"), collapse = " "),
    error = function(e) { warning(sprintf("Could not read: %s", input_path)); return(NULL) }
  )
  if (is.null(text) || nchar(trimws(text)) == 0) return(invisible(NULL))
  
  ann <- udpipe_annotate(model, x = text, doc_id = basename(input_path)) %>%
    as.data.frame() %>%
    select(
      doc_id,
      sentence_id,
      token_id,
      token,
      lemma,
      upos,
      xpos,
      dep_rel,
      head_token_id
    )
  
  write_tsv(ann, output_path)
}

# ── Process all files for this author ─────────────────────────────────────────
src_author <- file.path(src_root, author)
dst_author <- file.path(dst_root, author)
dir_create(dst_author)

files <- list.files(src_author, full.names = FALSE)

for (fname in files) {
  input_path  <- file.path(src_author, fname)
  output_name <- str_replace(fname, "\\.txt$", ".tsv")
  output_path <- file.path(dst_author, output_name)
  
  if (file_exists(output_path)) next
  
  annotate_file(input_path, output_path, model)
}

cat(sprintf("[Task %d] Done: %s / %s\n", task_id, split, author))