library(tidyverse)
library(dplyr)
library(tidyr)


df <- read.csv(file = "./metadata_merged.csv", header = TRUE, fileEncoding = "latin1")


# --- 1. Base stats per author ---
author_base <- df %>%
  group_by(author_username) %>%
  summarise(
    text_count   = n(),
    total_words  = sum(wordcount, na.rm = TRUE),
    .groups = "drop"
  )

# --- 2. Texts per genre (wide) ---
author_genre <- df %>%
  group_by(author_username, genre) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(
    names_from  = genre,
    values_from = n,
    values_fill = 0,
    names_prefix = "genre_"
  )

# --- 3. Texts per fandom (wide) ---
author_fandom <- df %>%
  group_by(author_username, fandom_code) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(
    names_from  = fandom_code,
    values_from = n,
    values_fill = 0,
    names_prefix = "fandom_"
  )

# --- 4. Join everything together ---
author_table <- author_base %>%
  left_join(author_genre, by = "author_username") %>%
  left_join(author_fandom, by = "author_username")

# --- 5. Distribution: how many authors have 1 text, 2 texts, etc. ---
text_count_dist <- author_base %>%
  count(text_count, name = "author_count") %>%
  arrange(text_count)

# --- View results ---
print(author_table)
print(text_count_dist)

# --- Optional: save to CSV ---
write.csv(author_table,      "author_summary.csv",       row.names = FALSE)
write.csv(text_count_dist,   "text_count_distribution.csv", row.names = FALSE)






author_base <- df %>%
  group_by(author_username) %>%
  summarise(
    text_count        = n(),
    total_words       = sum(wordcount, na.rm = TRUE),
    fandom_count      = n_distinct(fandom_categories),
    .groups = "drop"
  )

write.csv(author_base,      "author_base_summary.csv",       row.names = FALSE)





# VIS
library(ggplot2)
options(scipen = 999)

# 1. Author text count histogram
text_count_bis <- ggplot(text_count_dist %>% filter(text_count <= 50), aes(x = text_count, y = author_count)) +
  geom_col(fill = "#185FA5", width = 0.7) +
  scale_x_continuous(breaks = seq(0, 50, by = 5)) +
  labs(x = "Number of texts", y = "Authors") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

ggsave("text_vis.png", width = 14, height = 10, dpi = 150, bg = "white")

author_vis <- author_base %>%
  slice_max(total_words, n = 30) %>%
  mutate(author_username = reorder(author_username, total_words)) %>%
  ggplot(aes(x = total_words, y = author_username)) +
  geom_col(fill = "#0F6E56", width = 0.7) +
  scale_x_continuous(labels = scales::label_number(suffix = "k", scale = 1e-3)) +
  labs(x = "Total words", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave("author_vis.png", width = 10, height = 14, dpi = 150, bg = "white")


# combined Romance/Romanze and Liebesgeschichte
# condensed haykuu duplicate
genre_fandom <- df %>%
  filter(author_username %in% (author_base %>% filter(text_count > 1) %>% pull(author_username))) %>%
  separate_rows(genre, sep = ",\\s*") %>%
  mutate(
    genre = trimws(genre),
    genre = case_when(
      genre %in% c("Romanze", "Romance") ~ "Liebesgeschichte",
      TRUE ~ genre
    ),
    fandom_categories = case_when(
      fandom_categories == "Haikyuu!! / FF" ~ "Haikyuu!!",
      fandom_categories == "My Hero Academia / FF" ~ "My Hero Academia",
      fandom_categories == "J.R.R. Tolkien / Mittelerde / Der Hobbit" ~ "J.R.R. Tolkien / Mittelerde / Der Herr der Ringe",
      TRUE ~ fandom_categories
    )
  ) %>%
  group_by(genre, fandom_categories) %>%
  summarise(text_count = n(), .groups = "drop")



top_genres <- genre_fandom %>% 
  count(genre, wt = text_count, sort = TRUE) %>% 
  slice_head(n = 10) %>% 
  pull(genre)

top_fandoms <- genre_fandom %>% 
  count(fandom_categories, wt = text_count, sort = TRUE) %>% 
  slice_head(n = 15) %>% 
  pull(fandom_categories)

genre_fandom %>%
  filter(genre %in% top_genres, fandom_categories %in% top_fandoms) %>%
  mutate(genre = factor(genre, levels = rev(top_genres)),
         fandom_categories = factor(fandom_categories, levels = top_fandoms)) %>%
  ggplot(aes(x = fandom_categories, y = genre, size = text_count, colour = text_count)) +
  geom_point(alpha = 0.7) +
  scale_size_area(max_size = 20) +
  scale_colour_gradient(low = "#AFA9EC", high = "#3C3489") +
  labs(x = NULL, y = NULL) +
  guides(size = "none", colour = "none") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

ggsave("genre_fandom.png", width = 14, height = 8, dpi = 150, bg = "white")



genre_fandom %>%
  filter(genre == "Liebesgeschichte") %>%
  filter(fandom_categories %in% c(
    "Harry Potter / Harry Potter - FFs",
    "Marvel / Marvel Cinematic Universe / The Avengers"
  ))

genre_fandom %>%
  arrange(desc(text_count)) %>%
  write.csv("genre_fandom_combinations.csv", row.names = FALSE)






author_base %>% filter(text_count > 1) %>%
  ggplot(aes(x = text_count, y = total_words, colour = fandom_count)) +
  geom_point(alpha = 0.5, size = 1.5) +
  scale_colour_gradient(low = "#5DCAA5", high = "#3C3489", name = "Fandoms") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Number of texts", y = "Total words") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

ggsave("fandom_text_corr.png", width = 10, height = 8, dpi = 150, bg = "white")



author_base %>%
  filter(fandom_count <= 50, text_count > 1) %>%
  ggplot(aes(x = text_count, y = total_words, colour = fandom_count)) +
  geom_point(alpha = 0.5, size = 1.5) +
  scale_colour_gradient(low = "#5DCAA5", high = "#3C3489", name = "Fandoms") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Number of texts", y = "Total words") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

ggsave("fandom_text_corr2.png", width = 10, height = 8, dpi = 150, bg = "white")