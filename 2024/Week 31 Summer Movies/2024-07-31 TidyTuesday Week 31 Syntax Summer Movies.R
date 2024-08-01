
# Code for the #TidyTuesday Week 31 challenge. 
# Data comes from the Internet Movie Database. 
# This code were created by Roy Aulie Jacobsen as part of the #TidyTuesday challenge. 
# The code uses the tidyverse and tidytext library to explore data about summer movies. 

# Load libraries 
library(tidyverse)
library(tidytext)
library(patchwork)

# Load data 
summer_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-30/summer_movies.csv')

# Wrangle

romantic_summer_movies <- 
  summer_movies %>% 
  unnest_tokens(genre, genres) %>% 
  filter(genre == "romance")

title_summer_movies <- 
  romantic_summer_movies %>% 
  unnest_tokens(word, simple_title) %>% 
  filter(nchar(word) >= 4) %>% 
  anti_join(stop_words, by = "word") 

word_counts <- 
  title_summer_movies %>%
  count(word, sort = TRUE) %>% 
  slice_head(n = 15)

word_counts <- 
  word_counts %>%
  mutate(top5 = ifelse(row_number() <= 4, "Top 4", "Other"))

# Plot 1: Words in titles 
plot1 <- 
  ggplot(word_counts, aes(x = reorder(word, n), y = n, fill = top5)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.2, size = 3) +
  labs(
    title = "Frequency of Words in Romantic Summer Movie Titles",
    subtitle = "Summer, nights, dream, and midsummer are the three most common words",
    x = "Most common words", 
    y = " ", 
    fill = " "
    ) +
  scale_fill_manual(values = c("Top 4" = "#E3120B", "Other" = "#b4cede")) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), 
    axis.ticks.x = element_blank(), 
    axis.line.x = element_blank(), 
    axis.text.x = element_blank(), 
    legend.position = "none", 
    plot.title = element_text(size = 18, face = "bold"), 
    plot.subtitle = element_text(size = 14, face = "italic")
  )

# Plot 2: Genre distribution 
genre_distribution <- 
  summer_movies %>%
  separate_rows(genres, sep = ",") %>%
  filter(!is.na(genres), !is.na(year)) %>%
  count(year, genres)

plot2 <- 
  ggplot(genre_distribution, aes(x = year, y = n, fill = genres)) +
  geom_area() +
  labs(title = "Genre Distribution Over Time",
       x = " ",
       y = "Movie count",
       fill = "Genre", 
       caption = "R.A. Jacobsen | @AulieRoy | Source: IMDB"
  ) +
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(),
    plot.caption = element_text(size = 8, hjust = 0),
    plot.title = element_text(size = 18, face = "bold"), 
    plot.subtitle = element_text(size = 14, face = "italic")
  )

combined_plot <- plot1 / plot2
combined_plot

