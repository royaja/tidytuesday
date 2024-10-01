
# TidyTuesday Week 40. 
# The chess dataset this week comes from Lichess.org via Kaggle/Mitchell J.

# Load library 
library(tidyverse)

# Load the data
chess <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-01/chess.csv')

# Add rating tiers to the dataset
rating_bins <- c(800, 1200, 1400, 1600, 1800, 2000, Inf)
rating_labels <- c("800-1200", "1201-1400", "1401-1600", "1601-1800", "1801-2000", "2000+")

chess <- 
  chess %>%
  mutate(
    white_rating_tier = cut(white_rating, breaks = rating_bins, labels = rating_labels, include.lowest = TRUE)
  )

opening_stats <- 
  chess %>%
  filter(!is.na(opening_name)) %>%
  group_by(opening_name, white_rating_tier) %>%
  summarize(games_played = n(), .groups = 'drop')

top_openings <- 
  opening_stats %>%
  group_by(opening_name) %>%
  summarize(total_games = sum(games_played)) %>%
  top_n(8, total_games) %>%
  pull(opening_name)

opening_stats_filtered <- 
  opening_stats %>%
  filter(opening_name %in% top_openings)

# Plot 
opening_stats_filtered %>% 
  ggplot(aes(x = white_rating_tier, y = games_played, fill = opening_name)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  geom_text(
    aes(label = games_played),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "Top 8 Chess Openings by Rating Tier",
    subtitle = "Number of games played by white rating tier for popular chess openings",
    x = "White Rating Tier",
    y = "Number of Games Played",
    fill = "Opening"
  ) +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, face = "italic"),
    axis.title = element_text(size = 12),
    legend.position = "top"
  )
