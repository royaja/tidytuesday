
# TidyTuesday Challenge Week 51 
# This week`s` data explore magical spells from the recently released Dungeons and Dragons Free Rules (2024 edition).
# https://github.com/rfordatascience/tidytuesday/blob/main/data/2024/2024-12-17/readme.md
# Code developed by R.A.Jacobsen

# Load library 
library(tidyverse)
library(showtext)
library(patchwork)

# Load data
spells <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-17/spells.csv')

# Load fonts 
font_add_google("Roboto", "roboto")
showtext_auto()

# Number of Spells
main_plot <- spells %>%
  count(school, sort = TRUE) %>%
  ggplot(aes(x = reorder(school, n), y = n, fill = school)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(8, "Set3")) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 70), breaks = seq(0, 70, by = 10)) +  labs(
    title = "Popular Schools of Magic in Dungeons & Dragons",
    subtitle = "Transmutation has the highest number of spells, while conjuration ranks higest on the average spell level.",
    caption = "R.A.Jacobsen | @AulieRoy | Data: Dungeons and Dragons", 
    x = "School of Magic", 
    y = "Number of Spells"
  ) +
  theme_minimal(base_family = "roboto") +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 16, hjust = 0),
    plot.caption = element_text(hjust = 0, size = 9),
    axis.text = element_text(size = 12), 
    axis.line.x = element_line(), 
    axis.line.y = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Average Spell Level
inset_plot <- spells %>%
  group_by(school) %>%
  summarise(avg_level = mean(level, na.rm = TRUE)) %>%
  ggplot(aes(x = avg_level, y = reorder(school, avg_level), color = school)) +
  geom_point(size = 3) +
  scale_color_manual(values = RColorBrewer::brewer.pal(8, "Pastel1")) +
  labs(
    title = "Average Spell Level by School",
    x = "Avg. Level", 
    y = NULL
  ) +
  theme_bw(base_family = "roboto") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.position = "none"
  )

# Combine Plots
main_plot + inset_element(inset_plot, left = 0.55, bottom = 0.05, right = 0.95, top = 0.45)

