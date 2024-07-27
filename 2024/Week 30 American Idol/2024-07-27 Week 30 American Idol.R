
# Code shows contribution to the the #TidyTuesday challenge 
# Week 30 - American Idol data 
# Source - American Idol Data - https://github.com/kkakey/American_Idol

# Load libraries 
library(tidyverse)
library(ggthemes)

# Load data
eliminations <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-23/eliminations.csv')

# Data preparation
gender_dist <- eliminations %>%
  filter(!is.na(gender)) %>%  
  group_by(season, gender) %>%
  summarise(count = n(), .groups = 'drop')

# Plot
ggplot(gender_dist, aes(x = factor(season), y = count, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Gender Distribution of Contestants by Season",
    subtitle = "Analysis of gender distribution across various seasons",
    x = "Season",
    y = "Number of Contestants",
    fill = "Gender"
  ) +
  scale_fill_brewer(palette = "Set2") +  
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.3, size = 4)  
