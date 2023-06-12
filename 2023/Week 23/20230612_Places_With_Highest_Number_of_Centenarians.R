
# TidyTuesday Challenge Week 23 
# The data this week comes from the Wikipedia List of the verified oldest people via frankiethull on GitHub

# Load packages 
library(tidyverse)

# Load dataset
centenarians <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-30/centenarians.csv')

# Wrangle data 
country_counts <- 
  centenarians %>% 
  group_by(place_of_death_or_residence) %>%
  summarise(count = n()) %>%
  top_n(10, count)

# Text 
title <- "Top 10 Places with the Highest Number of Centenarians"
caption <- "R.A. Jacobsen | @AulieRoy | Data: Wikipedia List of the verified oldest people"

# Plot 
country_counts %>% 
  ggplot(aes(x = reorder(place_of_death_or_residence, desc(count)), y = count)) +
  geom_bar(fill = "#E3120B", stat = "identity") +
  labs(x = "Place", 
       y = "Count", 
       title = title, 
       caption = caption) +
  geom_text(aes(label = count), vjust = -0.5, color = "black") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.line.y.left = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.y = element_blank(), 
    axis.line.x = element_line(), 
    axis.ticks.x = element_line(), 
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "#D9D9D9"), 
    panel.grid.minor.y = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#F5F5F5"),
    plot.background = element_rect(fill = "#F5F5F5"), 
    plot.caption = element_text(size = 10, hjust = 0)
  )

