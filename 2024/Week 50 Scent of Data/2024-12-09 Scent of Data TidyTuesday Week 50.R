
# TidyTuesday Week 50. 
# This weeks data comes from the fascinating world of fragrances with a dataset sourced from Parfumo, 
# a vibrant community of perfume enthusiasts. Olga G. webscraped these data from the various fragrance sections on the Parfumo website. 
# Inspiration from https://www.youtube.com/watch?v=B9FaffIOAVE&t=635s
# Code developed by R.A.Jacobsen

# Load libraries  
library(tidyverse)
library(janitor)
library(ggtext)
library(showtext)

# Add font
font_add_google("Roboto", "roboto")
showtext_auto()

# Load data 
parfumo_data_clean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-10/parfumo_data_clean.csv') %>% 
  clean_names()

# Wrangle Data
parfumo_data_count <- 
  parfumo_data_clean %>% 
  count(release_year, name = "count") %>% 
  filter(as.numeric(release_year) > 2013) %>% 
  mutate(release_year = as.numeric(release_year))

# Plot 
parfumo_data_count %>% 
  ggplot(aes(x = release_year, y = count)) +
  geom_col(width = 0.8, fill = "darkred") +
  geom_text(aes(label = count, y = 300),  
            vjust = 0.5,  
            hjust = 0.5,  
            angle = 90, 
            size = 6, 
            color = "white") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) + 
  coord_cartesian(ylim = c(0, max(parfumo_data_count$count) + 500)) +
  scale_x_continuous(
    breaks = seq(min(parfumo_data_count$release_year), 
                 max(parfumo_data_count$release_year), 
                 by = 1)
  ) +  
  scale_y_continuous(
    expand = c(0, 0),  
    limits = c(0, 2500),  
    breaks = seq(0, 2500, by = 500) 
  ) +  
  labs(
    x = "Year of Release", 
    y = "Number of Perfumes Released",
    title = "Trends in Perfume Releases from 2014 to 2024",
    subtitle = "Perfume releases peaked in 2015 with 2,377 launches, followed by a gradual decline. Releases remained steady between 2016 and 2022, averaging around 1,500 annually. A spike occurred in 2023 with 1,758 perfumes, but 2024 saw a sharp drop to 979.",
    caption = "R.A.Jacobsen | @AulieRoy | Data: Parfumo"
  ) +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(face = "bold", size = 20, family = "roboto", lineheight = 1.2),
    plot.subtitle = element_textbox_simple(size = 14, family = "roboto", 
                                           width = unit(1, "npc"), 
                                           lineheight = 1.2, 
                                           margin = margin(t = 8, b = 8)),
    plot.caption = element_text(hjust = 0, size = 12, margin = margin(t = 10), family = "roboto"),
    axis.text.x = element_text(size = 12, family = "roboto"),
    axis.ticks.x = element_blank(), 
    axis.title.y = element_text(size = 12, face = "bold", family = "roboto"),
    axis.line.y = element_blank(),  
    axis.ticks.y = element_blank(),  
    axis.text.y = element_text(size = 12, face = "bold", color = "black", family = "roboto"),  
    panel.grid.major.y = element_line(color = "gray", linewidth = 0.5)  
  )
