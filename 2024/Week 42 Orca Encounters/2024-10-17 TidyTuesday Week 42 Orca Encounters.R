
# TidyTuesday Challenge Week 42.  
# The data this week comes from the Center for Whale Research (CWR). 
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2024/2024-10-15/readme.md
# Code developed by R.A.Jacobsen

# Load libraries 
library(tidyverse)
library(maps)
library(showtext)
library(grid)

# Load dataset 
orcas <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-15/orcas.csv')

# Wrangle data 
orcas_df <- 
  orcas %>%
  filter(!is.na(begin_latitude) & !is.na(begin_longitude))

world_map <- map_data("world")

# plot the map 

font_add_google("Lato", "lato")
showtext_auto()

map <- 
  ggplot() +
  geom_rect(aes(xmin = -130, xmax = -120, ymin = 46, ymax = 50), fill = "#b0e0e6") +  
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "#66c266", color = "white") + 
  geom_point(data = orcas_df, aes(x = begin_longitude, y = begin_latitude), color = "red", size = 2, alpha = 0.8) +  
  coord_quickmap(xlim = c(-128, -122), ylim = c(47, 49)) +  
  labs(title = "Orca Encounters",
       subtitle = "The map highlights where orcas have been seen in the Salish Sea",
       caption = "R.A.Jacobsen | @AulieRoy | Data: Center for Whale Research",
       x = NULL,
       y = NULL) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(family = "lato", face = "bold", size = 18, hjust = 0), 
    plot.subtitle = element_text(family = "lato", size = 14, hjust = 0, margin = margin(b = 10)), 
    plot.caption = element_text(family = "lato", size = 10, hjust = 0, margin = margin(t = 10)),  
    axis.text = element_blank(),  
    axis.title = element_blank(), 
    axis.ticks = element_blank()  
  )

bar_plot <- 
  orcas_df %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarise(encounters = n()) %>%
  ggplot(aes(x = as.factor(year), y = encounters)) +
  geom_col(fill = "red") +  
  labs(
    title = "Ecounters by year",
  ) + 
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(family = "lato", face = "bold", size = 10),
    axis.title = element_blank(),  
    axis.text = element_text(size = 8)  
  ) + 
  coord_flip()

bar_grob <- ggplotGrob(bar_plot)

final_plot <- 
  map + 
  annotation_custom(grob = bar_grob, xmin = -128.2, xmax = -125, ymin = 47, ymax = 48.6)  

final_plot