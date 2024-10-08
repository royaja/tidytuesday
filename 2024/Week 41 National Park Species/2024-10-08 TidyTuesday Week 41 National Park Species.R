
# TidyTuesday Week 41 - National Park Species
# This weeks data explores species at the most visited National Parks in the USA
# Code developed by R.A.Jacobsen 

# Load libraries  
library(tidyverse)
library(janitor)
library(showtext)
library(scales)


# Load National Park Species data 
most_visited_nps_species_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-08/most_visited_nps_species_data.csv')

most_visited_nps_species_data <- 
  most_visited_nps_species_data %>% 
  clean_names()

# Richness of species 
species_richness <- 
  most_visited_nps_species_data %>% 
  group_by(park_name) %>% 
  summarise(specie_richness = n_distinct(common_names)) %>%
  arrange(desc(specie_richness)) %>%
  mutate(log_species_richness = log10(specie_richness + 1)) 

# Average species richness
avg_richness <- mean(species_richness$log_species_richness)

# Ploting 

font_add_google("Lato", "lato")
showtext_auto()


species_richness %>%
  ggplot(aes(x = reorder(park_name, log_species_richness), y = log_species_richness, fill = log_species_richness)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  geom_text(aes(label = comma(10^log_species_richness - 1)), hjust = -0.5, color = "black", size = 4.5) +  
  geom_hline(yintercept = avg_richness, linetype = "dashed", color = "red", size = 1) +
  scale_fill_gradient(low = "#f7fbff", high = "#E3120B") +
  labs(
    title = "Specie Richness in National Parks",
    subtitle = "Great Smoky Mountains National Park has the highest species richness, far exceeding the average of all parks.\nMost other parks show relatively similar levels of species richness, with a few slightly below the average.",
    x = " ",
    y = "Log-transformed Richness in Species"
    ) +
  coord_flip() +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.y = element_text(size = 14, margin = margin(r = 5)),  
    axis.text.x = element_text(size = 12, margin = margin(t = 5)),  
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),  
    plot.title = element_text(face = "bold", size = 20),  
    plot.subtitle = element_text( size = 16),
    axis.line.x = element_line(color = "black"), 
    axis.line.y = element_line(color = "black"), 
    axis.ticks.x = element_line(color = "black"), 
    axis.ticks.y = element_line(color = "black"), 
    panel.grid.major.x = element_line(color = "gray", linetype = "dashed"),  
    panel.grid.major.y = element_blank(), 
    plot.margin = margin(1, 1, 1, 1, "cm")  
  )
