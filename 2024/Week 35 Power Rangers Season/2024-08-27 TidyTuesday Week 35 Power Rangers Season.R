# TidyTuesday Week 35
# This weeks #TidyTuesday explore Power Rangers Seasons. 
# Data comes from Kaggle's Power Rangers Dataset.
# Code developed by Roy Aulie Jacobsen

# Load libraries 
library(tidyverse)

# Load dataset 
power_rangers_seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-27/power_rangers_seasons.csv')

# Rating per season
season_summary <- power_rangers_seasons %>%
  group_by(season_num) %>%
  summarise(avg_imdb_rating = mean(imdb_rating, na.rm = TRUE)) %>%
  arrange(season_num)

# Plot
ggplot(season_summary, aes(x = season_num, y = avg_imdb_rating)) +
  geom_area(fill = "darkgrey", alpha = 0.7) + 
  geom_line(color = "grey30", size = 1) +  
  geom_point(color = "grey30", size = 3) + 
  geom_text(aes(label = round(avg_imdb_rating, 1)), vjust = -0.5, color = "black", size = 3.5) + 
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, by = 1), expand = c(0, 0)) +  
  scale_x_continuous(breaks = seq(min(season_summary$season_num), max(season_summary$season_num), by = 1), expand = c(0, 0)) + 
  theme_minimal(base_size = 12) +
  labs(
    title = "Average IMDb Rating per Power Rangers Season",
    subtitle = "The Seventh Season Received the Highest Rating on IMDb", 
    x = "Season Number",
    y = "Average IMDb Rating",
    caption = "R.A. Jacobsen | @AulieRoy | Data Source: Power Rangers Seasons Dataset"
  ) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"), 
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line.y = element_line(),       
    axis.line.x = element_line(),        
    axis.ticks = element_line(),         
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 14, hjust = 0.5),  
    plot.caption = element_text(hjust = 0, size = 10)  
  )

