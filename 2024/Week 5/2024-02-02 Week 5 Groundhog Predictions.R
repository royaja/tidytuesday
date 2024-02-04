
# Contribution to TidyTuesday Week 5 on Twitter (Now X)
# Data: Groundhog Day Predictions from groundhog-day.com

# Load libraries
library(tidyverse)
library(maps)

# Load dataset
groundhogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-30/groundhogs.csv')

# Data wrangling
groundhogs_us <- 
  groundhogs |> 
  filter(country == "USA")

groundhog_us_cities <- 
  groundhogs_us |> 
  filter(predictions_count > 50)

us_map <- map_data("state")

# Plot 
ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "#f0f0f0", color = "#d4d4d4", size = 0.5) +
  geom_point(data = groundhogs_us, aes(x = longitude, y = latitude, size = predictions_count), 
             alpha = 0.8, color = "#0C0040") +
  geom_text(data = groundhog_us_cities, aes(x = longitude, y = latitude, label = city), size = 3, vjust = 1.5, hjust = 1, color = "#0C0040") +
  labs(title = "Groundhog Day Prediction",
       subtitle = "Highest predictions in Punxsutawney, Quarryville, and Sun Prairie",
       x = " ", 
       y = " ",
       size = "Predictions Count", 
       caption = "R.A. Jacobsen | @AulieRoy | Data: Groundhog Day Predictions from groundhog-day.com") +
  scale_color_manual(values = c("#0C0040")) + 
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(color = "#d4d4d4", size = 0.2),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#f0f0f0"),
    plot.background = element_rect(fill = "#f0f0f0"),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "cm"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12), 
    plot.caption = element_text(size = 8, hjust = 0), 
  )
