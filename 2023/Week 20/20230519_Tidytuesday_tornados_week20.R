# TidyTuesday Challenge 
# The data this week comes from NOAA's National Weather Service Storm Prediction Center Severe Weather Maps, Graphics, and Data Page

# Load libraries 
library(tidyverse)

# Read data 
tornados <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-16/tornados.csv')

# Set a custom color palette
custom_palette <- c("#E3120B", "#475ED1")

# Define plot text 
title <- "Increase in Number of Tornados Since the 1950s"
subtitle <- "Tornado count by year, highlighting an upward trend"
caption <- "R.A. Jacobsen | @AulieRoy | Data: NOAA's National Weather Service Storm Prediction Center"

# Create a smoother line by interpolating data
smoothed_data <- tornados %>%
  group_by(year = year(date)) %>%
  summarise(count = n()) %>%
  tidyr::complete(year = full_seq(year, 1)) %>%
  na.omit() %>%
  mutate(count_smoothed = smooth.spline(count)$y)

# Plot 
ggplot(smoothed_data, aes(x = year, y = count)) +
  geom_line(color = custom_palette[1], size = 1.5, alpha = 0.8) +
  geom_point(color = custom_palette[1], size = 3) +
  geom_line(aes(y = count_smoothed), color = custom_palette[2], size = 1, linetype = "dashed") +
  scale_y_continuous(sec.axis = dup_axis(), breaks = seq(0, 2000, by = 200), limits = c(0, 2000)) +
  scale_x_continuous(breaks = seq(min(smoothed_data$year), max(smoothed_data$year), by = 10)) +
  labs(
    x = "", 
    y = "", 
    title = title, 
    subtitle = subtitle, 
    caption = caption
  ) + 
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
  ) +
  scale_color_manual(values = custom_palette[1]) +
  scale_fill_manual(values = custom_palette[1]) +
  guides(color = FALSE, fill = FALSE)
