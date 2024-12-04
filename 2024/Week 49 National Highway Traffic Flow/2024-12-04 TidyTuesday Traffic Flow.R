# TidyTuesday Challenge Week 49.  
# The data this week exploring National Highways Traffic Flow data! National Highways operates and maintains motorways and major A roads in England. 
# https://github.com/rfordatascience/tidytuesday/blob/main/data/2024/2024-12-03/readme.md
# Code developed by R.A.Jacobsen

# Load libraries
library(tidyverse)
library(janitor)
library(scales)

# Load data
A64_traffic <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-03/A64_traffic.csv') %>% 
  janitor::clean_names()

# Wrangle data
daily_data <- 
  A64_traffic %>%
  mutate(report_date = as.Date(report_date)) %>%
  group_by(report_date) %>%
  summarize(daily_volume = sum(total_volume, na.rm = TRUE))

# Plot
ggplot(daily_data, aes(x = report_date, y = daily_volume)) +
  geom_line(color = "darkred", size = 1.5) +  
  labs(
    title = "Traffic Volume on A64",
    subtitle = "Traffic volume throughout May, with significant fluctuations reflecting varying daily patterns. Data provides insight into peak and low traffic days.",
    x = NULL, 
    y = NULL,
    caption = "R.A.Jacobsen | @AulieRoy | Data: National Highways Traffic Data"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(family = "serif", face = "bold", hjust = 0, size = 18),
    plot.subtitle = element_text(family = "serif", size = 14, hjust = 0),
    plot.caption = element_text(family = "serif", hjust = 0, size = 10, face = "italic"),
    axis.title.x = element_text(family = "serif", size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(family = "serif", size = 14, margin = margin(r = 10)),
    axis.text.x = element_text(family = "serif", size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(family = "serif", size = 12, color = "black"),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(color = "gray40", size = 0.8),
    axis.ticks.x = element_line(color = "gray40", size = 0.8),
    axis.line.y.right = element_line(color = "gray40")
  ) +
  scale_x_date(date_labels = "%b %d", breaks = "5 days") + 
  scale_y_continuous(labels = scales::comma, position = "right")
