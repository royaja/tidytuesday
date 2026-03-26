
# TidyTuesday challenge week 13 - Coastal Ocean Temperature by Depth
# This week's data follows daily ocean temperatures over more than seven years
# at seven different depths from a coastal site in Nova Scotia, Canada.
# Here I use monthly averages to show how temperatures rise and fall throughout the year
# and how that pattern changes with depth.
# Code by R.A. Jacobsen

# load library 
library(tidyverse)
library(lubridate)
library(scales)
library(showtext)
library(sysfonts)

# Font
font_add_google("Lato", "lato")
showtext_auto()

# Load data 
ocean_temperature <-
  read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-31/ocean_temperature.csv",
    show_col_types = FALSE
  )


# Wrangle 
plot_data <-
  ocean_temperature %>%
  mutate(
    date = as.Date(date),
    month = floor_date(date, "month")
  ) %>%
  group_by(month, sensor_depth_at_low_tide_m) %>%
  summarise(
    temp = mean(mean_temperature_degree_c, na.rm = TRUE),
    days_with_data = sum(!is.na(mean_temperature_degree_c)),
    .groups = "drop"
  ) %>%
  filter(days_with_data >= 15) %>%
  mutate(
    depth = factor(
      sensor_depth_at_low_tide_m,
      levels = c(2, 5, 10, 15, 20, 30, 40),
      labels = c("2 m", "5 m", "10 m", "15 m", "20 m", "30 m", "40 m")
    )
  )


# Plot 
ggplot(plot_data, aes(month, temp, group = depth)) +
  geom_line(
    colour = "#355C73",
    linewidth = 0.9,
    lineend = "round"
  ) +
  geom_point(
    colour = "#355C73",
    size = 0.8,
    alpha = 0.8
  ) +
  facet_grid(depth ~ .) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = c(0, 10, 20),
    labels = \(x) paste0(x, "\u00B0C")
  ) +
  labs(
    title = "Ocean temperatures rise and fall throughout the year",
    subtitle = "Monthly averages from seven sensor depths in coastal Nova Scotia, 2018 to 2025",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14, base_family = "lato") +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = "#D9DEDF",
      linewidth = 0.35
    ),
    
    strip.background = element_blank(),
    strip.text.y = element_text(
      face = "bold",
      colour = "#173042",
      size = 13
    ),
    
    axis.text.x = element_text(
      colour = "#223B4A",
      size = 12
    ),
    axis.text.y = element_text(
      colour = "#6B7C85",
      size = 10
    ),
    
    plot.title = element_text(
      face = "bold",
      colour = "#173042",
      size = 20,
      hjust = 0,
      margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      colour = "#4F616B",
      size = 12,
      hjust = 0,
      margin = margin(b = 14)
    ),
    
    plot.title.position = "plot",
    panel.spacing.y = unit(0.45, "lines"),
    plot.margin = margin(t = 20, r = 15, b = 10, l = 15)
  )

