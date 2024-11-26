
# TidyTuesday Week 48 
# This weeks TidyTuesday explore U.S. Customs and Border Protection (CBP) encounter data
# Code created by Roy Aulie Jacobsen

# load libraries 
library(tidyverse)
library(scales)

# Load data 
cbp_resp <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-26/cbp_resp.csv')

# Wrangle data 
demographic <- cbp_resp |>
  group_by(fiscal_year, demographic) |>
  summarise(total_encounters = sum(encounter_count, na.rm = TRUE), .groups = 'drop') |>
  arrange(fiscal_year, demographic)

# Plot 
demographic |>
  ggplot(aes(x = fiscal_year, y = total_encounters, color = demographic)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("#FF5733", "#33FF57", "#3357FF", "#FF33A1", "#A133FF")
  ) +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = "M"),
    breaks = seq(0, max(demographic_trends_df$total_encounters, na.rm = TRUE), by = 500000),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_x_continuous(
    breaks = 2020:2024,
    limits = c(2020, 2024)
  ) +
  labs(
    title = "Evolution of Border Encounters by Demographic Group",
    subtitle = "Showing the total number of encounters divided by demographics",
    caption = "R.A.Jacobsen | @AulieRoy | Data: U.S. Customs and Border Protection (CBP) encounter data",
    x = "Year",
    y = "Number of Encounters",
    color = "Demographic Group"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(size = 14, hjust = 0),
    plot.caption = element_text(size = 8, hjust = 0), 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
