

# TidyTuesday Week 26
# This week's data explores shipwrecks recorded in Irish waters.
# The Wreck Inventory of Ireland (WIID) contains information on vessels lost
# around the Irish coast, including coordinates, names, dates, and historical
# details where available.
# This visualization maps wrecks with known coordinates and year, grouping them
# by historical era and highlighting selected wrecks of particular historical note.
# Code developed by R. A. Jacobsen

# Load library 
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)

# load data 
wreck_inventory <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-30/wreck_inventory.csv"
)

ireland <- ne_countries(
  country     = c("Ireland", "United Kingdom"),
  scale       = "medium",
  returnclass = "sf"
)

era_colors <- c(
  "Before 1600" = "#8B1A1A",
  "1600???1799"   = "#A0522D",
  "1800???1899"   = "#2E5D34",
  "1900???1918"   = "#1B3A6B",
  "1919???1946"   = "#4A3060"
)

# Wrangle 
wrecks_mapped <- wreck_inventory |>
  filter(
    !is.na(latitude),
    !is.na(longitude),
    !is.na(year),
    between(latitude,  50.5, 56),
    between(longitude, -11,  -4.5)
  ) |>
  mutate(
    era = case_when(
      year < 1600               ~ "Before 1600",
      between(year, 1600, 1799) ~ "1600???1799",
      between(year, 1800, 1899) ~ "1800???1899",
      between(year, 1900, 1918) ~ "1900???1918",
      year > 1918               ~ "1919???1946"
    ),
    era = factor(era, levels = names(era_colors))
  )

notable_wrecks <- wreck_inventory |>
  filter(
    !is.na(latitude),
    !is.na(longitude),
    str_detect(
      wreck_name,
      regex("lusitania|leinster|laurentic|girona|trinidad", ignore_case = TRUE)
    ),
    between(latitude,  50.5, 56),
    between(longitude, -11,  -4.5)
  ) |>
  mutate(label = paste0(wreck_name, "\n", year))

# Plot 
wrecks_mapped |>
  ggplot() +
  geom_sf(
    data      = ireland,
    fill      = "#D4C4A0",
    color     = "#8B7355",
    linewidth = 0.3
  ) +
  geom_point(
    aes(x = longitude, y = latitude, color = era),
    alpha = 0.55,
    size  = 0.7,
    na.rm = TRUE
  ) +
  geom_point(
    data  = notable_wrecks,
    aes(x = longitude, y = latitude),
    color = "#3C1A0A",
    alpha = 0.18,
    size  = 7,
    na.rm = TRUE
  ) +
  geom_point(
    data   = notable_wrecks,
    aes(x  = longitude, y = latitude),
    color  = "#3C1A0A",
    size   = 2.5,
    shape  = 21,
    fill   = NA,
    stroke = 0.8,
    na.rm  = TRUE
  ) +
  geom_label_repel(
    data               = notable_wrecks,
    aes(x = longitude, y = latitude, label = label),
    family             = "serif",
    fontface           = "italic",
    color              = "#3C1A0A",
    fill               = "#F5E6C4",
    size               = 2.8,
    lineheight         = 1.3,
    box.padding        = 0.6,
    point.padding      = 0.4,
    segment.color      = "#8B7355",
    segment.size       = 0.4,
    label.padding      = unit(0.25, "lines"),
    label.size         = 0.2,
    min.segment.length = 0.2
  ) +
  coord_sf(
    xlim = c(-11, -4.5),
    ylim = c(50.5, 56)
  ) +
  scale_color_manual(
    values = era_colors,
    name   = NULL,
    guide  = guide_legend(
      override.aes = list(size = 3, alpha = 1),
      nrow = 1
    )
  ) +
  labs( title = "Shipwrecks in Irish Waters", subtitle = "The points sketch a coastline shaped by trade,\nempire, war, weather, and ordinary journeys that never made it home. Named wrecks of particular\nhistorical note are marked in circles.", caption = "R. A. Jacobsen | @AulieRoy | Source: Wreck Inventory of Ireland (WIID)" ) +  theme_minimal() +
  theme(
    text             = element_text(family = "serif"),
    plot.background  = element_rect(fill = "#F5E6C4", color = NA),
    panel.background = element_rect(fill = "#9BBFCC", color = NA),
    panel.border     = element_rect(color = "#8B7355", linewidth = 0.8, fill = NA),
    panel.grid.major = element_line(color = "#C5B090", linewidth = 0.3, linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(color = "#8B7355", size = 7),
    axis.ticks       = element_line(color = "#8B7355"),
    plot.title = element_text(
      color  = "#3C2415",
      size   = 20,
      hjust  = 0.5,
      face   = "bold",
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      color      = "#5C4433",
      size       = 9,
      hjust      = 0.5,
      lineheight = 1.5,
      margin     = margin(b = 12)
    ),
    plot.caption = element_text(
      color  = "#7A6A5A",
      size   = 7,
      hjust  = 1,
      margin = margin(t = 8)
    ),
    legend.position = "bottom",
    legend.text     = element_text(color = "#3C2415", size = 8),
    legend.key      = element_rect(fill = "#F5E6C4", color = NA),
    plot.margin     = margin(20, 30, 20, 30)
  )

# save plot 
ggsave("irish_wrecks.png", width = 10, height = 10, dpi = 300, bg = "#F5E6C4")
