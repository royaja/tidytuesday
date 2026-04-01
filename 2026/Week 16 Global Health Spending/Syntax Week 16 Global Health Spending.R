
# TidyTuesday week 16
# Global Health Spending
# I kept this one pretty simple
# just looking at countries with the highest preventive care share
# and comparing that with curative care
# Code by R.A. Jacobsen

library(tidyverse)
library(showtext)
library(scales)

# font
sysfonts::font_add_google("Lato", "lato")
showtext_auto()

# load data
spending_purpose <- 
  readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-21/spending_purpose.csv"
)

latest_year <- max(spending_purpose$year, na.rm = TRUE)

cols <- 
  c(
  text = "#2F3A3D",
  grid = "#E8ECEA",
  line = "#D9E1DE",
  preventive = "#9CB7B0",
  curative = "#7F8886",
  bg = "white"
)

# Wrangle 
plot_df <- 
  spending_purpose %>%
  filter(
    year == latest_year,
    unit == "% of current health expenditure",
    spending_purpose %in% c("Curative care", "Preventive care")
  ) %>%
  select(country_name, spending_purpose, value) %>%
  pivot_wider(
    names_from = spending_purpose,
    values_from = value
  ) %>%
  rename(
    curative = `Curative care`,
    preventive = `Preventive care`
  ) %>%
  filter(!is.na(curative), !is.na(preventive)) %>%
  slice_max(preventive, n = 10, with_ties = FALSE) %>%
  arrange(preventive) %>%
  mutate(
    country_name = factor(country_name, levels = country_name),
    preventive_lab = paste0(round(preventive, 1), "%"),
    curative_lab = paste0(round(curative, 1), "%")
  )

points_df <- 
  plot_df %>%
  select(country_name, preventive, curative) %>%
  pivot_longer(
    cols = c(preventive, curative),
    names_to = "type",
    values_to = "value"
  ) %>%
  mutate(
    type = recode(
      type,
      preventive = "Preventive care",
      curative = "Curative care"
    )
  )

x_min <- max(0, min(plot_df$preventive) - 5)
x_max <- max(plot_df$curative) + 5

# plot 
ggplot(plot_df, aes(y = country_name)) +
  geom_segment(
    aes(x = preventive, xend = curative, yend = country_name),
    linewidth = 1.3,
    color = cols["line"]
  ) +
  geom_point(
    data = points_df,
    aes(x = value, color = type),
    size = 3.6,
    show.legend = TRUE
  ) +
  geom_text(
    aes(x = preventive, label = preventive_lab),
    hjust = 1.15,
    size = 3.5,
    family = "lato",
    color = cols["preventive"]
  ) +
  geom_text(
    aes(x = curative, label = curative_lab),
    hjust = -0.15,
    size = 3.5,
    family = "lato",
    color = cols["curative"]
  ) +
  scale_color_manual(
    values = c(
      "Preventive care" = cols["preventive"],
      "Curative care" = cols["curative"]
    )
  ) +
  scale_x_continuous(
    limits = c(x_min, x_max),
    labels = label_number(suffix = "%")
  ) +
  labs(
    title = "Preventive care still takes a much smaller share",
    subtitle = paste0(
      "Top 10 countries with the highest preventive care share in ", latest_year
    ),
    x = "Share of current health expenditure",
    y = NULL,
    color = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "lato") +
  theme(
    plot.background = element_rect(fill = cols["bg"], color = NA),
    panel.background = element_rect(fill = cols["bg"], color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = cols["grid"], linewidth = 0.4),
    axis.text.y = element_text(size = 10.5, color = cols["text"]),
    axis.text.x = element_text(size = 10, color = cols["text"]),
    axis.title.x = element_text(size = 11, color = cols["text"]),
    plot.title = element_text(size = 18, face = "bold", color = cols["text"]),
    plot.subtitle = element_text(
      size = 11,
      color = cols["text"],
      margin = margin(b = 12)
    ),
    legend.position = "top",
    legend.text = element_text(size = 10, color = cols["text"]),
    plot.margin = margin(20, 35, 20, 35)
  )


