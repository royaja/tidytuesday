
# Participating in the TidyTuesday challenge Week 21 - Sustainable Energy for All
# This weeks data explores Sustainable Energy for all (SE4ALL).
# The data is about energy consumption. Its an initativ that was launched in 
# 2010 by the UN Secretary General. 
# The SE4ALL database provides country level historical data for access to 
# electricity and non-solid fuel; share of renewable energy in total final 
# energy consumption by technology; and energy intensity rate of improvement.
# Code developed by R.A. Jacobsen

# load library 
library(tidyverse)
library(showtext)
library(sysfonts)
library(ggtext)
library(scales)

# fonts 
font_add_google("Lato", "lato")
showtext_auto()

# load data 
energy_cleaned <- 
  readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-26/energy_cleaned.csv",
                  show_col_types = FALSE
  )

bg <- 
  "#F7F3EA"

ink <- 
  "#12343B"

muted <- 
  "#7A6F5D"

grid_col <- 
  "#E2DACB"

base_col <- 
  "#C9BFA8"

country_cols <- 
  c(
    "Denmark" = "#A33A2A",
    "Ireland" = "#2F6B4F",
    "Norway" = "#0E3A53"
  )

target_countries <- 
  c("Denmark", "Ireland", "Norway")

# Wrangle data 
wind_df <- 
  energy_cleaned %>% 
  mutate(
    yr = as.integer(yr),
    country_name = str_squish(country_name)
  ) %>% 
  filter(country_name %in% target_countries) %>% 
  transmute(
    country_name,
    yr,
    wind_share = as.numeric(wind_energy_consumption_tfec_pct)
  ) %>% 
  filter(
    !is.na(yr),
    !is.na(wind_share)
  )

max_year <- 
  max(wind_df$yr, na.rm = TRUE)

end_labels <- 
  wind_df %>% 
  group_by(country_name) %>% 
  slice_max(yr, n = 1, with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(
    label = paste0(
      country_name,
      "\n",
      number(wind_share, accuracy = 0.1),
      "%"
    )
  )

annot <- 
  tribble(
    ~country_name, ~x,     ~y,    ~xend, ~yend, ~label,
    "Denmark",     1991.4, 2.45, 1998, 1.32, "Early wind growth\nbefore 2000",
    "Ireland",     2002.6, 2.05, 2007, 1.28, "Fast build-out from\nthe mid-2000s",
    "Norway",      2000.5, 1.00, 2004, 0.12, "Wind remained a\nsmall share"
  )

p <- 
  ggplot(wind_df, aes(x = yr, y = wind_share, color = country_name)) +
  geom_area(
    data = filter(wind_df, country_name == "Denmark"),
    fill = country_cols["Denmark"],
    color = NA,
    alpha = 0.06
  ) +
  geom_hline(
    yintercept = 0,
    color = base_col,
    linewidth = 0.45
  ) +
  geom_line(
    aes(linewidth = country_name),
    lineend = "round"
  ) +
  geom_point(
    data = end_labels,
    size = 3.2
  ) +
  geom_text(
    data = end_labels,
    aes(label = label),
    hjust = 0,
    nudge_x = 0.35,
    lineheight = 0.95,
    fontface = "bold",
    size = 4,
    show.legend = FALSE
  ) +
  geom_segment(
    data = annot,
    aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend,
      color = country_name
    ),
    linewidth = 0.35,
    alpha = 0.7,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = annot,
    aes(
      x = x,
      y = y,
      label = label,
      color = country_name
    ),
    hjust = 0,
    vjust = 0,
    fontface = "italic",
    size = 3.5,
    lineheight = 0.95,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = country_cols
  ) +
  scale_linewidth_manual(
    values = c(
      "Denmark" = 1.3,
      "Ireland" = 1.0,
      "Norway" = 1.0
    )
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    breaks = 0:4,
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  scale_x_continuous(
    breaks = seq(1990, max_year, 5),
    expand = expansion(mult = c(0.01, 0.15))
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  labs(
    title = "Wind power followed policy",
    subtitle = paste0(
      "<span style='letter-spacing:2px;'>ENERGY POLICY | 1990-", max_year, "</span><br>",
      "Three high-wind countries followed very different paths.<br>",
      "Wind energy as a share of <b>total final energy consumption</b>."
    ),
    x = NULL,
    y = "Share of TFEC",
    caption = paste0(
      "R. A. Jacobsen | @AulieRoy | Source: Sustainable Energy for all (SE4ALL)"
    )
  ) +
  theme_minimal(
    base_family = "lato",
    base_size = 12
  ) +
  theme(
    plot.background = element_rect(
      fill = bg,
      color = NA
    ),
    panel.background = element_rect(
      fill = bg,
      color = NA
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = grid_col,
      linewidth = 0.35
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      color = muted,
      size = 10,
      face = "bold",
      hjust = 1
    ),
    axis.text = element_text(
      color = muted,
      size = 10
    ),
    plot.title = element_text(
      color = ink,
      face = "bold",
      size = 26,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      color = muted,
      size = 12.5,
      lineheight = 1.3,
      margin = margin(b = 22)
    ),
    plot.caption = element_markdown(
      color = muted,
      size = 8.5,
      lineheight = 1.3,
      hjust = 0,
      margin = margin(t = 18)
    ),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    legend.position = "none",
    plot.margin = margin(24, 64, 22, 26)
  )

p

