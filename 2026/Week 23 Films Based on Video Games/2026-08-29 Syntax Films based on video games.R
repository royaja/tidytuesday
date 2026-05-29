
# TidyTuesday Week 23
# This weeks data explores films based on video games.
# The dataset is based on the Wikipedia article List of films based on video games.
# It includes theatrical releases, direct-to-video productions, television films,
# short films, and documentaries adapted from video game franchises.
# The data includes release dates, box office figures, budgets, critic scores,
# CinemaScore grades, distributors, and original game publishers where available.
# The data was curated by Georgios Karamanis.
# Code developed by R.A. Jacobsen


# load library
library(tidyverse)
library(showtext)
library(sysfonts)
library(ggtext)
library(scales)

font_add_google("Lato", "lato")
showtext_auto()
showtext_opts(dpi = 320)

bg    <- "#FFFFFF"
ink   <- "#1B2A38"
muted <- "#7C8D99"
grid  <- "#DDD6CB"

blue  <- "#2F607E"
rust  <- "#BF5840"


# load data
films <- 
  read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-09/game_films.csv",
  show_col_types = FALSE
)


# wrangle
plot_data <- 
  films |>
  filter(
    category == "Theatrical releases",
    worldwide_box_office_currency == "$",
    !is.na(worldwide_box_office),
    !is.na(original_game_publisher)
  ) |>
  mutate(
    original_game_publisher = str_squish(original_game_publisher)
  ) |>
  separate_rows(
    original_game_publisher,
    sep = "\\s*\\|\\s*"
  ) |>
  distinct(
    title,
    original_game_publisher,
    .keep_all = TRUE
  ) |>
  count(
    original_game_publisher,
    name = "films",
    sort = TRUE
  ) |>
  slice_head(n = 10) |>
  mutate(
    rank = row_number(),
    group = if_else(rank <= 3, "top", "other"),
    publisher = case_when(
      original_game_publisher == "The Pok??mon Company" ~ "The Pok??mon Company",
      original_game_publisher == "Sony Interactive Entertainment" ~ "Sony Entertainment",
      TRUE ~ original_game_publisher
    ),
    publisher = fct_reorder(publisher, films),
    label = paste0(films, " films")
  )


# plot
p <- 
  ggplot(
  plot_data,
  aes(
    x = films,
    y = publisher,
    fill = group
  )
) +
  geom_col(
    width = 0.42,
    color = NA
  ) +
  geom_text(
    aes(
      x = films + 0.20,
      label = label
    ),
    family = "lato",
    size = 2.05,
    color = ink,
    hjust = 0
  ) +
  scale_x_continuous(
    breaks = c(0, 5, 10, 15),
    limits = c(0, 16.8),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_manual(
    values = c(
      "top" = rust,
      "other" = blue
    ),
    guide = "none"
  ) +
  labs(
    title = "<span style='color:#BF5840'>A few publishers dominate</span><br>game-to-film adaptations",
    subtitle = "Top original game publishers by released theatrical film adaptations with reported dollar box office.",
    x = "Number of films",
    y = NULL,
    caption = "R. A. Jacobsen | @AulieRoy | Source: Wikipedia list of films based on video games"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "lato", base_size = 7.4) +
  theme(
    plot.background = element_rect(fill = bg, color = NA),
    panel.background = element_rect(fill = bg, color = NA),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      color = grid,
      linewidth = 0.20
    ),
    
    axis.title.x = element_text(
      color = ink,
      size = 6.5,
      face = "bold",
      margin = margin(t = 9)
    ),
    axis.text.x = element_text(
      color = muted,
      size = 6.1
    ),
    axis.text.y = element_text(
      color = ink,
      size = 6.3,
      face = "bold",
      lineheight = 0.78,
      margin = margin(r = 6)
    ),
    axis.ticks = element_blank(),
    
    plot.title = element_markdown(
      family = "lato",
      size = 11.2,
      face = "bold",
      color = ink,
      lineheight = 0.96,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      size = 6.4,
      color = muted,
      lineheight = 1.18,
      margin = margin(b = 16)
    ),
    plot.caption = element_text(
      size = 5.6,
      color = muted,
      hjust = 0,
      margin = margin(t = 15)
    ),
    
    plot.title.position = "plot",
    plot.caption.position = "plot",
    
    plot.margin = margin(22, 42, 20, 30)
  )

p
