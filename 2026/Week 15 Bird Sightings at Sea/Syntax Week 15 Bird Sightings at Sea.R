
# TidyTuesday week 15
# Bird Sightings at Sea
# This week's data comes from seabird logbook records near New Zealand
# Code by R.A. Jacobsen

library(tidyverse)
library(showtext)
library(scales)

# font
sysfonts::font_add_google("Lato", "lato")
showtext_auto()

# load data
birds <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/birds.csv"
)

# a few soft colours
cols <- c(
  text = "#2F3A3D",
  line = "#C9D5D1",
  point = "#8FA9A3",
  highlight = "#6D8B86",
  grid = "#E8ECEA",
  bg = "white"
)

# clean up and get top species
top_species <- birds %>%
  mutate(count = na_if(count, 99999)) %>%
  filter(!is.na(species_scientific_name)) %>%
  filter(!is.na(count), count > 0) %>%
  group_by(species_scientific_name) %>%
  summarise(
    total_birds = sum(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  slice_max(total_birds, n = 10, with_ties = FALSE) %>%
  arrange(total_birds) %>%
  mutate(
    species_scientific_name = factor(
      species_scientific_name,
      levels = species_scientific_name
    ),
    label = comma(total_birds),
    top_one = total_birds == max(total_birds)
  )

# plot
ggplot(top_species, aes(x = total_birds, y = species_scientific_name)) +
  geom_segment(
    aes(x = 0, xend = total_birds, yend = species_scientific_name),
    linewidth = 1.4,
    color = cols["line"]
  ) +
  geom_point(
    aes(fill = top_one),
    size = 4.8,
    shape = 21,
    stroke = 0,
    show.legend = FALSE
  ) +
  geom_text(
    aes(label = label),
    hjust = -0.15,
    size = 3.8,
    family = "lato",
    color = cols["text"]
  ) +
  scale_fill_manual(
    values = c("TRUE" = cols["highlight"], "FALSE" = cols["point"])
  ) +
  scale_x_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Some seabirds show up far more than others",
    subtitle = "Top 10 species by total birds counted in the at-sea records",
    x = "Birds counted",
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "lato") +
  theme(
    plot.background = element_rect(fill = cols["bg"], color = NA),
    panel.background = element_rect(fill = cols["bg"], color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = cols["grid"], linewidth = 0.4),
    axis.text.y = element_text(
      size = 11,
      color = cols["text"],
      face = "italic"
    ),
    axis.text.x = element_text(
      size = 10,
      color = cols["text"]
    ),
    axis.title.x = element_text(
      size = 11,
      color = cols["text"]
    ),
    plot.title = element_text(
      size = 20,
      face = "bold",
      color = cols["text"]
    ),
    plot.subtitle = element_text(
      size = 11,
      color = cols["text"],
      margin = margin(b = 12)
    ),
    plot.margin = margin(20, 50, 20, 20)
  )


