
# TidyTuesday week 20. 
# This weeks TidyTuesday data looks into the metadata from Crossref. divided 
# by country. 
# Below is a link to this weeks data: 
# https://github.com/rfordatascience/tidytuesday/blob/main/data/2026/2026-05-19/readme.md
# Code is developed by R.A.Jacobsen

# Load libraries
library(tidyverse)
library(ggrepel)
library(scales)
library(countrycode)
library(showtext)
library(sysfonts)

# Load data
metadata <- 
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-19/metadata_coverage_stats_by_country.csv",
    show_col_types = FALSE
  )

font_add_google("Lato", "lato")
showtext_auto()

# Wrangle
metadata <- 
  metadata %>%
  mutate(
    current_up_to = as.Date(current_up_to)
  )

my_document_type <- 
  "journal-article"

min_dois <- 
  10000

countries_to_label <- 
  c(
    "EST", "BRA", "USA", "CHE", "NOR",
    "FRA", "CHN", "IND", "ZAF", "IDN"
  )

country_month <- 
  metadata %>%
  filter(document_type == my_document_type) %>%
  filter(n_dois > 0)

country_month <- 
  country_month %>%
  group_by(current_up_to, region_id, iso3_code) %>%
  summarise(
    n_dois = sum(n_dois, na.rm = TRUE),
    with_ref = sum(with_ref, na.rm = TRUE),
    with_abstract = sum(with_abstract, na.rm = TRUE),
    with_license = sum(with_license, na.rm = TRUE),
    acknowledges_funding = sum(acknowledges_funding, na.rm = TRUE),
    with_orcid_for_authors = sum(with_orcid_for_authors, na.rm = TRUE),
    with_ror_id_for_affiliations = sum(with_ror_id_for_affiliations, na.rm = TRUE),
    .groups = "drop"
  )

country_month <- 
  country_month %>%
  mutate(
    country = countrycode(
      iso3_code,
      origin = "iso3c",
      destination = "country.name",
      warn = FALSE
    )
  )

country_month <- 
  country_month %>%
  mutate(
    country = if_else(
      is.na(country),
      iso3_code,
      country
    )
  )

country_month <- 
  country_month %>%
  mutate(
    share_ref = with_ref / n_dois,
    share_abstract = with_abstract / n_dois,
    share_license = with_license / n_dois,
    share_funding = acknowledges_funding / n_dois,
    share_orcid = with_orcid_for_authors / n_dois,
    share_ror = with_ror_id_for_affiliations / n_dois
  )

country_month <- 
  country_month %>%
  mutate(
    metadata_richness = (
      share_ref +
        share_abstract +
        share_license +
        share_funding +
        share_orcid +
        share_ror
    ) / 6
  )

start_date <- 
  country_month %>%
  filter(current_up_to >= as.Date("2025-01-01")) %>%
  summarise(
    start_date = min(current_up_to, na.rm = TRUE)
  ) %>%
  pull(start_date)

latest_date <- 
  country_month %>%
  summarise(
    latest_date = max(current_up_to, na.rm = TRUE)
  ) %>%
  pull(latest_date)

plot_data <- 
  country_month %>%
  filter(current_up_to == start_date | current_up_to == latest_date)

plot_data <- 
  plot_data %>%
  mutate(
    period = case_when(
      current_up_to == start_date ~ "start",
      current_up_to == latest_date ~ "latest"
    )
  )

plot_data <- 
  plot_data %>%
  select(
    iso3_code,
    country,
    region_id,
    period,
    n_dois,
    metadata_richness
  )

plot_data <- 
  plot_data %>%
  pivot_wider(
    names_from = period,
    values_from = c(n_dois, metadata_richness)
  )

plot_data <- 
  plot_data %>%
  mutate(
    richness_now = metadata_richness_latest,
    richness_start = metadata_richness_start,
    momentum = richness_now - richness_start,
    dois_now = n_dois_latest
  )

plot_data <- 
  plot_data %>%
  filter(!is.na(richness_now)) %>%
  filter(!is.na(momentum)) %>%
  filter(dois_now >= min_dois)

richness_cut <- 
  median(plot_data$richness_now, na.rm = TRUE)

momentum_cut <- 
  median(plot_data$momentum, na.rm = TRUE)

plot_data <- 
  plot_data %>%
  mutate(
    quadrant = case_when(
      richness_now >= richness_cut & momentum >= momentum_cut ~ "Research Nexus leaders",
      richness_now < richness_cut & momentum >= momentum_cut ~ "Fast movers",
      richness_now >= richness_cut & momentum < momentum_cut ~ "Established platforms",
      TRUE ~ "Metadata gap"
    )
  )

plot_data <- 
  plot_data %>%
  mutate(
    quadrant = factor(
      quadrant,
      levels = c(
        "Research Nexus leaders",
        "Fast movers",
        "Established platforms",
        "Metadata gap"
      )
    )
  )

label_data <- 
  plot_data %>%
  filter(iso3_code %in% countries_to_label)

# plot 
p <- 
  ggplot(
    plot_data,
    aes(
      x = richness_now,
      y = momentum,
      size = dois_now,
      fill = quadrant
    )
  ) +
  annotate(
    "rect",
    xmin = -Inf,
    xmax = richness_cut,
    ymin = momentum_cut,
    ymax = Inf,
    fill = "#F6E2C6",
    alpha = 0.75
  ) +
  annotate(
    "rect",
    xmin = richness_cut,
    xmax = Inf,
    ymin = momentum_cut,
    ymax = Inf,
    fill = "#D9ECE6",
    alpha = 0.75
  ) +
  annotate(
    "rect",
    xmin = richness_cut,
    xmax = Inf,
    ymin = -Inf,
    ymax = momentum_cut,
    fill = "#E8E4DC",
    alpha = 0.75
  ) +
  annotate(
    "rect",
    xmin = -Inf,
    xmax = richness_cut,
    ymin = -Inf,
    ymax = momentum_cut,
    fill = "#EFEFEF",
    alpha = 0.75
  ) +
  geom_vline(
    xintercept = richness_cut,
    linetype = "dashed",
    linewidth = 0.55,
    colour = "#6D6259"
  ) +
  geom_hline(
    yintercept = momentum_cut,
    linetype = "dashed",
    linewidth = 0.55,
    colour = "#6D6259"
  ) +
  geom_point(
    shape = 21,
    colour = "white",
    stroke = 0.45,
    alpha = 0.92
  ) +
  geom_label_repel(
    data = label_data,
    aes(label = country),
    family = "lato",
    size = 3.5,
    fontface = "bold",
    fill = "#FBF7EF",
    colour = "#202020",
    label.size = 0,
    box.padding = 0.45,
    point.padding = 0.25,
    min.segment.length = 0,
    segment.colour = "#8A8178",
    max.overlaps = 40,
    show.legend = FALSE
  ) +
  annotate(
    "text",
    x = min(plot_data$richness_now, na.rm = TRUE) +
      diff(range(plot_data$richness_now, na.rm = TRUE)) * 0.78,
    y = min(plot_data$momentum, na.rm = TRUE) +
      diff(range(plot_data$momentum, na.rm = TRUE)) * 0.84,
    label = "RESEARCH NEXUS\nLEADERS",
    family = "lato",
    fontface = "bold",
    size = 3.9,
    colour = "#71958F",
    lineheight = 0.9
  ) +
  annotate(
    "text",
    x = min(plot_data$richness_now, na.rm = TRUE) +
      diff(range(plot_data$richness_now, na.rm = TRUE)) * 0.12,
    y = min(plot_data$momentum, na.rm = TRUE) +
      diff(range(plot_data$momentum, na.rm = TRUE)) * 0.84,
    label = "FAST\nMOVERS",
    family = "lato",
    fontface = "bold",
    size = 3.9,
    colour = "#B98A58",
    lineheight = 0.9
  ) +
  annotate(
    "text",
    x = min(plot_data$richness_now, na.rm = TRUE) +
      diff(range(plot_data$richness_now, na.rm = TRUE)) * 0.78,
    y = min(plot_data$momentum, na.rm = TRUE) +
      diff(range(plot_data$momentum, na.rm = TRUE)) * 0.10,
    label = "ESTABLISHED\nPLATFORMS",
    family = "lato",
    fontface = "bold",
    size = 3.9,
    colour = "#888888",
    lineheight = 0.9
  ) +
  annotate(
    "text",
    x = min(plot_data$richness_now, na.rm = TRUE) +
      diff(range(plot_data$richness_now, na.rm = TRUE)) * 0.12,
    y = min(plot_data$momentum, na.rm = TRUE) +
      diff(range(plot_data$momentum, na.rm = TRUE)) * 0.10,
    label = "METADATA\nGAP",
    family = "lato",
    fontface = "bold",
    size = 3.9,
    colour = "#999999",
    lineheight = 0.9
  ) +
  scale_x_continuous(
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0.08, 0.08))
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0.12, 0.12))
  ) +
  scale_size_area(
    max_size = 17,
    breaks = c(10000000, 20000000, 30000000),
    labels = c("10M", "20M", "30M")
  ) +
  scale_fill_manual(
    values = c(
      "Research Nexus leaders" = "#267064",
      "Fast movers" = "#D77B2D",
      "Established platforms" = "#73777D",
      "Metadata gap" = "#BBBBBB"
    )
  ) +
  guides(
    fill = "none",
    size = guide_legend(
      title = "DOIs",
      title.position = "left",
      label.position = "bottom",
      nrow = 1,
      override.aes = list(
        fill = "#73777D",
        colour = "white",
        alpha = 0.35
      )
    )
  ) +
  labs(
    title = "Who is building the richest open research record?",
    subtitle = "More research output does not always mean better-connected metadata. Estonia shows how smaller publishing systems can move faster on making research easier to find, link and reuse.",
    x = "Richness in metadata",
    y = "Change in richness of metadata",
    size = "DOIs",
    caption = "Source: Crossref | R.A.Jacobsen | @AulieRoy"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "lato") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      colour = "#DDD6CC",
      linewidth = 0.35
    ),
    plot.title = element_text(
      size = 25,
      face = "bold",
      colour = "#202020",
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      size = 12.5,
      colour = "#4A4A4A",
      margin = margin(b = 18)
    ),
    axis.title = element_text(
      size = 11,
      face = "bold",
      colour = "#333333"
    ),
    axis.text = element_text(
      size = 10,
      colour = "#333333"
    ),
    legend.position = "bottom",
    legend.background = element_blank(),
    legend.box.margin = margin(t = 4, b = 4),
    legend.title = element_text(
      size = 10,
      face = "bold"
    ),
    legend.text = element_text(size = 9),
    legend.key = element_blank(),
    plot.caption = element_text(
      size = 8.5,
      colour = "#666666",
      hjust = 0,
      margin = margin(t = 12)
    ),
    plot.margin = margin(22, 28, 22, 28)
  )

p


