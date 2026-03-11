
# TidyTuesday Week 10: How likely is "likely"?
# Ordered uncertainty ladder for probability phrases

# This dataset comes from an online quiz created by Adam Kucharski,
# where 5,000+ participants compared probability phrases and assigned
# them values from 0 to 100%.

# Code developed by R. A. Jacobsen for the TidyTuesday challenge.

# Library 
library(tidyverse)
library(scales)
library(forcats)
library(sysfonts)
library(showtext)

# Load font
font_add_google("Lato", family = "lato")

showtext_auto()

# Load data
absolute_judgements <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-10/absolute_judgements.csv')


# Summarise 
phrase_summary <-
  absolute_judgements %>%
  group_by(term) %>%
  summarise(
    q10 = quantile(probability, 0.10, na.rm = TRUE),
    q25 = quantile(probability, 0.25, na.rm = TRUE),
    median_prob = median(probability, na.rm = TRUE),
    q75 = quantile(probability, 0.75, na.rm = TRUE),
    q90 = quantile(probability, 0.90, na.rm = TRUE),
    iqr = IQR(probability, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(median_prob) %>%
  mutate(
    term = factor(term, levels = term),
    note = case_when(
      term == "Realistic Possibility" ~ "Widest disagreement",
      term == "Might Happen" ~ "Also interpreted broadly",
      term == "Could Happen" ~ "Also interpreted broadly",
      TRUE ~ NA_character_
    ),
    note_x = q90 + 3
  )

# Plot
p <-
  ggplot(phrase_summary, aes(y = term)) +
  geom_vline(
    xintercept = 50,
    linewidth = 0.5,
    color = "#D9D9D9",
    linetype = "dashed"
  ) +
  geom_segment(
    aes(x = q10, xend = q90, yend = term),
    linewidth = 1.3,
    color = "#D0D0D0",
    lineend = "round"
  ) +
  geom_segment(
    aes(
      x = q25,
      xend = q75,
      yend = term,
      color = term == "Realistic Possibility"
    ),
    linewidth = 5.2,
    lineend = "round",
    show.legend = FALSE
  ) +
  geom_point(
    aes(x = median_prob),
    size = 3,
    color = "#1E2D59"
  ) +
  geom_text(
    aes(x = note_x, label = note),
    na.rm = TRUE,
    hjust = 0,
    size = 3.5,
    color = "#9C274C",
    family = "lato"
  ) +
  scale_color_manual(
    values = c("TRUE" = "#9C274C", "FALSE" = "#7A7A7A")
  ) +
  scale_x_continuous(
    limits = c(0, 110),
    breaks = seq(0, 100, 20),
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "How likely is 'likely'?",
    subtitle = "People tend to agree on phrases that sound close to certainty or impossibility, but meanings become much less shared in the middle of the scale.",
    x = "Perceived probability (%)",
    y = NULL,
    caption = "R. A. Jacobsen | @AulieRoy | Data: Adam Kucharski's CAPphrase project | TidyTuesday 2026 Week 10"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 13, base_family = "lato") +
  theme(
    plot.title = element_text(face = "bold", size = 24, hjust = 0),
    plot.subtitle = element_text(
      size = 12,
      color = "#3F3F3F",
      lineheight = 1.15,
      margin = margin(b = 18)
    ),
    axis.text.y = element_text(size = 11, color = "#222222"),
    axis.text.x = element_text(size = 10, color = "#555555"),
    axis.title.x = element_text(size = 11, margin = margin(t = 12)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#ECECEC", linewidth = 0.45),
    plot.caption = element_text(
      size = 9,
      color = "#6A6A6A",
      hjust = 0,
      margin = margin(t = 14)
    ),
    plot.margin = margin(20, 90, 30, 20)
  )

p
