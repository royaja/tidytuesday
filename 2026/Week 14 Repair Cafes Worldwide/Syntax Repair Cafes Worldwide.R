
# TidyTuesday 2026 week 14
# Repair Cafes Worldwide
# This week's dataset looks at repair cases from Repair Cafes around the world.
# I wanted to keep things simple and look at the categories that show up the most,
# and then compare how often things were fully repaired, partly repaired, or not repaired.
# Code by R.A. Jacobsen

# Load library 
library(tidyverse)
library(scales)
library(showtext)
library(sysfonts)

font_add_google("Lato", "lato")
showtext_auto()

# load data 

repairs <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-07/repairs.csv",
  show_col_types = FALSE
)

# wrangle 

top_categories <- 
  repairs %>%
  drop_na(category) %>%
  count(category, sort = TRUE) %>%
  slice_head(n = 10)

plot_data <- 
  repairs %>%
  drop_na(category, repaired) %>%
  mutate(
    repaired = str_to_lower(repaired),
    repaired = case_when(
      repaired %in% c("yes", "ja") ~ "Fully repaired",
      repaired == "half" ~ "Partly repaired",
      repaired == "no" ~ "Not repaired",
      TRUE ~ NA_character_
    )
  ) %>%
  drop_na(repaired) %>%
  semi_join(top_categories, by = "category") %>%
  count(category, repaired, name = "cases") %>%
  group_by(category) %>%
  mutate(
    total_cases = sum(cases),
    share = cases / total_cases
  ) %>%
  ungroup()


category_order <- 
  plot_data %>%
  filter(repaired == "Fully repaired") %>%
  arrange(share) %>%
  pull(category)


plot_data <- 
  plot_data %>%
  mutate(
    category_label = str_wrap(category, width = 30),
    category_label = factor(
      category_label,
      levels = str_wrap(category_order, width = 30)
    ),
    repaired = factor(
      repaired,
      levels = c("Fully repaired", "Partly repaired", "Not repaired")
    )
  )


count_labels <- 
  plot_data %>%
  distinct(category_label, total_cases)


ggplot(plot_data, aes(x = share, y = category_label, fill = repaired)) +
  geom_col(
    width = 0.72,
    color = "white",
    linewidth = 0.8
  ) +
  geom_text(
    data = count_labels,
    aes(
      x = 1.03,
      y = category_label,
      label = paste0("n = ", comma(total_cases))
    ),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3.6,
    color = "#6B7280",
    family = "lato"
  ) +
  scale_x_continuous(
    limits = c(0, 1.12),
    labels = label_percent(accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = c(
      "Fully repaired" = "#325D6E",
      "Partly repaired" = "#9DB7C1",
      "Not repaired" = "#E8ECEC"
    )
  ) +
  labs(
    title = "Textiles are repaired far more often than consumer electronics",
    subtitle = "Share of repair outcomes in the 10 most common product categories",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 12, base_family = "lato") +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18,
      color = "#1F2933"
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "#4B5563",
      margin = margin(b = 16)
    ),
    axis.text.y = element_text(
      size = 11,
      color = "#1F2933"
    ),
    axis.text.x = element_text(
      color = "#4B5563"
    ),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(color = "#1F2933"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      color = "#E5E7EB",
      linewidth = 0.4
    ),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(18, 40, 18, 18)
  )

