
# Participation in the TidyTuesday challenge for week 19. 
# This weeks TidyTuesday data provide a  time series data on industrial 
# production of food and beverages, transport equipment, and textiles covering over 100 years.
# The data comes from https://www.istat.it/ 
# To find the data, go to the offical TidyTuesday repository
# https://github.com/rfordatascience/tidytuesday/blob/main/data/2026/2026-05-05/readme.md
# I aim for a policy angle on this weeks data

# Code developed by R.A.Jacobsen 

# Load libraries
library(tidyverse)
library(showtext)
library(sysfonts)
library(scales)

font_add_google("Lato", "lato")
showtext_auto()

# Load data
food_beverages <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/food_beverages.csv",
    show_col_types = FALSE
  )

textiles <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/textiles.csv",
    show_col_types = FALSE
  )

transport <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/transport.csv",
    show_col_types = FALSE
  )

# Wrangle
production_long <-
  bind_rows(
    food_beverages %>%
      pivot_longer(
        cols = -Year,
        names_to = "industry",
        values_to = "value"
      ) %>%
      mutate(sector = "Food and beverages"),
    
    textiles %>%
      pivot_longer(
        cols = -Year,
        names_to = "industry",
        values_to = "value"
      ) %>%
      mutate(sector = "Textiles"),
    
    transport %>%
      # Drop shipbuilding and repair columns ??? not comparable with the rest
      select(-contains("Ship"), -contains("Repair")) %>%
      pivot_longer(
        cols = -Year,
        names_to = "industry",
        values_to = "value"
      ) %>%
      mutate(sector = "Transport")
  ) %>%
  filter(!is.na(value), value > 0) %>%
  mutate(
    industry_label = case_when(
      industry == "Ethyl_alcohol_1" ~ "Ethyl alcohol (1)",
      industry == "Ethyl_alcohol_2" ~ "Ethyl alcohol (2)",
      industry == "Steam_and_electric_engine" ~ "Steam/electric engines",
      industry == "Rail_cars_and_electric_locomotives" ~ "Rail cars & locos",
      industry == "Mail_luggage_vans_and_carriages" ~ "Mail/luggage vans",
      TRUE ~ str_replace_all(industry, "_", " ") %>% str_to_sentence()
    )
  )

change_data <-
  production_long %>%
  group_by(sector, industry, industry_label) %>%
  arrange(Year, .by_group = TRUE) %>%
  summarise(
    start_value = first(value),
    end_value = last(value),
    ratio = end_value / start_value,
    log2_ratio = log2(ratio),
    direction = if_else(ratio >= 1, "Increased", "Decreased"),
    .groups = "drop"
  ) %>%
  mutate(
    ratio_label = case_when(
      ratio >= 1000 ~ paste0(comma(round(ratio, 0)), "x"),
      ratio >= 10   ~ paste0(round(ratio, 0), "x"),
      ratio >= 1    ~ paste0(round(ratio, 1), "x"),
      TRUE          ~ paste0(round(ratio, 2), "x")
    ),
    label_hjust = if_else(log2_ratio >= 0, -0.25, 1.25)
  )

change_plot_data <-
  change_data %>%
  filter(!industry %in% c("Total_yarn", "Total_textiles"))

relative_axis_labels <-
  function(x) {
    ratio <- 2^x
    
    case_when(
      ratio >= 1000 ~ paste0(comma(round(ratio, 0)), "x"),
      ratio >= 10   ~ paste0(round(ratio, 0), "x"),
      ratio >= 1    ~ paste0(round(ratio, 1), "x"),
      TRUE          ~ paste0(round(ratio, 2), "x")
    )
  }

# Plot
econ_red <- "#E3120B"

ggplot(
  change_plot_data,
  aes(
    x = log2_ratio,
    y = fct_reorder(industry_label, log2_ratio),
    color = direction
  )
) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.45,
    color = "grey70"
  ) +
  geom_segment(
    aes(
      x = 0,
      xend = log2_ratio,
      yend = industry_label
    ),
    linewidth = 0.9
  ) +
  geom_point(size = 3) +
  geom_text(
    aes(
      label = ratio_label,
      hjust = label_hjust
    ),
    size = 3.2,
    show.legend = FALSE,
    family = "lato"
  ) +
  facet_grid(
    sector ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  scale_color_manual(
    values = c(
      "Decreased" = "grey45",
      "Increased" = econ_red
    )
  ) +
  scale_x_continuous(
    labels = relative_axis_labels,
    expand = expansion(mult = c(0.08, 0.18))
  ) +
  labs(
    title = "Italy's industrial base changed unevenly over time",
    subtitle = "Each bar shows how much a product's output changed from the first to the last available year.\nSome lines grew by orders of magnitude, others collapsed, which is reflecting industrial restructuring, wars, and trade shifts.",
    x = "Change from start",
    y = NULL,
    color = NULL,
    caption = "R.A. Jacobsen | @AulieRoy | Source: ISTAT historical statistics via TidyTuesday. 1x = no change from first observed value."
  ) +
  theme_minimal(base_size = 12, base_family = "lato") +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.y = element_text(
      face = "bold",
      angle = 0
    ),
    axis.text.y = element_text(size = 10.5),
    axis.title.x = element_text(size = 12),
    plot.title = element_text(
      face = "bold",
      size = 18
    ),
    plot.subtitle = element_text(
      color = "grey35",
      size = 12
    ),
    plot.caption = element_text(
      color = "grey40",
      size = 9,
      hjust = 0
    )
  )


