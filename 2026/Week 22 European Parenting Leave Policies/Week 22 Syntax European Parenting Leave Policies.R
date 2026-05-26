  
# TidyTuesday Week 22 
# This weeks data explores  European Parenting Leave Policies. The European 
# Parenting Leave Policies (EPLP) Dataset provides harmonised data on maternity, 
# co-parent, paid parental, and job-protected leave regulations across 21 European 
# countries from 1970 to 2024.
# The data was currated by Nicola Rennie
# Code developed by R.A.Jacobsen 
  
# Load library 
library(tidyverse)
library(showtext)
library(sysfonts)
library(ggtext)
library(scales)

# Fonts
font_add_google("Lato", "lato")
font_add_google("Spectral", "spectral")
showtext_auto()
showtext_opts(dpi = 96)

# Load data
eplp <- 
  readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-02/eplp.csv",
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

col_none <- 
  "#E8E0D0"

col_zero <- 
  "#C9BFA8"

week_bins <- 
  c(0, 2, 5, 10, 20, Inf)

week_labels <- 
  c("1-2 weeks", "3-5 weeks", "6-10 weeks", "11-20 weeks", "20+ weeks")

week_fills <- 
  c("#9DBCA8", "#5E9276", "#2F6B4F", "#1C4D3A", "#0E3A53")

country_lookup <- 
  tribble(
    ~country, ~country_name,
    "AT", "Austria",
    "BE", "Belgium",
    "CZ", "Czechia",
    "DE", "Germany",
    "DK", "Denmark",
    "EE", "Estonia",
    "ES", "Spain",
    "FI", "Finland",
    "FR", "France",
    "GB", "United Kingdom",
    "UK", "United Kingdom",
    "GR", "Greece",
    "HU", "Hungary",
    "IE", "Ireland",
    "IT", "Italy",
    "LT", "Lithuania",
    "LV", "Latvia",
    "NL", "Netherlands",
    "NO", "Norway",
    "PL", "Poland",
    "PT", "Portugal",
    "SE", "Sweden",
    "SI", "Slovenia",
    "SK", "Slovakia"
  )

panel <- 
  eplp %>% 
  mutate(
    country = str_to_upper(country),
    year = as.integer(year),
    co_ld = na_if(as.numeric(co_ld), -98)
  ) %>% 
  left_join(country_lookup, by = "country") %>% 
  mutate(
    country_name = coalesce(country_name, country)
  ) %>% 
  group_by(country_name) %>% 
  mutate(
    co_weeks = if_else(is.na(co_ld), 0, pmax(co_ld, 0)),
    first_year = suppressWarnings(min(year[co_weeks > 0], na.rm = TRUE)),
    first_year = if_else(is.finite(first_year), first_year, NA_integer_),
    recorded = !is.na(first_year) & year >= first_year
  ) %>% 
  ungroup() %>% 
  mutate(
    fill_class = case_when(
      !recorded ~ "No recorded leave",
      co_weeks == 0 ~ "0 weeks",
      TRUE ~ as.character(
        cut(
          co_weeks,
          breaks = week_bins,
          labels = week_labels,
          right = TRUE,
          include.lowest = TRUE
        )
      )
    ),
    fill_class = factor(
      fill_class,
      levels = c("No recorded leave", "0 weeks", week_labels)
    )
  )

fill_values <- 
  c(
    "No recorded leave" = col_none,
    "0 weeks" = col_zero,
    setNames(week_fills, week_labels)
  )

order_tbl <- 
  panel %>% 
  distinct(country_name, first_year) %>% 
  mutate(
    sort_year = coalesce(first_year, 9999L)
  ) %>% 
  arrange(sort_year, country_name)

panel <- 
  panel %>% 
  mutate(
    country_name = factor(
      country_name,
      levels = rev(order_tbl$country_name)
    )
  )

first_dots <- 
  panel %>% 
  filter(
    recorded,
    year == first_year
  ) %>% 
  distinct(
    country_name,
    first_year
  )

year_min <- 
  min(panel$year, na.rm = TRUE)

year_max <- 
  max(panel$year, na.rm = TRUE)

n_countries <- 
  nlevels(panel$country_name)

directive_year <- 
  2019

p <- 
  ggplot(panel, aes(x = year, y = country_name, fill = fill_class)) +
  annotate(
    "rect",
    xmin = directive_year - 0.5,
    xmax = year_max + 0.5,
    ymin = 0.4,
    ymax = n_countries + 0.6,
    fill = "#0E3A53",
    alpha = 0.05
  ) +
  geom_tile(
    width = 1.02,
    height = 0.82,
    color = bg,
    linewidth = 0.15
  ) +
  geom_point(
    data = first_dots,
    aes(
      x = first_year,
      y = country_name
    ),
    inherit.aes = FALSE,
    shape = 21,
    size = 1.7,
    fill = bg,
    color = ink,
    stroke = 0.45
  ) +
  annotate(
    "segment",
    x = directive_year,
    xend = directive_year,
    y = 0.4,
    yend = n_countries + 0.6,
    color = ink,
    linewidth = 0.35,
    linetype = "dashed",
    alpha = 0.55
  ) +
  annotate(
    "richtext",
    x = directive_year - 0.4,
    y = n_countries + 1.05,
    label = "<i>EU Work-Life Balance Directive, 2019</i>",
    hjust = 1,
    vjust = 0,
    family = "lato",
    size = 2.5,
    color = ink,
    fill = NA,
    label.color = NA
  ) +
  scale_fill_manual(
    values = fill_values,
    drop = FALSE,
    name = "Weeks of co-parent leave"
  ) +
  scale_x_continuous(
    breaks = seq(1970, 2020, 10),
    expand = expansion(mult = c(0.004, 0.02))
  ) +
  guides(
    fill = guide_legend(
      nrow = 1,
      label.position = "bottom",
      keywidth = unit(11, "pt"),
      keyheight = unit(7, "pt"),
      title.position = "left",
      title.vjust = 0.5
    )
  ) +
  labs(
    title = "Shared leave became a policy wave",
    subtitle = paste0(
      "<span style='letter-spacing:2px;'>EUROPEAN FAMILY POLICY | ", year_min, "-", year_max, "</span><br>",
      "A few countries introduced generous co-parent leave early. Many others moved later.<br>",
      "The dot marks the first recorded year with co-parent leave."
    ),
    x = NULL,
    y = NULL,
    caption = paste0(
      "R. A. Jacobsen | @AulieRoy | Source: European Parenting Leave Policies Dataset<br>"
    )
  ) +
  theme_minimal(
    base_family = "lato",
    base_size = 9
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
    panel.grid = element_blank(),
    axis.text.y = element_text(
      color = ink,
      size = 6.8,
      face = "bold"
    ),
    axis.text.x = element_text(
      color = muted,
      size = 8
    ),
    plot.title = element_text(
      family = "spectral",
      color = ink,
      face = "bold",
      size = 18,
      margin = margin(b = 4)
    ),
    plot.subtitle = element_markdown(
      color = muted,
      size = 8.5,
      lineheight = 1.2,
      margin = margin(b = 14)
    ),
    plot.caption = element_markdown(
      color = muted,
      size = 6.2,
      lineheight = 1.2,
      hjust = 0,
      margin = margin(t = 12)
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "bottom",
    legend.justification = "left",
    legend.title = element_text(
      color = muted,
      size = 7.2
    ),
    legend.text = element_text(
      color = muted,
      size = 6.8
    ),
    legend.margin = margin(t = 4),
    plot.margin = margin(18, 30, 18, 22)
  ) +
  coord_cartesian(
    clip = "off"
  )

p
