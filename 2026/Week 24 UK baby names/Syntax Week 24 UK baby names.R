
# TidyTuesday Week 24 - Trends in UK baby names 
# In this weeks TidyTuesday challgene, I look into trends in baby names
# in the UK.
# One of the questions being asked this week is: Can you show the Bridgerton trend in charts?

# Code developed by R.A.Jacobsen

# Load libraries
library(tidyverse)
library(showtext)
library(sysfonts)
library(rvest)
library(janitor)
library(scales)

# Load data
england_wales_names <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-16/england_wales_names.csv",
  show_col_types = FALSE
)

ni_names <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-16/ni_names.csv",
  show_col_types = FALSE
)

scotland_names <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-16/scotland_names.csv",
  show_col_types = FALSE
)


# Fonts

font_add_google("Lato", "lato")
showtext_auto()


# Wrangle
all_names <- 
  bind_rows(
    england_wales_names |> mutate(region = "England & Wales"),
    ni_names            |> mutate(region = "Northern Ireland"),
    scotland_names      |> mutate(region = "Scotland")
  ) |>
  rename_with(str_to_lower) |>
  mutate(
    region = factor(
      region,
      levels = c("England & Wales", "Scotland", "Northern Ireland")
    ),
    name = str_to_title(name),
    sex = str_to_lower(sex),
    sex = case_when(
      sex %in% c("f", "female", "girl", "girls") ~ "Girls",
      sex %in% c("m", "male", "boy", "boys") ~ "Boys",
      TRUE ~ sex
    )
  )


#Scrape names of characters from Wikipedia
url <- "https://en.wikipedia.org/wiki/List_of_Bridgerton_characters"

bridgerton_page <- read_html(url)

tables <- 
  bridgerton_page |>
  html_elements("table.wikitable") |>
  html_table(fill = TRUE)

bridgerton_cast <- 
  tables |>
  map(clean_names) |>
  keep(~ all(c("actor", "character") %in% names(.x))) |>
  pluck(1) |>
  mutate(
    across(where(is.character), ~ str_squish(.x))
  ) |>
  filter(
    actor != "",
    character != "",
    actor != "Actor"
  ) |>
  distinct()

bridgerton_cast


# Extract first names
bridgerton_first_names <- 
  bridgerton_cast |> 
  select(character) |> 
  mutate(
    character = str_squish(character),
    character = str_remove_all(character, "\\[[^]]*\\]"),
    character = str_remove_all(character, "\\s*\\([^)]*\\)"),
    character = str_remove(
      character,
      regex(
        "^(Young|Lady|Lord|Mrs\\.?|Mr\\.?|Miss|Queen|King|Princess|Prince|Duke|Duchess)\\s+",
        ignore_case = TRUE
      )
    ),
    character = word(character, 1),
    character = str_to_title(character)
  ) |> 
  filter(
    !is.na(character),
    character != ""
  ) |>
  distinct() |> 
  arrange(character)

bridgerton_first_names


# Create Bridgerton name vector
bridgerton_name_vector <- 
  bridgerton_first_names |> 
  pull(character)

# Filter baby-name data to Bridgerton names
bridgerton_names_data <- 
  all_names |> 
  filter(
    sex == "Girls",
    name %in% bridgerton_name_vector
  )

# Select focus names
bridgerton_focus_names <- c("Daphne", "Eloise", "Penelope")

bridgerton_focus <- 
  bridgerton_names_data |> 
  filter(
    name %in% bridgerton_focus_names,
    year >= 2010,
    year <= 2025,
    !is.na(rank)
  ) |>
  mutate(
    name = factor(name, levels = bridgerton_focus_names)
  )

# Plot
bridgerton_plot <- 
  ggplot(
    bridgerton_focus,
    aes(
      x = year,
      y = rank,
      color = name,
      group = name
    )
  ) +
  geom_vline(
    xintercept = 2020,
    linetype = "dashed",
    color = "#555555",
    alpha = 0.65,
    linewidth = 0.5
  ) +
  geom_line(linewidth = 1.15, alpha = 0.95) +
  geom_point(size = 2.3, alpha = 0.95) +
  scale_color_manual(
    values = c(
      "Daphne" = "#7A5C99",
      "Eloise" = "#3E6C85",
      "Penelope" = "#C77949"
    )
  ) +
  scale_y_reverse(
    labels = scales::comma,
    expand = expansion(mult = c(0.05, 0.08))
  ) +
  scale_x_continuous(
    breaks = seq(2010, 2025, by = 5),
    minor_breaks = NULL
  ) +
  facet_wrap(~ region) +
  labs(
    title = "Bridgerton names in ranking of UK baby names",
    subtitle = "Rankings for Daphne, Eloise, and Penelope among girls' names from 2010 to 2025, with lower ranks indicating greater popularity.",
    x = NULL,
    y = "Name rank, lower is more popular",
    color = NULL,
    caption = "R. A. Jacobsen | @AulieRoy | Source: TidyTuesday week 24", 
  ) +
  theme_minimal(
    base_size = 13,
    base_family = "lato"
  ) +
  theme(
    plot.background = element_rect(fill = "#F7F3EB", color = NA),
    panel.background = element_rect(fill = "#F7F3EB", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#D9D4CB", linewidth = 0.35),
    axis.text = element_text(color = "#222222"),
    axis.title = element_text(color = "#222222"),
    plot.title = element_text(
      size = 25,
      face = "bold",
      color = "#111111",
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      size = 13,
      color = "#5F5F5F",
      margin = margin(b = 16)
    ),
    plot.caption = element_text(
      size = 9,
      color = "#6F6F6F",
      hjust = 0,
      margin = margin(t = 14)
    ),
    strip.text = element_text(
      face = "bold",
      color = "#222222",
      size = 11
    ),
    legend.position = "top",
    legend.text = element_text(
      color = "#222222",
      size = 10.5
    ),
    legend.key = element_rect(fill = "#F7F3EB", color = NA),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(20, 28, 18, 28)
  )

bridgerton_plot

