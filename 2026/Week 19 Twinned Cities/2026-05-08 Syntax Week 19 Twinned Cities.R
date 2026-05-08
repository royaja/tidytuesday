
# TidyTuesday challange week 19 
# This weeks TidyTuesday challange looks into twinned cities.
# According to the description, this weeks data is about links between cities! 
# Twinned towns (also known as sister cities) are a form of legal or social agreement 
# between two geographically and politically distinct localities for the purpose of 
# promoting cultural and commercial ties. 
# The data can be found here: https://github.com/rfordatascience/tidytuesday/blob/main/data/2026/2026-05-12/readme.md
# Code is developed by R.A.Jacobsen

# load library 
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(scales)
library(showtext)
library(sysfonts)

# load data 
cities <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-12/cities.csv')

links <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-12/links.csv')

# fonts 
font_add_google("Lato", "lato")
showtext_auto()

# wrangle 

count <- 
  cities %>% 
  filter(!is.na(continent)) %>% 
  count(continent, sort = TRUE) %>% 
  mutate(
    continent = forcats::fct_reorder(continent, n),
    label = scales::comma(n)
  )

fig1 <- 
  ggplot(count, aes(x = n, y = continent, fill = continent)) +
  geom_col(width = 0.68) +
  geom_text(
    aes(label = label),
    hjust = -0.15,
    size = 4.5,
    family = "lato",
    fontface = "bold",
    color = "#222222"
  ) +
  scale_fill_manual(
    values = c(
      "Europe" = "#E3120B",
      "Asia" = "#6D8794",
      "North America" = "#8FA8A3",
      "South America" = "#A8B89A",
      "Africa" = "#BBC7AE",
      "Oceania" = "#C9BDD9"
    )
  ) +
  scale_x_continuous(
    labels = scales::comma,
    breaks = seq(0, max(count$n) + 500, by = 500),
    expand = expansion(mult = c(0, 0.12))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Europe dominates",
    subtitle = "Number of cities by continent",
    x = "Number of cities",
    y = NULL,
    caption = "R.A. Jacobsen | @AulieRoy | Source: Twinned cities"
  ) +
  theme_minimal(base_family = "lato") +
  theme(
    plot.background = element_rect(fill = "#F7F3EB", color = NA),
    panel.background = element_rect(fill = "#F7F3EB", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#D9D4CB", linewidth = 0.35),
    axis.text = element_text(size = 11, color = "#222222"),
    axis.title.x = element_text(size = 11, color = "#222222"),
    plot.title = element_text(
      size = 24,
      face = "bold",
      color = "#111111",
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      size = 13,
      color = "#5F5F5F",
      margin = margin(b = 18)
    ),
    plot.caption = element_text(
      size = 9,
      color = "#6F6F6F",
      hjust = 0,
      margin = margin(t = 14)
    ),
    legend.position = "none",
    plot.margin = margin(20, 30, 20, 30)
  )

fig1


degree <- 
  links %>% 
  pivot_longer(
    cols = c(source, target),
    names_to = "side",
    values_to = "id"
  ) %>% 
  count(id, name = "degree") %>% 
  arrange(desc(degree))

nodes <- 
  cities %>% 
  filter(
    !is.na(lng),
    !is.na(lat),
    !is.na(continent)
  ) %>% 
  left_join(degree, by = "id") %>% 
  mutate(
    degree = replace_na(degree, 0)
  )

edges <- 
  links %>% 
  left_join(
    nodes %>% 
      select(id, name, lng, lat, country, continent) %>% 
      rename(
        source = id,
        from_city = name,
        from_lng = lng,
        from_lat = lat,
        from_country = country,
        from_continent = continent
      ),
    by = "source"
  ) %>% 
  left_join(
    nodes %>% 
      select(id, name, lng, lat, country, continent) %>% 
      rename(
        target = id,
        to_city = name,
        to_lng = lng,
        to_lat = lat,
        to_country = country,
        to_continent = continent
      ),
    by = "target"
  ) %>% 
  filter(
    !is.na(from_lng),
    !is.na(from_lat),
    !is.na(to_lng),
    !is.na(to_lat)
  ) %>% 
  mutate(
    same_continent = from_continent == to_continent,
    distance = sqrt((from_lng - to_lng)^2 + (from_lat - to_lat)^2)
  ) %>% 
  filter(abs(from_lng - to_lng) < 180)


top_ids <- 
  degree %>% 
  slice_max(degree, n = 30, with_ties = FALSE) %>% 
  pull(id)

long_edges <- 
  edges %>% 
  filter(!same_continent) %>% 
  slice_max(distance, n = 350, with_ties = FALSE)

set.seed(123)

hub_edges <- 
  edges %>% 
  filter(source %in% top_ids | target %in% top_ids) %>% 
  slice_sample(n = min(350, nrow(.)))

plot_edges <- 
  bind_rows(long_edges, hub_edges) %>% 
  distinct(source, target, .keep_all = TRUE)

plot_nodes <- 
  nodes %>% 
  semi_join(
    tibble(id = c(plot_edges$source, plot_edges$target)) %>% distinct(),
    by = "id"
  ) %>% 
  filter(degree > 0)

wanted_cities <- c(
  "Mexico City",
  "Rio de Janeiro",
  "Istanbul",
  "Beijing",
  "Shanghai"
)

labels <- 
  nodes %>% 
  filter(name %in% wanted_cities)

world <- 
  rnaturalearth::ne_countries(
    scale = "medium",
    returnclass = "sf"
  )





fig2 <- 
  ggplot() +
  geom_sf(
    data = world,
    fill = "#EEE8DD",
    color = "#D6CEC1",
    linewidth = 0.18
  ) +
  geom_curve(
    data = plot_edges,
    aes(
      x = from_lng,
      y = from_lat,
      xend = to_lng,
      yend = to_lat
    ),
    curvature = 0.16,
    alpha = 0.13,
    linewidth = 0.23,
    color = "#4F5B5F"
  ) +
  geom_point(
    data = plot_nodes,
    aes(
      x = lng,
      y = lat,
      size = degree,
      color = continent
    ),
    alpha = 0.88
  ) +
  geom_text_repel(
    data = labels,
    aes(x = lng, y = lat, label = name),
    family = "lato",
    size = 3.4,
    color = "#222222",
    box.padding = 0.35,
    point.padding = 0.25,
    min.segment.length = 0,
    segment.color = "#333333",
    segment.size = 0.25,
    seed = 123
  ) +
  scale_color_manual(
    values = c(
      "Europe" = "#E3120B",
      "Asia" = "#6D8794",
      "North America" = "#D9A441",
      "South America" = "#A8B89A",
      "Africa" = "#6D9C72",
      "Oceania" = "#C9BDD9"
    )
  ) +
  scale_size_continuous(
    range = c(0.8, 5.2),
    guide = "none"
  ) +
  coord_sf(
    xlim = c(-170, 190),
    ylim = c(-58, 82),
    expand = FALSE
  ) +
  labs(
    title = "Twinned cities around the world",
    subtitle = "Connections between twinned cities around the world",
    color = NULL,
    x = NULL,
    y = NULL,
    caption = "R. A. Jacobsen | @AulieRoy | Source: Twinned Cities"
  ) +
  theme_void(base_family = "lato") +
  theme(
    plot.background = element_rect(fill = "#F7F3EB", color = NA),
    panel.background = element_rect(fill = "#F7F3EB", color = NA),
    plot.title = element_text(
      size = 25,
      face = "bold",
      color = "#111111",
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      size = 13,
      color = "#5F5F5F",
      margin = margin(b = 14)
    ),
    plot.caption = element_text(
      size = 9,
      color = "#6F6F6F",
      hjust = 0,
      margin = margin(t = 12)
    ),
    legend.position = "bottom",
    legend.text = element_text(size = 10.5, color = "#222222"),
    legend.key = element_rect(fill = "#F7F3EB", color = NA),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(18, 24, 16, 24)
  )

fig2

