
# TidyTuesday Challenge Week 9 
# Week 9 data is about Leap Day and Comes from a Wikipedia article: https://en.wikipedia.org/wiki/February_29

# Load libraries 
library(tidyverse)
library(patchwork)

# Load dataset 
births <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/births.csv')
deaths <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/deaths.csv')

# Wrangle data 
# Create a life span distribution 

deaths <- deaths %>%
  mutate(life_span = year_death - year_birth)

# Hex code #5F2E1F Indian Yellow from Bob Ross paintings: https://sites.radford.edu/~cdc/seansevillair.html

color_indian_yellow <- c("#FFB800")

life_spans <- 
  ggplot(deaths, aes(x = life_span)) +
  geom_histogram(binwidth = 5, fill = color_indian_yellow, color = "black") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) + 
  labs(
    title = "Distribution of Life Spans", 
    x = "Life Span (Year)", 
    y = "Count" 
  ) + 
  theme_minimal() + 
  theme(
    panel.grid.minor = element_blank(), 
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 12))  ) 


# Proportion of deaths by age group
deaths_age_groups <- deaths %>%
  mutate(
    age_at_death = year_death - year_birth,
    age_group = cut(age_at_death, breaks = c(0, 30, 60, 90, Inf), labels = c("0-30", "31-60", "61-90", "91+"))
  ) %>%
  group_by(age_group) %>%
  summarize(count = n()) %>% 
  filter(!is.na(age_group))

# Colors bob ross hex codes: https://sites.radford.edu/~cdc/seansevillair.html
colors_death_age_groups <- 
  c(
  "#5F2E1F",
  "#FFB800", 
  "#0C0040",
  "#102E3C"
)

proportion_death_age_groups <- 
  ggplot(deaths_age_groups, aes(x = "", y = count, fill = age_group)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  labs(
    title = "Proportion of deaths by age group", 
    x = "", 
    y = "") + 
  scale_fill_manual(values = colors_death_age_groups) +  # Use scale_fill_manual
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 12))) 

combined_plots <- plot_layout(life_spans, proportion_death_age_groups, nrow = 2)

print(combined_plot)
