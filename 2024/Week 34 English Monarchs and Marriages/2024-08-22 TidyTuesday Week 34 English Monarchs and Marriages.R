# TidyTuesday Week 34 
# This weeks #TidyTuesday explore English Monarchs and Marriages. 
# Data comes from ianvisits. 
# Link to data: https://www.ianvisits.co.uk/articles/a-list-of-monarchs-by-marriage-6857/
# Code developed by Roy Aulie Jacobsen

# Load packages 
library(tidyverse)
library(showtext)

# Load data 
english_monarchs_marriages_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-20/english_monarchs_marriages_df.csv')

# Wrangle data 
glimpse(english_monarchs_marriages_df)

monarchs_df <- 
  english_monarchs_marriages_df %>%
  mutate(
    king_age = as.numeric(str_extract(king_age, "\\d+")),
    consort_age = as.numeric(str_extract(consort_age, "\\d+")),
    year_of_marriage = as.numeric(str_extract(year_of_marriage, "\\d+"))
  ) %>%
  filter(!is.na(king_age) & !is.na(consort_age) & !is.na(year_of_marriage))

# Fonts
font_add_google(name = "Roboto", family = "Roboto")
showtext_auto()

# Plot 
monarchs_df %>%
  pivot_longer(cols = c(king_age, consort_age), names_to = "role", values_to = "age") %>%
  mutate(role = ifelse(role == "king_age", "King's Age", "Consort's Age")) %>%
  filter(!is.na(age) & age != "?") %>%
  ggplot(aes(x = role, y = as.numeric(age), color = role)) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 6, fill = "white") +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, by = 10)) +
  labs(
    title = "Ages of Marriage for English Monarchs and Consorts",  
    subtitle = "Consorts Tend to Be Younger Than Monarchs at the Time of Marriage",
    x = " ",
    y = "Age of Marriage (years)",
    color = NULL, 
    caption = "R.A. Jacobsen | @AulieRoy | Source: ianvisits"
  ) +
  scale_color_manual(values = c("#FED966", "#95B8D9")) +
  theme_minimal(base_family = "Roboto") +  
  theme(
    text = element_text(size = 12),  
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  
    plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic"),
    axis.title.y = element_text(margin = margin(r = 10), size = 12), 
    plot.caption = element_text(size = 10, hjust = 0),
    axis.text = element_text(size = 12),  
    axis.line = element_line(color = "black"),  
    axis.ticks = element_line(color = "black"),  
    legend.position = "none",  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.background = element_blank(), 
    plot.margin = margin(1, 1, 1, 1, "cm")  
  )
