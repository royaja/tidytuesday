
# TidyTuesday Week 45. 
# This weeks data is about Democracy and Dictatorship. 
# Dataset currated by Jon Harmon. 

# Code was developed by R.A.Jacobsen. @AulieRoy. 

# Load libraries  
library(tidyverse)
library(zoo)
library(showtext)

# Load dataset 
democracy_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-05/democracy_data.csv')

# Font
font_add_google("Lora", "lora")
showtext_auto()

# Wrangle data 
democracy_data <- 
  democracy_data %>% 
  filter(!is.na(is_democracy) & !is.na(has_free_and_fair_election) & !is.na(is_multiparty)) %>% 
  group_by(year) %>% 
  summarise(
    democracy_proportion = mean(is_democracy, na.rm = TRUE), 
    free_fair_election_proportion = mean(has_free_and_fair_election, na.rm = TRUE), 
    multiparty_proportion = mean(is_multiparty, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(democracy_proportion, free_fair_election_proportion, multiparty_proportion), 
               names_to = "Metric", values_to = "Proportion") %>%
  mutate(
    moving_average = rollmean(Proportion, k = 5, fill = NA, align = "center")
  )

# Plot
democracy_data %>% 
  ggplot(aes(x = year, y = Proportion, color = Metric)) +
  geom_line(size = 1.8) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", size = 1, alpha = 0.6) +
  labs(
    title = "Exploring Developments in Free & Fair Elections, Democracy, and Multiparty Systems Over Time",
    x = "Year",
    y = "Percentage of Countries",
    caption = "R.A.Jacobsen | @AulieRoy | Data: Democracy and Dictatorship",
    color = "Indicators",
    linetype = "Trend Type"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = c("democracy_proportion" = "#4B0082", 
                                "free_fair_election_proportion" = "#008B8B", 
                                "multiparty_proportion" = "#FFD700"),
                     labels = c("Democracy", "Free & Fair Elections", "Multiparty System")) +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "lora"),
    panel.background = element_rect(fill = "#F5FFF6", color = NA),
    panel.grid.major = element_line(color = "gray", size = 0.1),
    panel.grid.minor = element_blank(),
    axis.text = element_text(face = "bold", color = "black", size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.title = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 8)),
    plot.caption = element_text(size = 8, hjust = 0), 
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    legend.key.height = unit(8, "pt"),
    legend.box.spacing = unit(3, "pt")
  )

