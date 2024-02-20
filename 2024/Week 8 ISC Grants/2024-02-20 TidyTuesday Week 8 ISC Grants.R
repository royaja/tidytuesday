
# TidyTuesday Week 8
# This weeks TidyTuesday looks into R Consortium ISC Grants. 

# Load libraries 
library(tidyverse)
library(cowplot)

# Load data 
isc_grants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-20/isc_grants.csv')

# Wrangle data 
isc_grants |> 
  glimpse()

count_grants <- 
  isc_grants |> 
  group_by(group) |> 
  summarise(count = n()) |> 
  mutate(group = case_when(
    group == 1 ~ "Spring", 
    group == 2 ~ "Autumn"
  ))


# Plot 
count_grants_plot <- 
  count_grants |> 
  ggplot(aes(x = group, y = count)) +
  geom_bar(stat = "identity", fill = "#e5121a") +
  scale_y_continuous(limits = c(0, 55), breaks = seq(0, 55, by = 5)) +
  geom_text(aes(label = count), color = "black", size = 4, vjust = -0.5) +
  labs(
    y = "Count",
    x = "",
    title = "Number of Grants per Season"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  annotate("text", x = 0.5, y = 50, 
           label = 
           "The highest number of grants were given out during the spring, \n
            with a total of 51 grants from 2016 to 2023. In comparison, 34 \n
            grants were given out in the autumn within the same period.",
           hjust = 0, size = 4, color = "black")
