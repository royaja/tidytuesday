# TidyTuesday 
# Week 8 
# Data from Jared Wilber's data on Bob Ross Paintings via @frankiethull Bob Ross Colors data package

# Load library
library(tidyverse)


# Load data 
bob_ross <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv')


# Wrangle data 

bob_ross_colors_count <- bob_ross %>% 
  select(Black_Gesso:Alizarin_Crimson) %>% 
  pivot_longer(
    cols = everything(), 
    names_to = "colors",
    values_to = "count"
  ) %>% 
  group_by(colors) %>% 
  filter(count == TRUE) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))


# Plot 
bob_ross_colors_count %>% 
  ggplot(aes(x = count, y = reorder(colors, count), label = count)) +
  geom_col() +
  geom_text(hjust = -0.2) +
  labs(
    title = "Colors in Bob Ross Paintings", 
    caption = "R.A.Jacobsen | @AulieRoy | Jared Wilber's data on Bob Ross Paintings"
  ) + 
  xlab("Number of occurances") + 
  ylab("Colors") +
  theme_classic() +
  theme(
    plot.caption = element_text(hjust = -0.2)
  )




