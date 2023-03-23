
# TidyTuesday Week 12 
# The data this week comes from the Programming Language DataBase 

# Load library 
library(tidyverse)
library(cowplot)

# Load data 
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-21/languages.csv')

# Wrangle data 
qlpl <- 
  languages %>% 
  filter(
    type == "pl" | 
      type == "queryLanguage"
  )

qlpl_top200 <- 
  qlpl %>% 
  arrange(desc(number_of_users)) %>% 
  slice_head(n = 200)

mean <- 
  qlpl_top200 %>% 
  mutate(mean = mean(book_count))

# Text 
title <- "Number of books"
subtitle <- "This plot shows a development in the number of books published on programming and querylanguages \nwith the number of books published on queryLangauges being above the mean until the 2000s"
captation <- "R.A.Jacobsen | @AulieRoy | Data: Programming Language DataBase"
x = "Year"
y = "Count"

# Plot 
qlpl_top200 %>% 
  ggplot() + 
  geom_line(aes(x = appeared, y = book_count)) + 
  geom_hline(yintercept = mean(mean$mean), color="black") +
  scale_x_continuous(limits = c(1950, NA)) + 
  facet_wrap(. ~ type) + 
  labs(
    title = title, 
    subtitle = subtitle, 
    caption = captation, 
    x = x, 
    y = y
  ) +
  theme_cowplot() + 
  theme(
    plot.title = element_text(size = 24, face = "bold"), 
    plot.caption = element_text(size = 6),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  ) 
