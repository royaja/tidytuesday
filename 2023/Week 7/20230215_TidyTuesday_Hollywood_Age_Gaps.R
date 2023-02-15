
# TidyTuesday
# Date: 2023-02-15
# Week 7: This weeks data comes from Hollywood Age Gaps via Data is Plural
# By: R.A.Jacobsen

# load packages 
library(tidyverse)

# load data
age_gaps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv')

# Plot  
age_gaps %>% 
  filter(release_year > 1980 & release_year < 2010) %>% 
  ggplot() + 
  geom_jitter(aes(x = release_year, y = age_difference, color = character_1_gender)) +     
  stat_summary(aes(x = release_year, y = age_difference, color = character_1_gender),
               fun = "mean", geom = "line", size = 1.5) +
  labs(x = "Release Year", 
       y = "Age Difference", 
       color = "Gender", 
       title = "Hollywood Age Gap", 
       subtitle = "The mean age difference between actors in movies released between 1980 and 2010", 
       caption = "R.A.Jacobsen | @AulieRoy | Data is Plural") + 
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0))

# Save plot
ggsave(
  filename = "2023_02_15_tidy_tuesday_age_gaps.png",
  device = "png")
