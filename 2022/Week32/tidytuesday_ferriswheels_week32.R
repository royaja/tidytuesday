
# TidyTuesday Week 32 
# Top 10 heightst ferris wheels in the USA

-# Load libraries ----

library(tidyverse)
library(tidytuesdayR)

-# Load data ----
wheels <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv')

-# Wrangle data ---- 

ferriswheels <- wheels %>% 
  filter(is.na(closed)) %>% 
  filter(country == "USA") %>% 
  arrange(desc(height)) %>% 
  slice_max(height, n = 10)

-# Plot ----

ggplot(ferriswheels) + 
  geom_col(aes(x = height, y = reorder(name, height)), fill = "#0099f9") + 
  geom_text(aes(x = height, y =name, label = height), hjust = -0.5) +
  scale_x_continuous(limits = c(0, 800)) + 
  theme_classic() + 
  labs(
    title = "Top 10 heighest ferris wheels in the USA", 
    caption = "Roy Aulie Jacobsen | @AulieRoy | TidyTuesday week 32"
  ) + 
  ylab("Ferris wheel") + 
  xlab("Height (in feet)") +
  theme(
    plot.title = element_text(size = 30, hjust = 0, face = "bold"),
    plot.caption = element_text(size = 10, hjust=0)
    ) 

ggsave("top_10_heighest_ferris_wheels_us.png")