
# TidyTuesday Challenge 
# Week 49 
# Source: Hvitfeldt E (2022). elevators: Data Package Containing Information About Elevators in NYC. https://github.com/EmilHvitfeldt/elevators, https://emilhvitfeldt.github.io/elevators

# Load library 
library(tidyverse)
library(janitor)
library(cowplot)

# load data 
elevators <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-06/elevators.csv')

# Wrangle data 

elevators <- elevators %>% 
  janitor::clean_names() %>% # names to lower and snake case 
  janitor::remove_empty("cols") # Remove column "...27"

active_dumbwaiters <- elevators %>% 
  filter(dv_device_status_description == "ACTIVE", 
         device_type == "Dumbwaiter (D)") 

active_dumbwaiters %>% 
  group_by(borough) %>% 
  count(device_type, name = "Count") %>% 
  ggplot(aes(x = reorder(borough, Count), y = Count)) + 
  geom_bar(stat = "identity", fill= "#f68060", alpha=.6, width=.4) +
  geom_text(aes(label = Count, hjust = -0.25)) + 
  labs(
    title = "Number of active dumbwaiters within New York boroughs", 
  ) + 
  xlab("Boroughs") + 
  ylab("Count") + 
  coord_flip() + 
  cowplot::theme_cowplot()

ggsave("Active_Dumbwaiters_New_York.png")