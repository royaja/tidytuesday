
# TidyTuesday Challenge - Creating a plot that explore active substance in epilepsy medicine
# Week 11 
# The data this week comes from the European Medicines Agency via Miquel Anglada Girotto on GitHub. 

# Load library
library(tidyverse)
library(ggsvg)

# Load data 
drugs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-14/drugs.csv')

# Wrangle 
epilepsy <- drugs %>% 
  filter(str_detect(therapeutic_area, "Epilepsy"))

epilepsy_df <- epilepsy %>% 
  select(therapeutic_area, active_substance) %>% 
  mutate(active_substance = str_to_sentence(active_substance))

epilepsy_df <- epilepsy_df %>% 
  group_by(therapeutic_area, active_substance) %>% 
  mutate(drug_number = row_number()) %>% 
  ungroup()

# drug capsule
pills <- "https://www.svgrepo.com/show/321217/pill.svg"
svg_txt <- paste(readLines(pills), collapse = "\n")

# Text 
title <- "Most Common Active Substances in Drugs Against Epilepsy"
subtitle <- "There are 11 active substances used to treat epilepsy in the dataset, with Levetiracetam and Pregabalin being the most common \nwith some of these substances also being present in drugs that thread anxiety and neuralgia"
caption <- "R.A.Jacobsen | @AulieRoy | Data: European Medicines Agency"

# Plot 
epilepsy_df %>%
  ggplot() +
  geom_point_svg(aes(drug_number, active_substance), svg = svg_txt, size = 18) +
  xlim(0.9, 10) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption
  ) + 
  theme_classic() +
  xlab("") + 
  ylab("") +
  theme(
    panel.grid = element_blank(), 
    axis.text.x = element_blank(),
    axis.line = element_blank(),
    axis.ticks.x = element_blank(),
    panel.background = element_rect(fill = "#F0F0F0"),
    plot.background = element_rect(fill = "#F0F0F0"),
    plot.title = element_text(size = 24, face = "bold") 
  )
