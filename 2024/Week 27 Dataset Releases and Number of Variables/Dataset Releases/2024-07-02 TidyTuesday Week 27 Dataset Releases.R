
# TidyTuesday Week 27 
# This weeks tidytuesday challenge uses data from the ttmeta package which 
# automatically updates with information about the TidyTuesday datasets

# Load packages 
library(tidyverse)

# Load data 
tt_datasets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-02/tt_datasets.csv')

glimpse(tt_datasets)

# Viz showing number of datasets releases each year
tt_count <- 
  tt_datasets %>% 
  count(year, name = "count_year")

tt_count %>%
  ggplot(aes(x = year, y = count_year)) + 
  geom_col(fill = "#E3120B") + 
  labs(
    title = "Number of Dataset Releases: Annual Overview", 
    subtitle = "Data from the ttmeta Package",
    x = "Year", 
    y = "Number of Datasets", 
    caption = "R.A. Jacobsen | @AulieRoy | Source: TidyTuesday"
    ) + 
  scale_y_continuous(breaks = seq(0, max(tt_count$count_year), by = 20)) + 
  scale_x_continuous(breaks = tt_count$year) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(), 
    plot.caption = element_text(size = 8, hjust = 0),
  )
  