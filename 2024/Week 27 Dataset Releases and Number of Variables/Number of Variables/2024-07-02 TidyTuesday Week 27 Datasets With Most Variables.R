
# TidyTuesday Week 27 
# This weeks tidytuesday challenge uses data from the ttmeta package which 
# automatically updates with information about the TidyTuesday datasets

# Load packages 
library(tidyverse)

# Load data 
tt_datasets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-02/tt_datasets.csv')

glimpse(tt_datasets)

# top 10 datasets in terms of number of variables 
top_10_datasets <- 
  tt_datasets %>% 
  arrange(desc(variables)) %>% 
  slice_head(n = 10)

# viz of the 10 datasets with most variables
top_10_datasets %>%
  ggplot(aes(x = reorder(dataset_name, variables), y = variables)) + 
  geom_col(fill = ifelse(rank(-top_10_datasets$variables) <= 2, "#E3120B", "gray70"), width = 0.7) +
  coord_flip() + 
  labs(
    title = "Datasets with the Most Variables (2018 to 2024)",
    subtitle = "owid energy and patient risk profile contain the most variables",
    x = "Datasets",
    y = "Number of Variables",
    caption = "R.A. Jacobsen | @AulieRoy | Source: TidyTuesday"
  ) + 
  scale_y_continuous(breaks = seq(0, max(top_10_datasets$variables), by = 20)) +
  theme_minimal() + 
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 8, hjust = 0)
  )


