
# Load libraries
library(tidyverse)
library(MetBrewer)
library(showtext)

# Fonts
font_add_google(name = "Roboto", family = "Roboto")
showtext_auto()

# Load the dataset
worlds_fairs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-13/worlds_fairs.csv')

# Filter rows with missing (NA), zero, or less than 5 million
worlds_fairs_filtered <- 
  worlds_fairs %>%
  filter(!is.na(visitors) & visitors >= 5) %>%
  arrange(desc(visitors)) 

# Bar plot
ggplot(worlds_fairs_filtered, aes(x = reorder(name_of_exposition, visitors), y = visitors, fill = as.factor(category))) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = met.brewer("Juarez")) + 
  scale_y_continuous(breaks = seq(0, 80, by = 10), limits = c(0, 80)) + 
  theme_minimal(base_family = "Roboto") +
  labs(title = "Number of visitors across world???s fairs (??? 5 Million visitors)",
       x = " ",
       y = "Number of Visitors (in millions)",
       fill = "Category", 
       caption = "R.A. Jacobsen | @AulieRoy | Source: World's fair (Wikipedia)"
       ) +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    plot.caption = element_text(size = 8, hjust = 0),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 10, hjust = 1), 
    legend.position = "top", 
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),
    panel.grid.major.y = element_blank(), 
    panel.grid.minor = element_blank()
  ) 

