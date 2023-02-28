
# TidyTuesday
# Week 9 
# The data this week comes from AfriSenti: Sentiment Analysis dataset for 14 African languages 
# via @shmuhammad2004 (the corresponding author on the associated paper, and an active member of the R4DS Online Learning Community Slack).

# Load data 
library(tidyverse)
library(ggchicklet)
library(hrbrthemes)

# Load the data 
afrisenti <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/afrisenti.csv')
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/languages.csv')
language_scripts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_scripts.csv')
language_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_countries.csv')
country_regions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/country_regions.csv')

# Wrangle data 

afrisenti <- left_join(afrisenti, languages, by = "language_iso_code")

df <- afrisenti %>% 
  group_by(language, label) %>%
  summarise(count = n()) %>% 
  mutate(percentage = count/sum(count)*100) 

# Plot

colors <- c("#FF616D", "#F9A74B", "#60C5BA")

df %>% 
  ggplot(aes(x = language, y = percentage, fill = label)) + 
  geom_chicklet(radius = grid::unit(3, "mm")) +
  scale_fill_manual(values = colors, guide = guide_legend(label.position = "top", label.theme = element_text(size = 12))) +
  coord_flip() +
  labs(
    title = "Sentiments of Tweets Across African Languages",
    subtitle = "This graph shows the distribution of sentiments across African languages on Twitter",
    caption = "R.A.Jacobsen | @AulieRoy | Data from AfriSenti",
    fill = NULL,
    x = "Language", 
    y = "Percentage"
  ) +
  theme_ipsum_rc(grid="X") +
  theme(axis.text.x = element_text(color = "gray60", size = 10),
        legend.position = "top",
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 8))
