
# TidyTuesday Challenge Week 53 
# In this weeks challenge I am exploring the James Beard Awards!
# The data comes from Wikipedia. 
# For more information see: https://github.com/rfordatascience/tidytuesday/blob/main/data/2024/2024-12-31/readme.md
# Code developed by Roy Aulie Jacobsen 

# Load libraries 
library(tidyverse)
library(showtext)
library(ggtext)

# Set font 
font_add_google("Lato", "lato")
showtext_auto()

# Load dataset 
book <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-31/book.csv')

# Wrangle data 
book_winner <- 
  book %>% 
  filter(
    rank == "Winner",
    year > 2000
    ) %>% 
  group_by(year) %>% 
  count(name = "count_winners")

book_nominee <- 
  book %>% 
  filter(
    rank == "Nominee",
    year > 2000
    ) %>% 
  group_by(year) %>% 
  count(name = "count_nominee")


# Plot 
ggplot() + 
  geom_line(data = book_winner, 
            aes(x = year, y = count_winners),
            color = "darkred",
            size = 1.5
            ) + 
  geom_line(data = book_nominee, 
            aes(x = year, y = count_nominee),
            color = "darkgreen",
            size = 1.5) +
  geom_smooth(data = book_winner, 
              aes(x = year, y = count_winners, color = "Winners"), 
              method = "lm", 
              linetype = "dashed", 
              size = 1, 
              se = FALSE, 
              color = "darkred") +
  geom_smooth(data = book_nominee, 
              aes(x = year, y = count_nominee, color = "Nominees"), 
              method = "lm", 
              linetype = "dashed", 
              size = 1, 
              se = FALSE, 
              color = "darkgreen"
              ) +
  annotate("text", x = mean(range(book_winner$year)), 
           y = max(book_winner$count_winners) + 3, 
           label = "Winners", color = "darkred", size = 10, fontface = "bold") +
  annotate("text", x = mean(range(book_nominee$year)), 
           y = max(book_nominee$count_nominee) + 3, 
           label = "Nominees", color = "darkgreen", size = 10, fontface = "bold") +
  scale_x_continuous(
    breaks = seq(2000, 2025, by = 2),  
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  labs(
    title = "Trends in Book Awards From 2000 and Onwards",
    subtitle = "The number of <span style='color:darkred;'>winners</span>  has remained relatively stable, while the number of <span style='color:darkgreen;'>nominees</span>  shows an upward trend, indicating growing competition.",
    caption = "R.A.Jacobsen | @AulieRoy | Data: Wikipedia - James Beard Awards",
    x = "Year",
    y = "Number of Books" 
    ) +
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(color = "gray90", linetype = "solid"), 
    axis.text = element_text(size = 12), 
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(face = "bold", size = 20, hjust = 0), 
    plot.subtitle = element_markdown(size = 16, hjust = 0),
    plot.caption = element_text(size = 10, hjust = 0), 
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12)
    )
