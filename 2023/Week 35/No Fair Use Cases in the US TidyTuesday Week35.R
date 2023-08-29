
# TidyTuesday Week 35, 2023 
# Data from U.S. Copyright Office Fair Use Index

# Load packages 
library(tidyverse)

# Load data
fair_use_cases <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-29/fair_use_cases.csv')

# Wrangle data 
number_of_no_fair_use_cases_2000 <- 
  fair_use_cases |> 
  filter(fair_use_found == "FALSE", 
         year > 1999) |> 
  group_by(year) |> 
  summarize(total = n())

# Plot 
number_of_no_fair_use_cases_2000 |> 
  ggplot(aes(x = year, 
             y = total)) + 
  geom_line(color = "#F6423C", size = 0.8) + 
  geom_point(color = "#E3120B", size = 3) + 
  scale_y_continuous(
    sec.axis = sec_axis(~ ., name = "Count")
  ) +
  labs(
    x = "Year", 
    title = "Number of No Fair Use Cases",          
    subtitle = "There has been a growing number of No Fair Use Cases in the United States since the 2000s",  
    caption = "R.A. Jacobsen | @AulieRoy | Source: U.S. Copyright Office Fair Use Index"
  ) + 
  theme_minimal() + 
  theme(
    axis.line.x.bottom = element_line(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    axis.title.y.left = element_blank(), 
    axis.text.y.left = element_blank(),   
    axis.ticks.y.left = element_blank(), 
    plot.subtitle = element_text(face = "italic"),
    plot.caption = element_text(hjust = 0, margin = margin(0, 0, 0, 0))  # Adjust caption position
  )

