
# TidyTuesday Challenge Week 52 
# This week`s` data explore how global holidays impact seasonal human mobility. The data comes from the article "Global holiday datasets for understanding seasonal human mobility and population dynamics" by Shengjie Lai (et al) (thank you to @lgibson7 for finding the dataset).
# https://github.com/rfordatascience/tidytuesday/blob/main/data/2024/2024-12-24/readme.md
# Code developed by R.A.Jacobsen

# Load library 
library(tidyverse)
library(showtext)
library(patchwork)

# Load data
monthly_passengers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-24/monthly_passengers.csv')

# Load fonts 
font_add_google("Roboto", "roboto")
showtext_auto()


# Wrangle 
passenger_data <- 
  monthly_passengers %>%
  mutate(
    Date = as.Date(paste(Year, Month, "01", sep = "-")),  
    Month = factor(Month, levels = 1:12, labels = month.abb) 
  ) %>%
  filter(!is.na(Total_OS), 
         ISO3 == "USA")

# Trend
passenger_data %>%
  ggplot(aes(x = Date, y = Total_OS)) +
  geom_line(color = "darkred", size = 1.2) +
  labs(
    title = "Steady growth in U.S. airline passengers over time",
    subtitle = "Passenger counts show seasonality with higher peaks during holidays and summers. \nYear-over-year growth reflects increasing air travel demand in the United States.",
    x = NULL,
    y = NULL 
  ) +
  scale_x_date(
    date_breaks = "1 year",      
    date_labels = "%Y"         
  ) +
  scale_y_continuous(position = "right", name = "Total Passengers") + 
  theme_minimal(base_family = "roboto") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12), 
    axis.title.y.right = element_text(size = 14), 
    axis.text.y.right = element_text(size = 12), 
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.line = element_line(color = "black"),  
    axis.ticks = element_line(color = "black"), 
    axis.ticks.length = unit(0.25, "cm"), 
    panel.grid = element_blank()  
  )
