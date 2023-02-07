
# TidyTuesday Week 6

# Load libraries 
library(tidyverse)

# Load data
big_tech_stock_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')

# Plot
ggplot(data = big_tech_stock_prices, aes(x = date, y = open, color = stock_symbol)) +
  geom_line() +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Date", y = "Price when market opens", color = "Stock Symbol") +
  ggtitle("Opening stock prices for big tech companies") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line.y = element_blank(),
        axis.line.x = element_line(color = "black"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())