
# Contribution to TidyTuesday Week 6 on Twitter (Now X)
# Data on world heritage sites

# Load library 
library(tidyverse)

# Load data 
heritage <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-06/heritage.csv')

# Wrangle data 
df <- 
  heritage |>
  pivot_longer(
    cols = -country,
    names_to = "year",
    values_to = "n_sites"
  ) 

df <- 
  df |> 
  group_by(country) |> 
  mutate(total_sites = sum(n_sites))

# Plot 
df |> 
  ggplot(aes(x = country, y = n_sites, fill = year)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n_sites), position = position_stack(vjust = 0.5), color = "white", size = 4) +
  geom_text(aes(label = total_sites, y = total_sites), vjust = -0.5, size = 4, color = "black") + 
  scale_fill_manual(values = c("2004" = "#1f78b4", "2022" = "#F6423C")) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 3)) +
  labs(title = "Number of Heritage Sites by Country",
       caption = "R.A. Jacobsen | @AulieRoy | Data: World Heritage Data",
       x = " ",
       y = "Number of Heritage Sites",
       fill = "Year") +
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    plot.caption = element_text(size = 8, hjust = 0) 
  )
