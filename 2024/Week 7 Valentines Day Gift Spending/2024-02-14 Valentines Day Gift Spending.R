
# Submission for TidyTuesday week 7
# Valentines day consumer survey data: https://www.kaggle.com/datasets/infinator/happy-valentines-day-2022
# Inspiration drawn from @nrennie35

# Load library
library(tidyverse)
library(ggstream)
library(wesanderson)

# Load data
historical_spending <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/historical_spending.csv')

# Wrangling
historical_spending <-
  historical_spending %>%
  janitor::clean_names()

historical_spending_long <-
historical_spending %>%
  select(-percent_celebrating, -per_person) %>%
  pivot_longer(cols = -year,
               names_to = "gift",
               values_to = "spending")

historical_spending_long <-
  historical_spending_long %>%
  mutate(gift = str_replace_all(gift, "_", " "),
         gift = str_to_title(gift))

average_change <- historical_spending_long %>%
  group_by(gift) %>%
  summarise(average_change = (last(spending) - first(spending)) / first(spending) * 100)


# Plot
colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3", "#b2df8a")

plot <-
  historical_spending_long %>%
  ggplot(aes(x = year, y = spending, fill = gift)) +
  geom_stream(lwd = 0.25,
              n_grid = 100) +
  geom_stream_label(aes(label = gift)) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = seq(2010, 2022, by = 2)) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.caption = element_text(size = 8, color = "black", hjust = 0)
    ) +
  labs(
    x = " ",
    y = " ",
    title = "Gift Spending Over Time",
    subtitle = "Average spending has increased the most on jewlery and gift cards",
    caption = "R.A. Jacobsen | @AulieRoy | Source: Valentines day consumer data"
  ) +
  annotate("text",
           x = 2016,
           y = 90,
           label = "Average spending on jewelry\nincreased by almost 13 percent from 2010 to 2022,\nwhile spending on gift cards increased by around 5 percent.",
           size = 3,
           color = "black")

ggsave("2024-02-14 ggstream Valentines Day Gift Spending Over Time.png", plot, width = 13, height = 8, units = "in")
