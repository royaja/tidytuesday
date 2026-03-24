
# TidyTuesday challenge week 12
# This week's TidyTuesday challenge explores the first one million digits of pi.
# Code developed by R.A.Jacobsen

# Load library 
library(tidyverse)
library(scales)
library(showtext)
library(sysfonts)

font_add_google("Lato", "lato")
showtext_auto()

# data
pi_digits <-
  read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-24/pi_digits.csv",
    show_col_types = FALSE
  )

# make groups of digits so the plot is easier to read
bucket_size <- 5000

pi_balance <-
  pi_digits %>%
  mutate(
    bucket = ceiling(digit_position / bucket_size) * bucket_size
  ) %>%
  count(bucket, digit) %>%
  complete(bucket, digit = 0:9, fill = list(n = 0)) %>%
  group_by(digit) %>%
  arrange(bucket, .by_group = TRUE) %>%
  mutate(
    cum_n = cumsum(n),
    digits_seen = bucket,
    share = cum_n / digits_seen,
    gap = abs(share - 0.10)
  ) %>%
  ungroup()

pi_plot_data <-
  pi_balance %>%
  group_by(bucket) %>%
  summarise(
    biggest_gap = max(gap),
    .groups = "drop"
  )

first_small_gap <-
  pi_plot_data %>%
  filter(biggest_gap <= 0.001) %>%
  slice(1)

theme_pi <- function() {
  theme_minimal(base_family = "lato", base_size = 15) +
    theme(
      plot.title.position = "plot",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text = element_text(color = "grey20", size = 12),
      axis.title = element_text(color = "grey20", size = 13),
      plot.title = element_text(face = "bold", size = 20),
      plot.subtitle = element_text(size = 13, color = "grey30", lineheight = 1.15),
    )
}

ggplot(pi_plot_data, aes(x = bucket, y = biggest_gap)) +
  geom_line(
    linewidth = 1,
    color = "#4E79A7"
  ) +
  geom_vline(
    xintercept = first_small_gap$bucket,
    linetype = "dashed",
    linewidth = 0.5,
    color = "grey50"
  ) +
  annotate(
    "text",
    x = first_small_gap$bucket,
    y = max(pi_plot_data$biggest_gap) * 0.9,
    label = paste(
      "Around", comma(first_small_gap$bucket), "digits,",
      "the biggest gap drops below 0.1 percentage points"
    ),
    hjust = -0.02,
    family = "lato",
    size = 4.4,
    color = "grey25"
  ) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title = "The digits in pi get more even as you add more of them",
    subtitle = paste(
      "This plot shows the biggest gap from an even split.",
      "At the start the gap moves around quite a bit,",
      "but it gets smaller as more digits are included."
    ),
    x = "Number of digits included",
    y = "Biggest gap from an even split",
  ) +
  coord_cartesian(clip = "off") +
  theme_pi()



