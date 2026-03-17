
# TidyTuesday challenge week 11
# This weeks TidyTuesday challenge come from the suite of Salmonid mortality 
# datasets published by the Norwegian Veterinary Institute.
# Code developed by R.A.Jacobsen 

# library 
library(tidyverse)
library(janitor)
library(scales)
library(patchwork)
library(showtext)
library(sysfonts)

# font 
font_add_google("Lato", "lato")
showtext_auto()

# Load data 
losses <- 
  read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-17/monthly_losses_data.csv"
)


# Wrangle 

salmon_losses <- 
  losses %>%
  clean_names() %>%
  mutate(
    date = as.Date(date),
    geo_group = str_to_lower(geo_group),
    species = str_to_sentence(species)
  ) %>%
  filter(
    geo_group == "country",
    species == "Salmon"
  ) %>%
  select(date, dead, discarded, escaped, other) %>%
  pivot_longer(
    cols = c(dead, discarded, escaped, other),
    names_to = "loss_type",
    values_to = "n"
  ) %>%
  mutate(
    loss_type = factor(
      loss_type,
      levels = c("dead", "discarded", "escaped", "other"),
      labels = c("Dead", "Discarded", "Escaped", "Other")
    )
  )


salmon_losses_share <- 
  salmon_losses %>%
  group_by(date) %>%
  mutate(share = n / sum(n, na.rm = TRUE)) %>%
  ungroup()


# colors  
loss_cols <- 
  c(
  "Dead" = "#d9a3a8",
  "Discarded" = "#e8cfb0",
  "Escaped" = "#a9cfc5",
  "Other" = "#b7b8d8"
)

# Theme 

theme_salmon <- 
  function() {
  theme_minimal(base_family = "lato", base_size = 13) +
    theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(size = 10, color = "grey40"),
      axis.text = element_text(color = "grey20"),
      axis.title.y = element_text(color = "grey20")
    )
}

# Plotting total losses 

ggplot(salmon_losses, aes(x = date, y = n, fill = loss_type)) +
  geom_col(width = 25, color = "white", linewidth = 0.2) +
  scale_fill_manual(values = loss_cols) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  labs(
    title = "Monthly losses of salmon in Norway",
    subtitle = "Total losses by category",
    y = "Number of salmon"
  ) +
  theme_salmon()

p1 <- 
  ggplot(salmon_losses, aes(x = date, y = n, fill = loss_type)) +
  geom_col(width = 25, color = "white", linewidth = 0.2) +
  scale_fill_manual(values = loss_cols) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  labs(
    title = "Monthly losses of salmon in Norway",
    subtitle = "Total losses by category",
    y = "Number of salmon"
  ) +
  theme_salmon()

# composition of losses

ggplot(salmon_losses_share, aes(x = date, y = share, fill = loss_type)) +
  geom_area(color = "white", linewidth = 0.2, alpha = 0.95) +
  scale_fill_manual(values = loss_cols) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  labs(
    title = "What those losses consist of",
    subtitle = "Monthly composition of reported losses",
    y = "Share of monthly losses"
  ) +
  theme_salmon()

p2 <- 
  ggplot(salmon_losses_share, aes(x = date, y = share, fill = loss_type)) +
  geom_area(color = "white", linewidth = 0.2, alpha = 0.95) +
  scale_fill_manual(values = loss_cols) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  labs(
    title = "What those losses consist of",
    subtitle = "Monthly composition of reported losses",
    y = "Share of monthly losses"
  ) +
  theme_salmon()

final_plot <- 
  p1 / p2 +
  patchwork::plot_layout(heights = c(1, 1), guides = "collect") +
  patchwork::plot_annotation(
    title = "Salmon losses in Norway over time",
    subtitle = paste(
      "Most reported salmon losses are recorded as dead fish,",
      "but the total number of losses rises and falls over time.",
      "The makeup of those losses also shifts from month to month,",
      "with smaller categories becoming more visible in some periods."
    ),
    caption = "Source: TidyTuesday Week 11 (2026) ??? Norwegian Veterinary Institute",
    theme = theme(
      plot.title = element_text(
        family = "lato",
        face = "bold",
        size = 22,
        color = "#2F2F2F"
      ),
      plot.subtitle = element_text(
        family = "lato",
        size = 12,
        color = "#5A5A5A",
        lineheight = 1.15
      ),
      plot.caption = element_text(
        family = "lato",
        size = 9.5,
        color = "#6B6B6B"
      ),
      plot.background = element_rect(fill = "#FCFAF7", color = NA)
    )
  ) &
  theme(legend.position = "top")

final_plot

