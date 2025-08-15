
# TidyTuesday Challenge Week 25  
# This week`s` data explore Measles cases across the world
# https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-06-24/readme.md#measles-cases-across-the-world
# Code developed by R.A.Jacobsen

# Load libraries 
library(tidyverse)
library(showtext)
library(ggstream)
library(viridis)

# Load dataset 
cases_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_year.csv')

# Font 
font_add_google("Lato", "lato")
showtext_auto()


# Wrangle the data
measles <- 
  cases_year %>% 
  drop_na(measles_total)

region_stream <- 
  measles %>% 
  group_by(region, year) %>% 
  summarize(cases = sum(measles_total), .groups = "drop") %>% 
  mutate(region = fct_reorder(region, cases, .fun = sum))


# WHO policy milestones 
# IA2030: https://apps.who.int/gb/ebwha/pdf_files/WHA74/A74_9Add4-en.pdf
# Global Measles and Rubella Strategic Plan 2012-2020: https://www.afro.who.int/health-topics/measles 
# Roadmap to elimination: https://www.who.int/publications/i/item/WER9209
# Mesles prevention in Africa: https://www.cdc.gov/mmwr/volumes/72/wr/mm7236a3.htm

policy <- tibble(
  year  = c(2012, 2015, 2020, 2021),
  label = c(
    "GVAP endorsed\n(May 2012)",
    "GVAP mid-target\n(end 2015)",
    "GVAP ends\n(2020)",
    "IA2030 launches\n(Apr 2021)"
  )
)

max_cases <- max(region_stream$cases)

# Caption
caption_text <- str_wrap(
  "Launched in 2012, the Global Vaccine Action Plan (GVAP) set targets to reduce measles deaths by 95% and achieve elimination by 2015 and 2020. In April 2021, IA2030 succeeded GVAP, focusing on sustained coverage and equity in immunization.",
  width = 300
)

# Plot 
ggplot(region_stream, aes(x = year, y = cases, fill = region)) +
  geom_stream(
    type       = "mirror",
    bw         = 0.5,
    extra_span = 0,
    true_range = "none",
    color      = "grey90",
    size       = 0.2
  ) +
  geom_hline(yintercept = 0, color = "grey80", size = 0.4) +
  geom_vline(
    data        = policy,
    aes(xintercept = year),
    linetype    = "dashed",
    color       = "grey50",
    inherit.aes = FALSE
  ) +
  geom_text(
    data        = policy,
    aes(x = year, y = max_cases * 1.05, label = label),
    inherit.aes = FALSE,
    angle       = 90,
    vjust       = -0.4,
    hjust       = 0,
    size        = 3.5,
    family      = "lato"
  ) +
  scale_fill_viridis_d(
    option = "plasma",
    begin  = 0.15,
    end    = 0.85,
    name   = "WHO Region"
  ) +
  scale_y_continuous(
    labels = scales::comma_format(accuracy = 1),
    expand = expansion(mult = c(0.02, 0.12))
  ) +
  scale_x_continuous(
    breaks = seq(min(region_stream$year), max(region_stream$year), by = 2)
  ) +
  labs(
    title    = "Measles Cases by Year and WHO Region",
    subtitle = "Strengthen measles vaccination in Africa to prevent a repeat of the 2019 outbreak",
    x        = "Year",
    y        = "Number of Cases",
    caption  = caption_text
  ) +
  theme_minimal(base_family = "lato", base_size = 14) +
  theme(
    plot.title         = element_text(face = "bold", size = 18),
    plot.subtitle      = element_text(size = 12, margin = margin(b = 8)),
    axis.title         = element_text(size = 12),
    axis.text          = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "bottom",
    legend.title       = element_text(face = "bold"),
    plot.caption       = element_text(size = 10, hjust = 0),
    plot.caption.position = "plot",
    plot.margin        = margin(15, 15, 15, 15)
  )

