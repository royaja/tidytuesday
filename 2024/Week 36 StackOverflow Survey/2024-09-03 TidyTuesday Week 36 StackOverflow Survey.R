
# TidyTuesday Challenge Week 36. 
# Data comes from the 2024 Stack Overflow Annual Developer Surver 

# Load libraries
library(tidyverse)
library(showtext)

# Load data
stackoverflow_survey_single_response <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-03/stackoverflow_survey_single_response.csv')

# Nordic countries
nordic_countries <- c("Sweden", 
                      "Norway", 
                      "Denmark", 
                      "Finland", 
                      "Iceland")
# Wrangle data 

# Filter Nordic countries and remove NA in ai_sent
nordic_responses <- 
  stackoverflow_survey_single_response %>% 
  filter(country %in% nordic_countries, !is.na(ai_sent))

# Recode and merge categories in ai_sent
nordic_responses <- 
  nordic_responses %>%
  mutate(ai_sent_simplified = case_when(
    ai_sent %in% c(1, 5) ~ "Favorable",
    ai_sent %in% c(3, 6) ~ "Unfavorable",
    ai_sent == 2 ~ "Indifferent",
    ai_sent == 4 ~ "Unsure"
  ))

# Count by country and AI sentiment
country_ai_sentiment <- 
  nordic_responses %>%
  group_by(country, ai_sent_simplified) %>%
  summarise(count = n()) %>%
  ungroup()

# Calculate percentages
country_ai_sentiment <- 
  country_ai_sentiment %>%
  group_by(country) %>%
  mutate(percentage = count / sum(count) * 100)

# Plot 

# Add Google Font "Lora"
font_add_google("Lora", "lora")
showtext_auto()

color_palette <- c(
  'Favorable' = '#B3E2CD',
  'Indifferent' = '#FDCDAC',
  'Unfavorable' = '#CBD5E8',
  'Unsure' = '#F4CAE4'
)

background_color <- "#f0f0f0"

title = "Sentiments Toward Use of AI in the Nordic Countries "
subtitle = "Iceland, Norway, and Sweden are three countries with most favorable sentiments toward use of AI in the developer workflow. \nDenmark have the largest percentage of developers answering that they have unfavorable sentiments toward the use of AI in the \ndevelopment workflow."
caption = "R.A.Jacobsen | @AulieRoy | Data: 2024 Stack Overflow Annual Developer Survey"

country_ai_sentiment %>% 
  ggplot(aes(x = country, y = percentage, fill = ai_sent_simplified)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_fill(vjust = 0.5), 
            size = 5, color = "black") +  
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = " ",
    y = NULL,  
    fill = NULL
  ) +
  scale_fill_manual(values = color_palette) +
  theme_minimal(base_size = 16) +  
  theme(
    plot.background = element_rect(fill = background_color, color = NA),  
    panel.background = element_rect(fill = background_color, color = NA),  
    plot.title = element_text(size = 24, hjust = 0, face = "bold", margin = margin(b = 10), color = "black", family = "lora"),
    plot.subtitle = element_text(size = 18, hjust = 0, margin = margin(b = 10), color = "black", family = "lora"),
    plot.caption = element_text(size = 12, hjust = 0, margin = margin(t = 20), color = "black", family = "lora"),
    legend.position = "top",
    legend.text = element_text(size = 14, family = "lora"),  
    legend.background = element_rect(fill = background_color, color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(), 
    axis.text.y = element_blank(),  
    axis.text.x = element_text(size = 16, face = "bold", color = "black", family = "lora")  
  )