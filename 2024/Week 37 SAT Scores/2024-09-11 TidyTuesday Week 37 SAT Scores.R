
# TidyTuesday Week 37 - SAT Scores 
# This week's TidyTuesday explores economic diversity and student outcomes.
# The dataset comes from Opportunity Insights via an article and interactive visualization 
# from the Upshot at the New York Times.
# Code developed by R.A. Jacobsen @AulieRoy

# Load libraries 
library(tidyverse)
library(extrafont)

# Load dataset 
college_admissions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-10/college_admissions.csv')

# Wrangle data 
college_admissions_summarized <- 
  college_admissions %>%
  group_by(par_income_bin, tier) %>%
  summarise(avg_sat = mean(attend_sat, na.rm = TRUE))

# Plot 
college_admissions_summarized %>%
  ggplot(aes(x = factor(par_income_bin), y = avg_sat, group = 1, color = tier)) +
  geom_line(size = 2) +   
  geom_point(size = 4) +    
  facet_wrap(~ tier) +
  labs(
    title = "How Income Shapes SAT Scores Across Different College Tiers",
    subtitle = "The plot shows that higher-income students score higher on the SAT, particularly in Ivy Plus and selective private universities.\nIn contrast, selective public universities show less variation in SAT scores across income groups. This suggests that a high-income \nbackground is more crucial for admission to more prestigious institutions.",
    x = NULL, 
    y = "Average SAT Score"
  ) +
  theme_minimal(base_size = 14) +  
  theme(
    plot.title = element_text(face = "bold", hjust = 0, size = 18, family = "Lato"), 
    plot.subtitle = element_text(size = 14, family = "Lato"), 
    axis.title.y = element_text(face = "bold", size = 14, family = "Lato"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, family = "Lato"),     
    axis.text.y = element_text(size = 12, family = "Lato"),
    panel.grid.major = element_line(color = "gray80", size = 0.5),      
    panel.grid.minor = element_blank(),                               
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),  
    panel.background = element_blank(),                               
    legend.position = "none"
  ) +
  scale_color_brewer(palette = "Set2")  
