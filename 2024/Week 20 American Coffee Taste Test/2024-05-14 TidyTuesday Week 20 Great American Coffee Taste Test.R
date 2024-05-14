
# TidyTuesday Week 20 
# The Great American Coffee Taste Test

# Load packages 
library(tidyverse)
library(viridis)

# Load dataset 
coffee_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-14/coffee_survey.csv')

# Filter missing values 
coffee_data_filtered <- 
  coffee_data %>% 
  filter(!is.na(brew))

# Calculate frequency of each brewing method
coffee_data_frequency <- 
  coffee_data_filtered %>% 
  group_by(brew) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))


# Exclude 'Other' category and keep only top 10 brewing methods
coffee_data_top10 <- 
  coffee_data_frequency %>% 
  filter(brew != "Other") %>% 
  slice_head(n = 10)

# Create a bar plot for the top 10 brew methods
top10_plot <- coffee_data_top10 %>%
  ggplot(aes(x = reorder(brew, n), y = n, fill = brew)) +  
  geom_bar(stat = "identity", color = "black") +
  scale_fill_viridis_d() + 
  labs(title = "Top 10 Brewing Methods at Home",
       x = "Brewing Method",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  
        axis.text.y = element_text(size = 10), 
        axis.title = element_text(size = 12),
        panel.grid.major = element_line(color = "gray", linetype = "dashed"), 
        panel.grid.minor = element_blank(), 
        legend.position = "none") 

# Show the plot
print(top10_plot)

# Show the plot
print(top10_plot)
