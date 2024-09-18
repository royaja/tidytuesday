
# TidyTuesday Challenge Week 38. 
# Exploring dialogue in Hamlet. 
# The dataset this week comes from shakespeare.mit.edu 
# (via github.com/nrennie/shakespeare) which is the Web's first 
# edition of the Complete Works of William Shakespeare.

# Load library 
library(tidyverse)
library(showtext) 
library(quanteda)
library(quanteda.textstats)
library(patchwork)

# Load data 
hamlet <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-17/hamlet.csv')

# Wrangle data
dialogue_count <- 
  hamlet %>%
  filter(character != '[stage direction]') %>%
  group_by(character) %>%
  summarise(line_count = n()) %>%
  arrange(desc(line_count))


#  Font 
font_add_google("Roboto", "roboto")
showtext_auto()

# plot 
dialogue_count %>% 
  ggplot(aes(x = reorder(character, -line_count), y = line_count)) +
  geom_bar(stat = 'identity', fill = "orange") +  
  coord_flip() +
  geom_text(aes(label = line_count), hjust = -0.1, size = 3) +  
  labs(title = "Dialogue Frequency Across Characters", 
       x = NULL, 
       y = "Number of Lines") +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "roboto"),            
    axis.line.x = element_line(color = "black"),       
    axis.line.y = element_line(color = "black"),     
    axis.ticks.x = element_line(color = "black"),   
    axis.ticks.y = element_line(color = "black"),      
    panel.grid.minor = element_blank(),               
    panel.grid.major.y = element_blank(),              
    panel.grid.major.x = element_line(color = "gray80"),  
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    axis.text.x = element_text(size = 10),          
    axis.text.y = element_text(size = 10, hjust = 1), 
    axis.title.x = element_text(size = 12),          
    axis.title.y = element_text(size = 12)             
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Flesch-Kincaid readability score
readability_scores <- 
  hamlet %>%
  filter(character != '[stage direction]') %>%
  group_by(character) %>%
  summarise(text = paste(dialogue, collapse = " ")) %>%
  mutate(readability = textstat_readability(text, measure = "Flesch"),
         readability = readability[, "Flesch"]) %>% 
  filter(!is.na(readability))


readability_scores %>% 
  ggplot(aes(x = reorder(character, -readability), y = readability)) +
  geom_bar(stat = 'identity', fill = "orange") + 
  coord_flip() +
  geom_text(aes(label = round(readability, 1)), hjust = -0.1, size = 3) +  
  labs(title = "Readability of Dialogue for Each Character (Flesch Score)", 
       x = NULL,
       y = "Flesch Score") +
  theme_minimal(base_size = 12) + 
  theme(
    text = element_text(family = "roboto"),         
    axis.line.x = element_line(color = "black"),    
    axis.line.y = element_line(color = "black"),      
    axis.ticks.x = element_line(color = "black"),     
    axis.ticks.y = element_line(color = "black"),    
    panel.grid.minor = element_blank(),              
    panel.grid.major.y = element_blank(),           
    panel.grid.major.x = element_line(color = "gray80"), 
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
    axis.text.x = element_text(size = 10),           
    axis.text.y = element_text(size = 10, hjust = 1), 
    axis.title.x = element_text(size = 12),          
    axis.title.y = element_text(size = 12)           
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

