

# Load packages
library(tidytext)
library(ggwordcloud)

# Load data
grants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-03/grants.csv')

# Data wrangling
titles <- 
  grants %>% 
  select(opportunity_title) %>% 
  unnest_tokens(word, opportunity_title) %>% 
  anti_join(get_stopwords(language = "english", source = "snowball"))

titles_p <- 
  titles %>% 
  filter(nchar(as.character(word)) > 3) %>% 
  filter(!grepl("\\d", word)) %>% 
  count(word, sort = TRUE) %>% 
  ungroup()

top_50_words <- 
  titles_p |> 
  slice_head(n = 50)

# Plot
ggplot(top_50_words, aes(label = word, size = n, color = factor(rank(desc(n)) <= 5))) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 15) +
  scale_color_manual(values = c("white", "yellow")) +
  labs(
    title = "Top 50 words that occur in grant opportunity titles", 
    caption = "R.A. Jacobsen | @AulieRoy | Source: US Government Grant Opportunities"
  ) + 
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#001F3F", color = "#001F3F"),
    plot.background = element_rect(fill = "#001F3F", color = "#001F3F"),
    text = element_text(color = "white", size = 15),  # Adjust text size as needed
    plot.title = element_text(color = "white", size = 18, face = "bold"),  # Adjust title size as needed
    plot.caption = element_text(color = "white", size = 8, hjust = 0)
  ) 

