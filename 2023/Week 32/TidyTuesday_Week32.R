# TidyTuesday Week 32 

# Load library 
library(tidyverse)
library(tidytext)
library(wordcloud)

# Read the CSV data
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/episodes.csv')

# Wrangle 
episodes_words <- 
  episodes |> 
  unnest_tokens(word, title) %>%
  anti_join(stop_words)

count_words <- 
  episodes_words |> 
  count(word, sort = TRUE)

# WordCloud 

wordcloud(
  words = count_words$word,
  freq = count_words$n,
  scale = c(3, 0.5),
  min.freq = 2,
  random.order = FALSE,
  rot.per = 0.2,
  use.r.layout = FALSE  
)

title("Title of episodes from Hot Ones", line = -0.5, cex.main = 1.5)
mtext("The most used words in titles are wings, eating, and spicy", side = 3, line = -2, cex = 1.2)


