
# TidyTuesday Week 44 - Horror articles 
# This code was created for participation in the TidyTuesday challenge. 
# In this weeks TidyTuesday, I look into subtitles of horror articles. 

# Load libraries 
library(tidyverse)
library(tidytext)
library(cowplot) 
library(stm)
library(ggthemes)
library(ggwordcloud)

# Load #TidyTuesday data  
horror_articles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-31/horror_articles.csv')

# Process data 
tidy_horror_articles <- 
  horror_articles |> 
  unnest(subtitle) |> 
  unnest_tokens(word, subtitle) |> 
  anti_join(get_stopwords())  
  
count_common_words <- 
  tidy_horror_articles |> 
  count(word, sort = TRUE) 

# 10 most common words occuring in subtitles 
# Creating a barplot 

figure1 <- 
  count_common_words |> 
  slice_head(n = 10) |> 
  ggplot(aes(x = reorder(word, -n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") + 
  labs(
    title = "Most frequent words occuring in subtitles", 
    x = "Words", 
    y = "Count"
  ) + 
  theme_cowplot()


# Wordcloud 

figure2 <- 
  count_common_words |> 
  slice_head(n = 90) |> 
  ggplot(aes(label = word, 
             size = n, 
             color = n)) +
  geom_text_wordcloud() +
  theme_minimal() + 
  scale_color_gradient(low = "darkred", high = "red")


# Topic modeling subtitles 

horror_articles_sparse <-
  tidy_horror_articles |> 
  count(author, word, sort = TRUE) |> 
  filter(nchar(as.character(word)) > 3) |> 
  cast_sparse(author, word, n)
  
dim(horror_articles_sparse)

set.seed(123)

topic_model <- 
  stm(horror_articles_sparse, K = 6)

summary(topic_model)

td_beta <- 
  tidy(topic_model)

td_beta

td_gamma <- 
  tidy(topic_model, matrix = "gamma",
                 document_names = rownames(horror_articles_sparse))

top_N_terms <- 4  

top_terms <- 
  td_beta |> 
  arrange(beta) |> 
  group_by(topic) |> 
  top_n(top_N_terms, beta) |> 
  arrange(-beta) |> 
  select(topic, term) |> 
  summarise(terms = list(term)) |> 
  mutate(terms = map(terms, paste, collapse = ", ")) |> 
  unnest()

gamma_terms <- 
  td_gamma |> 
  group_by(topic) |> 
  summarise(gamma = mean(gamma)) |> 
  arrange(desc(gamma)) |> 
  left_join(top_terms, by = "topic") |> 
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

figure3 <- 
  gamma_terms |> 
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = -0.02, 
            nudge_y = 0.0005, 
            size = 3,
            family = "IBMPlexSans") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.7)) + 
  theme_minimal()

ggp<- ((figure1 + figure2)/figure3) 

ggp 

