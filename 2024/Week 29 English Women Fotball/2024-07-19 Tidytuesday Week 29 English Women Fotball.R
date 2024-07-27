
# Code shows contribution to the the #TidyTuesday challenge 
# Week 29 - English Women's Football
# Source - The English Women's Football (EWF) Database, May 2024

# Load libraries 
library(tidyverse)

# Load data
ewf_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-16/ewf_matches.csv')

glimpse(ewf_matches)

# plotting the distribution of home and away scores 
ewf_matches %>%
  gather(key = "team", value = "score", home_team_score, away_team_score) %>%
  ggplot(aes(x = score, fill = team)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  geom_text(aes(label = ..count..), 
            stat = "count", position = position_dodge(width = 1), vjust = -0.5) +
  labs(title = "Distribution of Home and Away Team Scores",
       x = "Scores",
       y = "Count", 
       fill = "Team", 
       caption = "R.A. Jacobsen | @AulieRoy") +
  scale_fill_manual(values = c("home_team_score" = "#1f77b4", "away_team_score" = "#2ca02c")) + # specify colors
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(), 
    plot.caption = element_text(size = 8, hjust = 0),
    plot.title = element_text(size = 18, face = "bold")
  )


# Plot win, loss, and draw counts by team


win_loss_draw <- ewf_matches %>%
  gather(key = "result_type", value = "count", home_team_win, away_team_win, draw) %>%
  mutate(team = case_when(
    result_type == "home_team_win" ~ home_team_name,
    result_type == "away_team_win" ~ away_team_name,
    result_type == "draw" ~ home_team_name
  )) %>%
  group_by(team, result_type) %>%
  summarize(total = sum(count)) %>%
  ungroup()

# Plot win, loss, and draw counts by team
win_loss_draw %>%
  ggplot(aes(x = team, y = total, fill = result_type)) +
  geom_col(position = "dodge") +
  labs(title = "Win, Loss, and Draw Counts by Team",
       x = "Team",
       y = "Count", 
       fill = "Result") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        plot.title = element_text(size = 18, face = "bold"))


