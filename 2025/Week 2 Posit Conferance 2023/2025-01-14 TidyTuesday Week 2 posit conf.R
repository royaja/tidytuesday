
# TidyTuesday Challenge Week 2 2025 
# Data comes from the posit conferance and was provided by Rachael Dempsey 
# Code developed by R.A.Jacobsen 

# load library 
library(tidyverse)
library(showtext)

# font
font_add_google("Roboto", "roboto")
showtext::showtext_auto()

# load the dataset 
conf2023 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-14/conf2023.csv')

# wrangle 
session_data <- 
  conf2023 %>%
  group_by(session_type) %>%
  summarize(
    n = n(),
    representative_title = session_title[which.max(table(session_title))]
  ) %>%
  mutate(
    percentage = (n / sum(n)) * 100 
  )

# plot
session_data %>% 
  ggplot(aes(x = reorder(session_type, n), y = n, fill = session_type)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +  
  geom_text(
    aes(label = paste0(n, " (", sprintf("%.1f", percentage), "%)\n", representative_title)),
    hjust = -0.1, vjust = 0.5, size = 3.5
  ) +
  coord_flip() + 
  scale_fill_manual(values = c("keynote" = "#1f77b4", "regular" = "#ff7f0e", "lightning" = "#7f7f7f")) + 
  scale_y_continuous(limits = c(0, 190), expand = c(0, 0)) + 
  labs(
    title = "Type and Number of Sessions",
    subtitle = "Regular sessions dominate (84.5%), with lightning (11.2%) and keynote (4.3%) trailing behind.",
    x = "Type of Session", y = "Count of Sessions",
    caption = "R.A. Jacobsen | @AulieRoy | Data: Posit Conferences"
  ) +
  theme_bw(base_size = 14) + 
  theme(
    panel.grid = element_blank(),  
    plot.title = element_text(face = "bold", size = 20, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 14, margin = margin(b = 15)),
    plot.caption = element_text(size = 10, hjust = 0, margin = margin(t = 10))
  )
