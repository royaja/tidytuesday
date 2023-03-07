
# TidyTuesday Challenge 
# Week 10
# The data this week comes from the Atlas of Living Australia. Thanks to Di cook for preparing this week's dataset!

# Load libraries 
library(tidyverse)
library(rnaturalearth)

# Read in the data 
numbats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-07/numbats.csv')

# Select the latitude and longitude variables
numbat_df <- numbats %>%
  select(decimalLatitude, decimalLongitude)

# Download a map of Australia using the rnaturalearth package
aus_map <- ne_countries(country = "australia", returnclass = "sf")


cities <- data.frame(city = c("Sydney", "Melbourne", "Brisbane", "Perth", "Adelaide"),
                     latitude = c(-33.8688, -37.8136, -27.4698, -31.9535, -34.9285),
                     longitude = c(151.2093, 144.9631, 153.0251, 115.8605, 138.6007))


# Plot 
ggplot() +
  geom_sf(data = aus_map, fill = "gray90", color = "gray50") +
  geom_point(data = numbat_df, aes(x = decimalLongitude, y = decimalLatitude), size = 1, color = "darkred") +
  annotate("text", x = cities$longitude, y = cities$latitude, label = cities$city, size = 3, color = "black") +
  labs(title = "Numbats are most common in the Perth area in Australia") +
  xlab("") +
  ylab("") +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) + 
  theme_void()
