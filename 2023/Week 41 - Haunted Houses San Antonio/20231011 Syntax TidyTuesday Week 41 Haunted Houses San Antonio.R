
# Contribution to #TidyTuesday Week 41 on Twitter (Now X)
# Data on Haunted Places in the United States by Tim Renner on data.world
# Inspiration from; 
# https://joshuamccrain.com/tutorials/maps/streets_tutorial.html
# https://github.com/nrennie/tidytuesday/blob/main/2023/2023-10-10/20231010.R


# Load packages 
library(tidyverse)
library(osmdata) 
library(sf)

# Load TidyTuesday data for week 41 - Haunted Places in the United States
haunted_places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-10/haunted_places.csv')

# Mapping out haunted places in San Antonio
san_antonio <- 
  haunted_places %>% 
  filter(city == "San Antonio")

big_streets <- getbb("San Antonio, United States") %>% 
  opq() %>% 
  add_osm_feature(key = "highway", 
                  value = c("motorway", 
                            "primary")) %>% 
  osmdata_sf()

med_streets <- getbb("San Antonio, United States")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", 
                            "tertiary")) %>%
  osmdata_sf()


# Plot map 
ggplot() +
  geom_sf(data = med_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .6) +
  coord_sf(xlim = c(-98.7, -98.3), 
           ylim = c(29.3, 29.6),
           expand = FALSE)  +
  geom_point(data = san_antonio, aes(x=longitude, 
                                     y=latitude), 
             size = 2, 
             alpha=.8, 
             fill="firebrick4", 
             color="white", 
             pch=21, 
             inherit.aes = F) +
  labs(
    title = "Haunted Houses in San Antonio",
    subtitle = "The city with second most haunted house in the United States", 
    caption = "R.A. Jacobsen | @AulieRoy | Data: Tim Renner on data.world"
  ) + 
  theme_void() + 
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    plot.caption = element_text(size = 8, hjust = 0), 
    panel.background = element_rect(fill = "#F4DFB6"),
    plot.background = element_rect(fill = "#F4DFB6"), 
  )
