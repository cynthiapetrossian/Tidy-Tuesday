### Tidy Tuesday 2022-03-01 ####
### Created by: Cynthia Petrossian ####
### Created on: 2022-03-01 ####

### Load Libraries ####
library(tidyverse)
library(here)
library(tidytuesdayR)
library(mapdata)
library(maps)

### Read In Data ####
stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv') #loading tidy tuesday data
mapdata <- map_data("world") #loading in world map data
states <- map_data("state") #loading in state map data
counties <- map_data("county") #loading in county map data

### Data Analysis ####
California <- stations %>% #create California dataset
  filter(STATE == "CA", FUEL_TYPE_CODE == "ELEC") %>%  #filter for data from CA with fuel type Electric
  rename("region" = STATE) #rename STATE column to region
view(California)

CA <- states %>% #create California 
  filter(region == "california") #filter California from states data
CACounties <- counties %>%
  filter(region == "california") #filter out California counties from county data

ca_base <- ggplot(data = CA, mapping = aes(x = long, #create base CA map. x axis is longitude
                                           y = lat, #y axis is latitude
                                           group = group)) + #group by group
  coord_quickmap() + #sets aspect ratio for map
  geom_polygon(color = "black", fill = "gray") #adds CA border and colors state in gray
CaliforniaMap <- ca_base + geom_point(data = California, aes(x = LONGITUDE, #adding in data as points on top of base map. x is longitude
                                    y = LATITUDE, #y is latitude
                                    group = region, #group by region (California)
                                    color = FUEL_TYPE_CODE))+ #color by Fuel type (Electric)
  xlim(-125,-114)+ #set x axis limits
  ylim(32, 42.5)+ #set  axis limits
  guides(color = FALSE)+ #remove legend
  labs(title = "Electric Charging Stations in California Counties", #add title
       caption = "Source: US Department of Transportation") #add source caption
CaliforniaMap + theme_void()+ #add theme void (empty theme)
  geom_polygon(data = CACounties, fill = NA, color = "white") + #add county borders on top of map
  geom_polygon(color = "black", fill = NA) #add border back on top of map

