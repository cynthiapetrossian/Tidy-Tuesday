### Today we are going to be doing the 2022-02-22 Tidy Tuesday #####
### Created by: Cynthia Petrossian ####
### Creted on: 2022-02-22 ####

### Load libraries ####
library(tidyverse)
library(here)
library(tidytuesdayR)

### Read In Data ####
freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')
#Load in Tidy Tuesday data
mapdata <- map_data("world") #Load in map data for the world

### Data Analysis ####
CL_GME <- freedom %>% 
  filter(country %in% c("Bahrain", "Cyprus", "Egypt", "Iran", "Iraq", "Israel", 
                        "Jordan", "Kuwait", "Lebanon", "Oman", "Qatar", 
                        "Saudi Arabia", "Syria", "Turkey", "United Arab Emirates", 
                        "Yemen", "Djibouti", "Somalia", "Algeria", "Libya", 
                        "Morocco", "Sudan", "Tunisia", "Afghanistan", "Pakistan", 
                        "Mauritania", "Armenia", "Azerbaijan", "Georgia", 
                        "Kazakhstan", "Kyrgyzstan", "Tajikistan", "Turkmenistan", 
                        "Uzbekistan", "Palestine")) %>% #filter to only include countries in the greater middle east
  filter(year == 2020) %>% #filter for year 2020
  select("region" = country, "Civil_Liberties" = CL, year) #select country, CL, and year columns. Rename country to region and CL to Civil_Liberties
 
 
mapdata <- inner_join(mapdata, CL_GME, by = "region") #combine mapdata with CL_GME using innerjoin to only include countries filtered for

map1 <- ggplot(mapdata, aes(x = long, #use mapdata dataset, set x axis to longitude
                            y = lat, #set y axis to latitude
                            group = group))+ #uses group value in mapdata, makes sure countries fit together
  geom_polygon(aes(fill = Civil_Liberties), color = "black")+ #set geom to polygon, fill countries with Civil Liberties data, set border colors to black
  labs(title = "Civil Liberty Scores in the Greater Middle East (2020)")+ #change title
  theme(axis.text.x = element_blank(), #remove x axis text
        axis.text.y = element_blank(), #remove y axis text
        axis.ticks = element_blank(), #remove axis ticks
        axis.title.x = element_blank(), #remove x axis title
        axis.title.y = element_blank())+ #remove y axis title
  scale_fill_gradient(name = "Civil Liberties") #change name of gradient bar to Civil Liberties

print(map1) #print the map (view was not working for some reason)

ggsave(here("2022-02-22", "Outputs", "tt2022-02-22.png"), #saving plot as png
       width = 7, height = 5) #setting image dimensions as 7 x 5 inches
