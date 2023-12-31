library(gifski)
library(gganimate)
library(ggthemes)
library(png)
library(tidyverse)
library(sf)

#Enter Season
hurricaneseason = 2023


#Import and Transform Data
data <- as.tibble(read.csv(header = TRUE,
                           'https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/csv/ibtracs.NA.list.v04r00.csv'))


currentyear <- data %>%
  select(SID,
         SEASON,
         NUMBER,
         BASIN,
         SUBBASIN,
         NAME,
         ISO_TIME,
         NATURE,
         LAT,
         LON,
         DIST2LAND,
         LANDFALL, 
         USA_STATUS,
         USA_WIND,
         USA_PRES,
         USA_EYE,
         USA_GUST,
         STORM_SPEED,
         STORM_DIR) %>%
  
  filter(SEASON == hurricaneseason, 
         NAME != 'NOT_NAMED')

currentyear$LAT <- as.numeric(currentyear$LAT)
currentyear$LON <- as.numeric(currentyear$LON)
currentyear$Date <- as.Date(currentyear$ISO_TIME)
currentyear$DateTime <- ymd_hms(currentyear$ISO_TIME)




#Create map
world_map = map_data("world") %>% 
  filter(! long > 180)

countries = world_map %>% 
  distinct(region) %>% 
  rowid_to_column()

countries %>% 
  ggplot(aes(fill = rowid, map_id = region)) +
  geom_map(map = world_map) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  coord_map("moll") +
  theme_map()

#Initiate Plot
p <- ggplot()+
  geom_map(map = world_map, data = world_map,aes(x = long, y = lat, map_id = region))+
  geom_line(data = currentyear, aes(x = LON, y = LAT, color = NAME, group = Date, size = 2)) +  
  coord_cartesian(xlim = c(-100,0), ylim = c(5,50)) +
  geom_text(data = currentyear, aes(label = DateTime, x = -35, y = 50, size = 100))+
  theme_bw()+ facet_wrap(~NAME)

#Create gif
anim <- p +  transition_manual(Date,cumulative = TRUE)

animate(anim, fps = 10, duration = 20, end_pause = 40,renderer = gifski_renderer(),width = 800, height = 800)
anim_save("Hurricanes.gif")
