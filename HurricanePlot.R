library(gifski)
library(gganimate)
library(tidyverse)



#Import and Transform Data
data <- as.tibble(read.csv(header = TRUE,
                           'https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/csv/ibtracs.NA.list.v04r00.csv'))
#Dataset documentation
#https://www.ncei.noaa.gov/sites/default/files/2021-07/IBTrACS_v04_column_documentation.pdf


hurricaneseason = 2023

basins <- unique(data$BASIN)
subbasins <- unique(data$SUBBASIN)

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



mindate <- min((currentyear$Date))
maxdate <- max(currentyear$Date)


#Create map
world_map = map_data("world") %>% 
  filter(! long > 180)

countries = world_map %>% 
  distinct(region) %>% 
  rowid_to_column()

#Initiate Plot
p <- ggplot()+
  geom_map(map = world_map, data = world_map,aes(x = long, y = lat, map_id = region))+
  geom_path(data = currentyear, aes(x = LON, y = LAT, color = NAME), linewidth = 1)) +  
  coord_cartesian(xlim = c(-100,0), ylim = c(5,50)) +
  theme(panel.background = element_rect(fill = "#add8e6"))+
  labs(title = paste("Hurricane Season:",hurricaneseason),
       subtitle = paste(mindate, " - ", maxdate))+
  ylab("Latitude")+
  xlab("Longitude")
  
  

#Create gif
anim <- p +  transition_manual(DateTime, range = NULL, cumulative = TRUE)

animate(anim, fps = 10, duration = 30, end_pause = 40,renderer = gifski_renderer(),width = 800, height = 800)
anim_save("Hurricanes.gif")
