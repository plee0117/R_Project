library(dplyr)
library(stringr)
library(ggplot2)
library(leaflet)


crashes <- read.csv('./R_Project/NYPD_Motor_Vehicle_Collisions_-_Crashes.csv', stringsAsFactors = F)

crashes %>% filter(., BOROUGH == 'BROOKLYN') %>% filter(.,!is.na(LATITUDE)) %>% 
  filter(., !is.na(LONGITUDE)) -> bkcrash
bkcrash %>% filter(., NUMBER.OF.CYCLIST.INJURED>0)-> cycinjcrash

leaflet(cycinjcrash) %>% addTiles() %>% 
  addCircles(~LONGITUDE,~LATITUDE)

bkcrash %>% filter(., NUMBER.OF.CYCLIST.KILLED>0)-> cycdeadcrash

leaflet(cycdeadcrash) %>% addTiles() %>% 
  addCircles(~LONGITUDE,~LATITUDE, color = 'red')
