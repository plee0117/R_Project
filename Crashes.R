library(dplyr)
library(stringr)
library(ggplot2)
library(leaflet)
library(googleVis)
library(maps)
library(shiny)

crashes <- read.csv('./R_Project/NYPD_Motor_Vehicle_Collisions_-_Crashes.csv', 
                    stringsAsFactors = F)
# get rid of weird/unknown locations and dates
crashes %>% filter(.,!is.na(LATITUDE) & LATITUDE != "") %>% 
  filter(., !is.na(LONGITUDE)& LONGITUDE != "")%>% 
  filter(., !is.na(BOROUGH) & BOROUGH != "") %>% 
  filter(., !is.na(DATE) & DATE!= "") %>% 
  filter(., !is.na(ZIP.CODE) & ZIP.CODE != "") %>%  
  filter(., LATITUDE >40.4 & LONGITUDE < -73) ->
  crashnona
crashnona %>% group_by(., BOROUGH) %>% summarise(n())

crashnona %>% summarise(., min(LATITUDE), max(LATITUDE))
crashnona %>% summarise(., min(LONGITUDE), max(LONGITUDE))
head(crashnona %>% arrange(LONGITUDE) %>% select(LONGITUDE),20)



unique(bkcrash$CONTRIBUTING.FACTOR.VEHICLE.5)

crashnona %>% filter(., BOROUGH == 'BROOKLYN') -> bkcrash
bkcrash %>% filter(., NUMBER.OF.CYCLIST.INJURED>0)-> cycinjcrash

leaflet(cycinjcrash) %>% addTiles() %>% 
  addCircles(~LONGITUDE,~LATITUDE)# some still show up in SI and MHTN

bkcrash %>% filter(., NUMBER.OF.CYCLIST.KILLED>0)-> cycdeadcrash

leaflet(cycdeadcrash) %>% addTiles() %>% 
  addCircles(~LONGITUDE,~LATITUDE, color = 'red')
