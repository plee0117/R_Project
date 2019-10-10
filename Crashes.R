library(dplyr)
library(stringr)
library(ggplot2)
library(leaflet)
library(googleVis)
library(maps)
library(shiny)
library(DT)
library(shinydashboard)
library(geojsonio)
library(rgeos)
library(rgdal)
library(sp)
library(sf)

bikepathgeo <- geojson_read('./R_Project/Bicycle_Routes.geojson',what = 'sp')
boroboundary <- geojson_read('./R_Project/Borough_Boundaries.geojson', what = 'sp')
bikepriority <- geojson_read('./R_Project/VZV_Bike_Priority_Areas.geojson', what = 'sp')
crashnona <- read.csv('./R_Project/cleancrash2.csv', stringsAsFactors = F)
AccReason <- read.csv('./R_Project/accreason.csv', stringsAsFactors = F)
Kcrash <- read.csv('./R_Project/Kcrash.csv', stringsAsFactors = F)
Mcrash <- read.csv('./R_Project/Mcrash.csv', stringsAsFactors = F)
Qcrash <- read.csv('./R_Project/Qcrash.csv', stringsAsFactors = F)
Scrash <- read.csv('./R_Project/Scrash.csv', stringsAsFactors = F)
Xcrash <- read.csv('./R_Project/Xcrash.csv', stringsAsFactors = F)

Scrash <- crashnona %>% filter(., BOROUGH == 'STATEN ISLAND')
write.csv(Scrash, file = './R_Project/Scrash.csv')


crashnona %>% filter(., TIME >"6:30" & TIME < '9:00') %>% filter(., BOROUGH == "BROOKLYN") %>% 
  ggplot(aes(x = YEAR)) + geom_bar(aes(group = DAYOFWEEK, fill = DAYOFWEEK))

crashnona %>% filter(., CONTRIBUTING.FACTOR.VEHICLE.1 != "") %>% filter(., CONTRIBUTING.FACTOR.VEHICLE.2 != "") %>% 
  filter(., CONTRIBUTING.FACTOR.VEHICLE.3 != "") %>% filter(., CONTRIBUTING.FACTOR.VEHICLE.4 != "") %>% summarise(., n())

crashnona %>% filter(., CONTRIBUTING.FACTOR.VEHICLE.3 != "") %>% filter(., CONTRIBUTING.FACTOR.VEHICLE.4 != "") %>% 
   summarise(., n())

unique(c(unique(crashnona$CONTRIBUTING.FACTOR.VEHICLE.1),unique(crashnona$CONTRIBUTING.FACTOR.VEHICLE.2),
      unique(crashnona$CONTRIBUTING.FACTOR.VEHICLE.3),unique(crashnona$CONTRIBUTING.FACTOR.VEHICLE.4),
      unique(crashnona$CONTRIBUTING.FACTOR.VEHICLE.5))) -> accdf

sort(unique(crashnona$CONTRIBUTING.FACTOR.VEHICLE.1))
as.data.frame(accdf,stringsAsFactors = F) -> acc_reason
acc_reason <- cbind(acc_reason,c(1))
colnames(acc_reason) = c('reason','category')
acc_reason %>% filter(., nchar(reason)>2) %>% arrange(., reason) -> acc_reason

crashnona$CONTRIBUTING.FACTOR.VEHICLE.1 <- 
  ifelse(crashnona$CONTRIBUTING.FACTOR.VEHICLE.1 == 'Reaction to Other Uninvolved Vehicle',
  'Reaction to Uninvolved Vehicle',crashnona$CONTRIBUTING.FACTOR.VEHICLE.1)

category = c(c(2,1,1,3,1,2,1,1,1,1),
             c(1,1,1,1,1,1,1,1,3,2),
             c(1,3,1,1,3,2,2,2,3,2),
             c(3,1,1,3,3,3,1,3,3,3),
             c(2,1,2,2,2,3,3,1,1,1),
             c(4,1,3,3,2))

acc_reason$category <- category
acc_reason[acc_reason$reason == 'Prescription Medication',]
acc_reason[38,2] = 1 

acc_reason$category <- ifelse(acc_reason$category == 1, 'Human',
                              ifelse(acc_reason$category == 2, 'Vehicle',
                                     ifelse(acc_reason$category == 3, 'Environmental','Other')))
acc_reason
write.csv(acc_reason, file = './R_Project/accreason.csv')

acc_reason %>% filter(., category == 3) %>% arrange(., category)
crashnona %>% filter(., CONTRIBUTING.FACTOR.VEHICLE.1 =='Illnesss') %>% 
  select(., CONTRIBUTING.FACTOR.VEHICLE.1)

asdfsw ='asdf'
sub(pattern = 'sd',x = asdfsw,replacement = 'ii')
asdfsw
rm(accdf)
?rbind
?leaflet
?addPolylines
class(bikepathgeo)
str(bikepathgeo)
leaflet() %>% addTiles() %>% addPolygons(data = boroboundary, weight = 1, color = 'red') %>% 
  addPolylines(data = bikepriority, weight = 1, color = 'blue') %>% 
  addPolylines(data = bikepathgeo, weight = 1, color = 'green')

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
# Identify day, month, year, day of week
crashnona %>% mutate(., MONTH = str_sub(DATE,1,2), DAY = str_sub(DATE,4,5),
                     YEAR = str_sub(DATE, 7,10))-> crashnona
crashnona$DATE = as.Date(crashnona$DATE, format = '%m/%d/%Y')
crashnona$DAYOFWEEK = weekdays(crashnona$DATE)

write.csv(crashnona, file = './R_Project/cleancrash2.csv')



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

cycdeadcrash %>% filter(., DATE > '01/01/2019')->newcycded
cycdeadcrash$DATE
newcycded$DATE
over(crashnona,boroboundary)
?sp::over
sp::over(crashnona,boroboundary)
coordinates(crashnona) <- ~LONGITUDE + LATITUDE
head(coordinates(crashnona))
class(crashnona)
class(cleansf)

clean_sp <- as(cleansf,"Spatial")

cleansf <- st_as_sf(x = crashnona, coords = ~LONGITUDE + LATITUDE, CRSobj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
over(cleansf,boroboundary)
CRS(crashnona) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
slotNames(crashnona)
crashnona@proj4string

slotNames(boroboundary)
boroboundary$polygons
dim(boroboundary)
head(boroboundary@polygons[1][[1]])
head(boroboundary[2,3:3])
name(boroboundary)

str(boroboundary)
crs(boroboundary)
cleansf <- st_as_sf(cleansf, CRS= "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
readOGR(boroboundary)
class(boroboundary)
summary(boroboundary)
?spTransform
