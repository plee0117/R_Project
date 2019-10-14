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
library(shinythemes)
library(RColorBrewer)
library(tidyr)
library(data.table)

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



# above into global
timeline = data.frame(Year = 2012:2019, 
                      Events = c(NA,"Citi Bike","Vision Zero",rep(NA,times = 5)))

bikelanes <- st_read('./R_Project/extras/20190110__NYCDOT_BICYCLE_NETWORK/NYCDOT_BICYCLE_NETWORK_current_.shp')
bikelanes %>% 
leaflet() %>% addTiles() %>% addPolygons()

slotNames(bikelanes)
class(bikelanes)

Xcrash <- Xcrash %>% select(., -ATH, -ATV, -ATE)
write.csv(AccReason, file = './R_Project/accreason.csv', row.names = F)

AccReason$category <- ifelse(AccReason$category == 'Vehicle', 'Vehicular',AccReason$category)


pickboro<- function(boroname){
  switch (boroname,
          'Bronx' = Xcrash,
          'Brooklyn' = Kcrash,
          'Manhattan' = Mcrash,
          'Queens' = Qcrash,
          'Staten Island' = Scrash)
}

Kcrash %>% filter(., CONTRIBUTING.FACTOR.VEHICLE.1 !='1' &
                     CONTRIBUTING.FACTOR.VEHICLE.2 !='1' &
                     CONTRIBUTING.FACTOR.VEHICLE.3 !='1' & 
                     CONTRIBUTING.FACTOR.VEHICLE.4 !='1' &
                     CONTRIBUTING.FACTOR.VEHICLE.5 !='1') ->
  Kcrash
Kcrash -> Kcrashtest
Xcrash$ATE <- ifelse(Xcrash$ATE == 'ATE', 'N', Xcrash$ATE)

Kcrash %>%  
  pivot_longer(., cols =  c(CONTRIBUTING.FACTOR.VEHICLE.1, 
                            CONTRIBUTING.FACTOR.VEHICLE.2,
                            CONTRIBUTING.FACTOR.VEHICLE.3, 
                            CONTRIBUTING.FACTOR.VEHICLE.4, 
                            CONTRIBUTING.FACTOR.VEHICLE.5), 
               names_to = 'AccFactor', values_to = 'AccFactorVal') %>% 
  filter(., AccFactorVal != '') %>% filter(., AccFactorVal != 'Unspecified') %>% 
  inner_join(., AccReason, by = c('AccFactorVal' = 'reason')) %>% 
  # group_by(., AccFactorVal) %>% 
  # summarise(., No_Accidents = n()) %>% arrange(., desc(No_Accidents)) %>% 
  group_by(category) %>% 
  summarise( total = sum(n())) -> asdf

asdf %>% summarise(sum(No_Accidents))[[1]][1]
gvisColumnChart(data = asdf, xvar = 'AccFactorVal', yvar = 'No_Accidents', 
)->qwer
plot(qwer)


gvisPieChart(
  data = asdf, labelvar = "category", numvar = "total"
) -> qwer

Kcrash %>% 
  group_by(.,YEAR) %>% 
  summarise(., Total = n()) -> totalcrash
Kcrash %>% 
  group_by(.,YEAR) %>% 
  summarise(., Total = n()) -> totalcrash
Kcrash  %>% filter(., NUMBER.OF.PEDESTRIANS.INJURED >0 |
                        NUMBER.OF.PEDESTRIANS.INJURED > 0) %>% 
  group_by(., YEAR) %>% 
  summarise(., Pedestrians = n()) -> pedcrash
Kcrash  %>% filter(., NUMBER.OF.CYCLIST.INJURED >0 |
                        NUMBER.OF.CYCLIST.INJURED > 0) %>% 
  group_by(., YEAR) %>% 
  summarise(., Cyclist = n()) -> cyccrash
linedata <- inner_join(totalcrash, pedcrash, by = 'YEAR') %>%
  inner_join(., cyccrash, by = 'YEAR')




?gvisColumnChart
  ggplot(aes(x = AccFactorVal))+ geom_bar(stat = 'identity',aes(y = No_Acc))

?geom_bar
  group_by(., category, AccFactorVal) %>% 
  ggplot(aes(x = category)) + geom_bar(aes(group = AccFactorVal), position = 'dodge')


AccReason$reason
#->
 asdf %>% filter(., AccTypeVal != "") %>% summarise(n())
 asdf %>% filter(., AccFactorVal != "") %>% summarise(n())
 head(asdf %>% filter(., AccFactorVal == "" & AccTypeVal != "") %>% 
        select(., AccFactorVal, AccTypeVal),1) #%>% summarise(n())

asdf %>% anti_join(., AccReason, by =c("AccFactorVal" = "reason")) %>% 
  select(., AccFactorVal) %>% summarise(., n())

  qwer

qwer
?anti_join


  
  
  
    group_by(AccType) %>% arrange(AccType)

?complete.cases

AccReason
AccFactorColumn = c(CONTRIBUTING.FACTOR.VEHICLE.1,
                      CONTRIBUTING.FACTOR.VEHICLE.2,
                      CONTRIBUTING.FACTOR.VEHICLE.3,
                      CONTRIBUTING.FACTOR.VEHICLE.4,
                      CONTRIBUTING.FACTOR.VEHICLE.5)
Scrash %>% 
  filter(., YEAR == '2019') %>% 
  filter(., NUMBER.OF.CYCLIST.INJURED > 0 |NUMBER.OF.CYCLIST.KILLED > 0 | 
           NUMBER.OF.PEDESTRIANS.INJURED > 0 | NUMBER.OF.PEDESTRIANS.KILLED > 0) %>% 
  spread()


?pivot_wider




colnames(Xcrash)
Scrashtest$ATE <- ifelse(
  Scrashtest$CONTRIBUTING.FACTOR.VEHICLE.1 %in% 
    AccReason[AccReason$category == 'Environmental','reason'],'Y',
  Scrashtest$ATE
)

head(Xcrashtest %>% filter(., ATH =='Y' | ATV == 'Y' | ATE == 'Y') %>%  
       select(., starts_with('AT')),50)


Xcrashtest %>% filter(., ATH == 'Y') %>% summarise(n())

AccReason[AccReason$category == 'Environmental','reason']

for (idx in c(1:nrow(Xcrashtest))) {
  if (Xcrashtest$CONTRIBUTING.FACTOR.VEHICLE.1[idx] %in% 
      AccReason[AccReason$category == 'Human','reason']) {
    Xcrashtest$ATH[idx] = 'Y'
  }
}
head(Xcrashtest)
Xcrashtest %>% rename(.,ATH = "ATH")

rm(idx)



colnames(Xcrash)

Xcrash %>% filter(., BOROUGH != 'BRONX')
Scrash <- crashnona %>% filter(., BOROUGH == 'STATEN ISLAND')
write.csv(Qcrashtest, file = './R_Project/Qcrash.csv')

Kcrash %>% left_join(., AccReason, 
                      by = c('CONTRIBUTING.FACTOR.VEHICLE.1' = 'reason')) %>% 
  group_by(., YEAR, category) %>% ggplot(.,aes(x = YEAR)) + 
  geom_bar(aes(group = category, fill = category), position = 'fill')

Kcrash %>% group_by(., YEAR, CONTRIBUTING.FACTOR.VEHICLE.1) %>% 
  summarise(., n())

Kcrash %>% filter(., CONTRIBUTING.FACTOR.VEHICLE.1 != 'Unspecified' &
                    CONTRIBUTING.FACTOR.VEHICLE.2 != 'Unspecified'  &
                  CONTRIBUTING.FACTOR.VEHICLE.3 != 'Unspecified' ) %>% 
  group_by(., YEAR) %>% summarise(.,n())

str(Kcrash %>% filter(., CONTRIBUTING.FACTOR.VEHICLE.1 == 'Unspecified') %>% 
  group_by(., YEAR) %>% summarise(.,n()))
Kcrash %>% filter(., YEAR == '2019') %>% summarise(.,n()) -> asdf
class(asdf)
asdf[1,1]




boro2 = 'Bronx'
year2019 = 2019
switch (boro2,
        'Bronx' = Xcrash,
        'Brooklyn' = Kcrash,
        'Manhattan' = Mcrash,
        'Queens' = Qcrash,
        'Staten Island' = Scrash) %>% filter(., YEAR == year2019)

Kcrash %>% filter(., NUMBER.OF.CYCLIST.INJURED == 0 &
                    NUMBER.OF.CYCLIST.KILLED >0) %>% summarise(.,n())
colnames(Kcrash)
colnames(AccReason)
AccReason$reason
?inner_join
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

plot(gvisGeoChart(data = boroboundary))

asdf = c(letters[1:4])
qwer =         c(1,3,2,4)
zxc = rbind(asdf,qwer)
zxc %>% filter(a == 1) %>% pivot_longer(.,c('b','c','d'),names_to = "newcolumn",values_to = "values")-> asdf
asdf
asdf[values == max(values),newcolumn]

?pivot_longer



colnames(zxc)=asdf
as.data.frame(zxc)->zxc

zxc
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
leaflet() %>% addTiles() %>% 
  addPolygons(data = boroboundary[boroboundary@data$boro_name == 'Bronx'])
head(boroboundary[boroboundary@data$boro_name == 'Bronx']$coords)
slot(object = boroboundary,name = 'data')

boroboundary@polygons[boroboundary@data$boro_name[]=='Bronx']


colnames(boroboundary)
class(boroboundary@polygons)

?is.element
leaflet() %>% addTiles() %>% addPolygons(data = boroboundary, layerId = LETTERS[1:5]) %>% 
  removeShape(layerId = LETTERS[1:4])

leaflet() %>% addTiles() %>% addPolygons(data = bikepriority$boro_cd_cod['BK 12'])



zipcodes <- st_read('./R_Project/ZIP_CODE_040114.shp')

?readOGR
dim(boroboundary)
head(boroboundary@polygons[1][[1]])
head(boroboundary[2,3:3])
name(boroboundary)
slotNames(boroboundary)
boroboundary$boro_name


str(boroboundary)
crs(boroboundary)
cleansf <- st_as_sf(cleansf, CRS= "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
readOGR(boroboundary)
class(boroboundary)
summary(boroboundary)
?spTransform


head(Kcrash)
head(boroboundary)
