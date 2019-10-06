library(dplyr)
library(stringr)
getwd()
listoffiles <- list.files(path = './R_Project/FoodData_Central_csv_2019-04-02/', pattern = '*.csv', full.names = T)
listoffiles[1:3]
for (idx in 1:3) {
  make.names(str_sub(listoffiles[idx],45,-5)) <- read.csv(listoffiles[idx], stringsAsFactors = F)
}
?make.names
food_category = read.csv('food_category.csv', stringsAsFactors = F)
food = read.csv('food.csv', stringsAsFactors = F)

food$Shrt_Desc = str_to_upper(food$Shrt_Desc)

fdtyper = function(desc){
  if(is.na(str_locate(desc,',')[,1])){
    return(desc)
  } else if (str_locate(desc,',')[,1] == 1){
      return(str_sub(desc,2,nchar(desc)))
  } else{
    return(str_sub(desc,1,str_locate(desc,',')[,1]-1))
  }
} 

test = c("asdf,as",",as","asdf")
test2 = c("as","a,s")
fdtyper('as,df')
fdtyper(test)
fdtyper(test2)
as.vector(sapply(test, fdtyper))
class(result)
str(result)
is.numeric(str_locate(food$Shrt_Desc,',')[,1])
result
str_locate(test,',')
fdtyper(',as')

food %>% mutate(., fdtype = as.vector(sapply(food$Shrt_Desc,fdtyper))) -> foodWL

food %>% mutate(., fdtype = fdtyper(Shrt_Desc)) -> foodWL
foodWL %>% group_by(., fdtype) %>% select(., fdtype) ->typeoffood
as.vector(unique(typeoffood)) -> unifoods
class(unifoods)
head(unifoods[[1]],200)
foodWL[56,]
rm(unique_foods)
