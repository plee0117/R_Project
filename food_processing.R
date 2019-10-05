library(dplyr)
library(stringr)

getwd()
food = readxl::read_xlsx('R_Project/ABBREV.xlsx')
food %>% mutate(., fdtype = str_sub(Shrt_Desc,1,str_locate(Shrt_Desc,',')[,1]-1)) -> foodWL
