library(dplyr)

#source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")
#library(calendarHeat)
library(ggalluvial)
library(data.table) # dla ITime
library(lubridate) # dla within
library(RColorBrewer)
library(calendR)

source("scripts/scripts_tymek.R", encoding="utf-8")
source("scripts/SpotifyTheme.R")

 get_number_of_songs <-  function(df){
  df %>% 
    mutate(timestamp= substr(endTime,1,10)) %>% 
    mutate(timestamp= as.Date(timestamp)) %>% 
    group_by(timestamp) %>% 
    filter(substr(timestamp,1,7)=="2022-12"| substr(timestamp,1,7)=="2022-01") %>% 
    summarise(N=n()) 
 }

 get_adhd_number <-  function(df) {
   df %>% 
     mutate(timestamp= substr(timestamp,1,10)) %>% 
     filter(substr(timestamp,1,7)=="2022-12"| substr(timestamp,1,7)=="2022-01") %>% 
     mutate(timestamp = as.Date(timestamp)) %>% 
     group_by(timestamp) %>% 
     summarise(N=n()) 
 }


#normalise data
scale_fn <- function(x) { x / sqrt(sum(x^2)) }

#Dodanie nowego argumentu okreœlaj¹cego typ wykresu, tworzenie legendy inne dla ka¿dego z nich
# type="spotify" to spotify a cokolowiek innego to adhd





