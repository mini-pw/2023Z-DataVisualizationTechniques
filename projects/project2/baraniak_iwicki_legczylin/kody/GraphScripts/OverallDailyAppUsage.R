library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
dane_tel_baraniak <- read_csv("Dane/Tel/dane_tel_baraniak.csv", 
                              col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
                                               Time = col_time(format = "%H:%M:%S"), 
                                               Duration = col_time(format = "%H:%M:%S")))
dane <- dane_tel_baraniak %>% 
  mutate(weekday = weekdays(Date)) %>% 
  mutate(working_day = ifelse(weekday == "Saturday" | weekday == "Sunday", 
                              FALSE, TRUE)) %>% 
  filter(`App name` != "Screen off (locked)" & 
           `App name` != "Screen on (unlocked)" & 
           `App name` != "Screen on (locked)" &
           `App name` != "Device shutdown")

overall_usage_plot <- function(weekday = NA, workingDay = NA,
                               week = NA, types = NA){
  temp <- data
  
  if(!is.na(weekday)){
    temp <- temp %>% filter(day%%7 == weekday%%7)
  }
  
  if(!is.na(workingDay)){
    if(workingDay!=TRUE){
      temp <- temp %>% filter(day%%7 == 1 | day%%7 == 0)
    }
    else{
      temp <- temp %>% filter(day%%7 != 1 & day%%7 != 0)
    }
  }
  
  if(!is.na(week)){
    temp <- temp %>% filter(day%/%7 + 1 == week)
  }
  
  if(!is.na(types)){
    temp <- temp %>% filter(type %in% types)
  }
  
  temp <- temp %>% group_by(type, hour, day) %>% 
    summarise(sumTime = sum(Duration)) %>% 
    summarise(avgTime = mean(sumTime)/60)
  
  ggplot(temp, aes(hour, avgTime, fill = type)) +
    geom_col() 
}

dane %>%
  filter(day%%7 %in% c(1,2)) %>%
  filter(day%/%7 %in% c(1,2)) %>%
  filter(AppName != "Screen off (locked)" & 
           AppName != "Screen on (unlocked)" & 
           AppName != "Screen on (locked)" &
           AppName != "Device shutdown") %>% 
  group_by(type, hour, day) %>% 
  summarise(sumTime = sum(Duration)) %>% 
  summarise(avgTime = mean(sumTime)/60) %>% 
  ggplot(aes(hour, avgTime, fill = type)) +
  geom_col() 

