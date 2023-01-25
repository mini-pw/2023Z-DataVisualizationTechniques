library("rjson")
library("dplyr")
library("lubridate")
library("stringr")
library("ggplot2")
library("shiny")
library("tidyr")



decCsv <- as.data.frame(read.csv(file = "raw_data/december_data_w", encoding = "UTF-8"))
janCsv <- as.data.frame(read.csv(file = "raw_data/january_data_w", encoding = "UTF-8"))

decCsv <- as.data.frame(read.csv(file = "raw_data/december_data_t", encoding = "UTF-8"))
janCsv <- as.data.frame(read.csv(file = "raw_data/january_data_t", encoding = "UTF-8"))
# 
decCsv <- as.data.frame(read.csv(file = "raw_data/december_data_c", encoding = "UTF-8"))
janCsv <- as.data.frame(read.csv(file = "raw_data/january_data_c", encoding = "UTF-8"))

###
#To dodał Czarek
#jak to zaaplikujesz na moich ramkach danych to powinno działać wyszukiwanie mojego domu :)
janCsv <- janCsv %>% 
  mutate(placeVisit_location_name = case_when(
    (placeVisit_location_name == "" & placeVisit_location_address != "") ~ placeVisit_location_address,
    TRUE ~ placeVisit_location_name
  ))
###

filterDecCsv <- decCsv %>% 
  filter(placeVisit_location_name != "")

filterJanCsv <- janCsv %>% 
  filter(placeVisit_location_name != "")

filterData<-bind_rows(filterDecCsv, filterJanCsv) %>% 
  select(placeVisit_duration_startTimestamp, placeVisit_duration_endTimestamp, placeVisit_location_name) %>% 
  mutate(placeVisit_location_name = tolower(placeVisit_location_name))
names(filterData) <- c("timeStart", "timeEnd", "name")
x <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-=" 
filterData$name <- gsub("[[:punct:]]", " ", filterData$name) 

filterData$timeStart <- paste(substring(filterData$timeStart, 1, 10), substring(filterData$timeStart, 12, 16))
filterData$timeEnd <- paste(substring(filterData$timeEnd, 1, 10), substring(filterData$timeEnd, 12, 16))

filterData$timeStart <- strptime(filterData$timeStart, '%Y-%m-%d %H:%M')
filterData$timeEnd <- strptime(filterData$timeEnd, '%Y-%m-%d %H:%M')
filterData <- filterData %>% 
  mutate(time_diff = as.integer(difftime(timeEnd, timeStart, units = "days"))) %>% 
  filter(time_diff==0)
#View(diffDay)
#View(filterData)
#cos tu nie dziala w tym while
# while (filterData %>%  filter(time_diff != 0) %>% summarise(n()) %>% first() > 0){
#   diffDay <- filterData %>%  
#     filter(time_diff != 0) %>% 
#     bind_rows(as.data.frame(.) %>% select(name, timeStart, timeEnd, time_diff) %>% 
#                 mutate(timeStart = timeStart, timeEnd = as.Date(timeStart) + days(1), time_diff = as.integer(difftime(timeEnd, timeStart, units = "days")), part = "new"), as.data.frame(.) %>% 
#                 select(name, timeStart, timeEnd, time_diff) %>% 
#                 mutate(timeStart = as.Date(timeEnd), timeEnd = timeEnd, time_diff = as.integer(difftime(timeEnd, timeStart, units = "days")), part = "new")) %>% 
#     filter(part == "new") %>% 
#     select(-part)
#   
#   filterData <- filterData %>% 
#     filter(time_diff == 0) %>% 
#     bind_rows(diffDay)
#   print(1)
# }

filterData$timeStart <- strptime(filterData$timeStart, '%Y-%m-%d %H:%M')
filterData$timeEnd <- strptime(filterData$timeEnd, '%Y-%m-%d %H:%M')

#View(filterData)

filterData <- filterData %>%
  mutate(minutes = as.numeric(difftime(timeEnd, timeStart, units="mins"))) %>% 
  mutate(week = format(filterData$timeStart, format="%Y-%U")) %>% 
  mutate(weekday = wday(timeStart))


#stringi Wojtek
home <- c("mikrus", "cieplewo", "łęgowo")
uni <- c("university", "akademik", "lincoln", "politechnika", "faculty", "central")
fun <- c("unii", "fryzjer", "rostock", "museum", "suntago", "royal", "church", "polny", "game", "frankfurt", "hamburg")

#stringi Tymek
home <- c("konstancin", "home")
uni <- c("wydział", "pw")
fun <- c("ramen", "asia", "boisko", "green", "garden", "mcdonald's", "momencik", "park", "soto", "sphinx", "cafe",
         "piaseczno", "handsome", "dworzec", "kabaty", "stańczyka", "kebab", "marcello", "dolinka", "pawilony", "wars", 
         "tarasy", "gołków", "arkadia", "boisko", "galeria", "janki")
other <- c("biedronka", "stara", "lidl", "magazyn", "lecznica", "żabka")

#View(filterData)
#stringi Czarek
home <- c("kazimierów", "willa", "repkowska", "sokołowska")
uni <- c("gmach", "university")
fun <- c("cybermachina", "gato", "kredens", "kuchnia", "manekin", "muzeum", "cafe", "restauracja")


homes <- filterData[sapply(strsplit(filterData$name, split=" "), function(str) any(home %in% str)), ] %>% 
  select("timeStart", "timeEnd") %>% 
  mutate(type = "home")

unis <- filterData[sapply(strsplit(filterData$name, split=" "), function(str) any(uni %in% str)), ] %>% 
  select("timeStart", "timeEnd") %>% 
  mutate(type = "uni")

funs <- filterData[sapply(strsplit(filterData$name, split=" "), function(str) any(fun %in% str)), ] %>% 
  select("timeStart", "timeEnd") %>% 
  mutate(type = "fun")


filterData <- left_join(filterData, homes, by=c('timeStart'='timeStart', 'timeEnd'='timeEnd'))
filterData <- left_join(filterData, unis, by=c('timeStart'='timeStart', 'timeEnd'='timeEnd'))
filterData <- left_join(filterData, funs, by=c('timeStart'='timeStart', 'timeEnd'='timeEnd'))
filterData <- filterData %>%
  mutate(type = case_when(
    !is.na(type.x)~"home",
    !is.na(type.y)~"uni",
    !is.na(type)~"fun",
    TRUE~"other"
  )) %>% select(-type.x, -type.y)

#View(filterData)

#dir.create("ramkiW")
saveRDS(filterData, file = "time_data/dataW.rds")
saveRDS(filterData, file = "time_data/dataT.rds")
saveRDS(filterData, file = "time_data/dataC.rds")

View(filterDataC)
filterDataW <- readRDS(file = "time_data/dataW.rds")
filterDataT <- readRDS(file = "time_data/dataT.rds")
filterDataC <- readRDS(file = "time_data/dataC.rds")
#View(filterDataC)
#filterDataC %>% filter(time_diff != 0)

filterDataW <- filterDataW %>% 
  mutate(person = "W")
filterDataT <- filterDataT %>% 
  mutate(person = "T")
filterDataC <- filterDataC %>% 
  mutate(person = "C")
filterData <- rbind(filterDataW, filterDataT, filterDataC) %>% 
  select(-time_diff)

saveRDS(filterData, file = "time_data/data.rds")
filterData <- readRDS(file = "time_data/data.rds")
filterData %>% filter(minutes > 1440)

View(filterData)
View(df_merged)

person <- c("W", "T", "C")
baseFrame <- data.frame(weekday = c(1:7), hours = integer(7)) %>% 
  expand(weekday, hours, person)
saveRDS(baseFrame, file = "ramkiW/baseFrame.rds")

#View(baseFrame)
#filterDataW <- filterDataW %>% mutate(hours = sum(minutes)/60)
#View(filterDataW)                        
graphData <- filterData %>% 
  filter(week == "2022-50" & type == "home" & person %in% c("W", "T", "C")) %>% 
  select(week, weekday, minutes, person) %>% 
  group_by(weekday, person) %>% 
  summarise(hours = sum(minutes)/60) %>% 
  data.frame()
View(graphData)
#View(graphDataW)

days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
graphData <- graphData %>% 
  mutate(weekdayN=case_when(
    weekday==1~"Mon",
    weekday==2~"Tue",
    weekday==3~"Wed",
    weekday==4~"Thu",
    weekday==5~"Fri",
    weekday==6~"Sat",
    weekday==7~"Sun"
  ))

graphData  %>% 
  print(weekday)

typeof(factor(graphData$weekday[1]))
View(graphData)
graphData <- graphData %>% 
  full_join(baseFrame, by = c("weekday", "person")) %>% 
  mutate(hours = coalesce(hours.x, hours.y)) %>% 
  select(-c(hours.x, hours.y)) %>% 
  filter(person %in% c("W", "T"))

plot <- ggplot(data = graphData, aes(x=weekday, y=hours, group = person, colour = person)) +
  geom_line() + 
  geom_point() +
  theme_bw()+
  scale_x_continuous("weekday", labels = graphData$weekday, breaks = graphData$weekday)
plot

plot1 <- ggplot(data = graphData, aes(x=weekday, y=hours, group = person, color = person)) +
  geom_line() + 
  scale_color_manual(
    values = c("C" = "#4285F4", "W" = "#0F9D58", "T" = "#F4B400")
  ) +
  geom_point() +
  theme_minimal()+
  scale_x_continuous("Weekday", labels = graphData$weekdayN, breaks = graphData$weekday) +
  labs(y = "Hours") +
  theme(legend.title = element_blank(),
        legend.position = "none")
plot1

View(graphData)

