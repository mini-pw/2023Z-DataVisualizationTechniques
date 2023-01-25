library(dplyr)

merged_time <- read.csv("Łukasz Grabarski_merged_time.csv", sep = ",") # tutaj dla kazdej z 3 ramek to zrobisz

merged_time_to_work <- merged_time 

merged_time_to_work$date <- strtrim(merged_time_to_work$date, 19) # deleting miliseconds

merged_time_to_work <- merged_time_to_work %>% 
  mutate(date_only = as.Date(date)) %>% 
  mutate(time_only = date) %>% 
  mutate(hour_only = date) %>% 
  mutate(year_only = date) %>% 
  mutate(month_only = date) %>% 
  mutate(day_only = date)

merged_time_to_work$time_only <- format(as.POSIXct(merged_time_to_work$time_only), 
                                        format = "%H:%M:%S")
merged_time_to_work$hour_only <- format(as.POSIXct(merged_time_to_work$hour_only),
                                        format = "%H")
merged_time_to_work$year_only <- format(as.POSIXct(merged_time_to_work$year_only),
                                        format = "%Y")
merged_time_to_work$month_only <- format(as.POSIXct(merged_time_to_work$month_only),
                                         format = "%m")
merged_time_to_work$day_only <- format(as.POSIXct(merged_time_to_work$day_only),
                                       format = "%d")

merged_time_to_work$year_only <- as.numeric(merged_time_to_work$year_only) 
merged_time_to_work$year_only <- as.integer(merged_time_to_work$year_only) 
typeof(merged_time_to_work$year_only)

merged_time_to_work$month_only <- as.numeric(merged_time_to_work$month_only) 
merged_time_to_work$month_only <- as.integer(merged_time_to_work$month_only) 
typeof(merged_time_to_work$year_only)

merged_time_to_work$day_only <- as.numeric(merged_time_to_work$day_only) 
merged_time_to_work$day_only <- as.integer(merged_time_to_work$day_only) 
typeof(merged_time_to_work$day_only)

merged_time_to_work$hour_only <- as.numeric(merged_time_to_work$hour_only) 
merged_time_to_work$hour_only <- as.integer(merged_time_to_work$hour_only) 
typeof(merged_time_to_work$hour_only)


write.csv(merged_time_to_work, "D:\\IAD\\sem3\\wizualizacja danych\\projekt2\\lukasz_merged_time_to_work.csv", row.names=FALSE)
 ## tutaj ustawisz swoja scieze analogicznie (podwojne \\!)

##i tutaj tez zmienisz nazwe 3 razy (bo jest na 3 ) i bd miala wtedy w twoim folderze z projektem te 3 ramki

#### i teraz jak juz je masz, to w tym pliku w shiny te ify beda takie:
# if( ten wybor to magda) merged_time_to work <- read.csv("magda_merged_time_to_work.csv", sep = ",")
# a w kodzie plotu sie nic nie zmieni
