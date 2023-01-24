df <- read.csv("data.csv", sep=";")

library(dplyr)
library(stringr)
library(lubridate)

date <- df$Date

month <- str_extract(date, "[:alpha:]*")
day <- str_extract(date, "[:digit:]+")
year <- str_extract(date, "\\d{4}")
month <- case_when(
  month == "December" ~ "12",
  month == "January" ~ "01",
  TRUE ~ ""
)

new_date <- paste(year, month, day, sep = "-")
new_date <- as.Date(new_date)

df$Date <- new_date


time <- df$UsageTime

hour <- str_extract(time, "[:digit:]+h")
minute <- str_extract(time, "[:digit:]+m")
second <- str_extract(time, "[:digit:]+s")

nhour <- as.integer(str_extract(hour, "[:digit:]+"))
nminute <- as.integer(str_extract(minute, "[:digit:]+"))
nsecond <- as.integer(str_extract(second, "[:digit:]+"))

new_time <- ifelse(is.na(nhour), 0, nhour) * 3600 + ifelse(is.na(nminute), 0, nminute) * 60 + ifelse(is.na(nsecond), 0, nsecond)

df$UsageTime <- new_time

dev <- df$Device
d <- gsub("[0-9]+", "", dev)

df$Device <- d
df <- df %>% mutate(Device = recode(Device, 'samsung SM-AB' = 'Samsung', 'HUAWEI STK-LX' = "Huawei"))

              