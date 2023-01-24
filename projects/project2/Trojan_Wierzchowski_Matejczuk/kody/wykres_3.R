library(dplyr)
library(vistime)
library(lubridate)
library(rjson)
library(plotly)
library(stringr)
library(tidyverse)

pobierz_dane <-function(name_telefon,name_komputer){
  
  telefon <- jsonlite::fromJSON(name_telefon,)
  telefon <- telefon$buckets$`aw-watcher-android-test`$events %>% 
    select(2:4) %>%
    mutate(title ="", type = "tel")
  telefon$data <- telefon$data$app
  colnames(telefon)[3] <- "app"
  komputer <- read.csv(name_komputer) %>%
    mutate(type = "komp")
  
  df <- rbind(telefon,komputer) %>%
    drop_na(duration)
  return(df)
}


obrabianie_danych <-function(df){
  df <- df %>% mutate(date = substr(timestamp, 1, 10), hour = substr(timestamp, 12, 19)) %>%
    mutate(start = as.POSIXlt(paste(date, hour, sep = " "))) %>% 
    mutate(end = as.POSIXlt(start + as.integer(duration))) %>%
    select(c(start,end,app, type))
  
  for (row in 1:nrow(df)) {
    s <- df[row, "start"]
    e  <- df[row, "end"]
    if (s[,"mday"] != e[,"mday"]){
      new_start <- as.POSIXlt(paste(substr(e,1,10), "00:00:00", sep = " "))
      new_end <- as.POSIXlt(paste(substr(s,1,10), "23:59:59", sep = " "))
      a <- c(as.POSIXlt(new_start) ,as.POSIXlt(e))
      df[row, "end"] <- data.frame(new_end)
      df <- rbind(df,data.frame(start = as.POSIXlt(new_start), end = e, app = df[row, "app"], type = df[row, "type"]))
    }
  }
  
  df<- df %>%
    mutate(duration = difftime(end,start),
           day = date(start))%>% 
    filter(duration>0 , app!="unknown") %>%
    group_by(app, day, type) %>%
    mutate(dur = sum(duration)) %>% filter(dur>10*60) %>% 
    ungroup() %>%
    select(-c(duration, dur))  %>%
    arrange(app, start)
  df <- as.data.frame(df)
  df <- df %>% mutate(start = as.POSIXct(start),end = as.POSIXct(end))
  
  row = 0
  while(!is.na(df[row+2,"app"])){
    row = row + 1
    if (df[row,"day"]!=df[row+1,"day"] | df[row,"app"]!=df[row+1,"app"] ){next}
    if (as.numeric(difftime(df[row +1 , "start"],df[row, "end"], units = "secs")) < 600){
      df[row, "end"] <- df[row + 1 , "end"]
      df<-df[-(row+1),]
      row = row - 1
    }
  }
  
  
  
  df <- df %>% 
    mutate(color = "#2c3e50") %>%
    mutate(app = paste(toupper(substr(app, 1, 1)), substr(app, 2, nchar(app)), sep="")) %>%
    mutate(app = gsub(".exe", "", app,  ignore.case = T)) %>%
    mutate(tooltip = paste(app,"\n", "Od", format(start, "%H:%M:%S"), "do", format(end, "%H:%M:%S")))
  
  return(df)
}

save <-function(name, df){
  write.csv(df, name, row.names=FALSE)
}

ST <- pobierz_dane("stel.json","skomp.csv")
ST <- obrabianie_danych(ST)

WW <- pobierz_dane("wtel.json","wkomp.csv")
WW <- obrabianie_danych(WW) 

MM <- pobierz_dane("mtel.json","mkomp.csv")
MM <- obrabianie_danych(MM) 


save("wykres_3_ST",ST)
save("wykres_3_MM",MM)
save("wykres_3_WW",WW)