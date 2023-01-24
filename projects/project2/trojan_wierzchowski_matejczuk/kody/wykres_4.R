library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(forcats)



ramka_komputer <- function(komp,afk){
  afk <- read.csv(afk)
  window <- read.csv(komp)
  window <- window %>% mutate(date = substr(timestamp, 1, 10), hour = substr(timestamp, 12, 19)) %>%
    mutate(start = as.POSIXlt(paste(date, hour, sep = " "))) %>% 
    mutate(end = as.POSIXlt(start + as.integer(duration)))
  
  afk <- afk %>% mutate(date = substr(timestamp, 1, 10), hour = substr(timestamp, 12, 19)) %>%
    mutate(start = as.POSIXlt(paste(date, hour, sep = " "))) %>% 
    mutate(status = case_when(status == "afk" ~ 0,
                              T ~ 1)) %>%
    group_by(start) %>%
    summarise(duration = max(duration), status = max(status)) %>%
    ungroup() %>%
    mutate(status = case_when(status == 0 ~ "afk",
                              T ~ "not-afk")) %>%
    mutate(end = as.POSIXlt(start + as.integer(duration))) 
  
  AFK <- afk %>%
    filter(status == "afk") %>%
    select(start,end)
    
  NAFK <- afk %>%
    filter(status == "not-afk") %>%
    select(start,end)
  
  AFK <- AFK %>% mutate(indx = -1)
  
  for (i in 1:nrow(NAFK)) {
    AFK <- AFK %>%
      mutate(start = case_when(start>NAFK$end[i] | end <NAFK$start[i]  ~ start,
                               start>=NAFK$start[i]& end<=NAFK$end[i] ~ as.POSIXlt(NA),
                               start<NAFK$start[i]& end >= NAFK$start[i] & end <= NAFK$end[i] ~ start,
                               start>=NAFK$start[i]&  start <= NAFK$end[i]& end > NAFK$end[i] ~ as.POSIXlt(NAFK$end[i] + 1),
                               start<NAFK$start[i] & end > NAFK$end[i] ~ start),
             end = case_when(start>NAFK$end[i] | end <NAFK$start[i]  ~ end,
                             start>=NAFK$start[i]& end<=NAFK$end[i] ~ as.POSIXlt(NA),
                             start<NAFK$start[i]& end >= NAFK$start[i] & end <= NAFK$end[i] ~ as.POSIXlt(NAFK$start[i]-1),
                             start>=NAFK$start[i]&  start <= NAFK$end[i]& end > NAFK$end[i] ~ end,
                             start<NAFK$start[i] & end > NAFK$end[i] ~ end),
             indx = case_when(start<NAFK$start[i] & end > NAFK$end[i]  ~ as.numeric(i),
                              T ~ indx))
  }
  
  do_poprawy <- AFK %>%
    filter(indx != -1)
  
  
  
  if (nrow(do_poprawy)>=1){
    for (i in 1:nrow(do_poprawy)) {
      df <- data.frame(start = NAFK$end[do_poprawy$indx[i]] + 1 ,end = do_poprawy$end[i],indx = do_poprawy$indx[i])
      do_poprawy[i,"end"]<- NAFK$start[do_poprawy$indx[i]] -1
      do_poprawy <- rbind(do_poprawy,
                     df)
    }
  
    
    AFK <- AFK %>%
      filter(indx == -1)
    
    AFK <- rbind(AKK, do_poprawy)
  }
  
  
  AFK <- AFK %>% select(-indx)
  
  window <- window %>% mutate(indx = -1)
  
  for (i in 1:nrow(AFK)) {
    window <- window %>%
      mutate(start = case_when(start>AFK$end[i] | end <AFK$start[i]  ~ start,
                                start>=AFK$start[i]& end<=AFK$end[i] ~ as.POSIXlt(NA),
                                start<AFK$start[i]& end >= AFK$start[i] & end <= AFK$end[i] ~ start,
                                start>=AFK$start[i]&  start <= AFK$end[i]& end > AFK$end[i] ~ as.POSIXlt(AFK$end[i] + 1),
                                start<AFK$start[i] & end > AFK$end[i] ~ start),
             end = case_when(start>AFK$end[i] | end <AFK$start[i]  ~ end,
                              start>=AFK$start[i]& end<=AFK$end[i] ~ as.POSIXlt(NA),
                              start<AFK$start[i]& end >= AFK$start[i] & end <= AFK$end[i] ~ as.POSIXlt(AFK$start[i]-1),
                              start>=AFK$start[i]&  start <= AFK$end[i]& end > AFK$end[i] ~ end,
                              start<AFK$start[i] & end > AFK$end[i] ~ end),
             indx = case_when(start<AFK$start[i] & end > AFK$end[i]  ~ as.numeric(i),
                              T ~ indx))
    
  }
  
  
  window <- window %>%
    select(app, start, end, indx)
  
  wazne <- window %>%
    filter(indx != -1)
  
  if (nrow(wazne)>=1){
  for (i in 1:nrow(wazne)) {
    df <- data.frame(app = wazne$app[i],start = AFK$end[wazne$indx[i]] + 1 ,end = wazne$end[i],indx = wazne$indx[i])
    wazne[i,"end"]<- AFK$start[wazne$indx[i]] -1
    wazne <- rbind(wazne,
                   df)
    
  }
    
  
  window <- window %>%
    filter(indx == -1)
    
  window <- rbind(window, wazne)
  }
  
  window <- window %>% select(-c(indx))
  
  
  
  window <- window %>%
    mutate(st = "n-afk")
  
  a <- AFK %>%
    mutate(app = NA,
           st = "afk" )
  
  df <- rbind(window,a)
  
  df <- df %>% arrange(start) %>%
    fill(app) %>%
    filter(app != '') 
  df <- df %>%
    mutate(app = paste(toupper(substr(app, 1, 1)), substr(app, 2, nchar(app)), sep="")) %>%
    mutate(app = gsub(".exe", "", app,  ignore.case = T))
  
  df <- df %>% 
    mutate(duration = end - start) %>%
    filter(duration > 0) %>%
    mutate(
    type = "komputer")
  return(df)
}

ramka_koncowa <- function(tel,komp){
  telefon <- jsonlite::fromJSON(tel,)
  telefon <- telefon$buckets$`aw-watcher-android-test`$events %>% 
    select(2:4) 
  telefon$data <- telefon$data$app
  
  telefon <- telefon %>%
    filter(duration > 0) %>%
    mutate(day = substr(timestamp, 1, 10), hour = substr(timestamp, 12, 19)) %>%
    mutate(start = as.POSIXlt(paste(day, hour, sep = " ")),
            end = as.POSIXlt(start + as.integer(duration)),
           type = "telefon",
           st = "n-afk") %>%
    mutate(app = paste(toupper(substr(data, 1, 1)), substr(data, 2, nchar(data)), sep="")) %>%
    mutate(app = gsub(".exe", "", data)) %>%
    select(-c(timestamp,data,day,hour))
  
  df <- rbind(komp, telefon) %>%
    select(-duration)
  
  
  df <- df %>%
    mutate(s_h = hour(start) ,e_h= hour(end))
  
  zle <- df %>% filter(s_h != e_h ) %>%
    select(-c(s_h,e_h))
  
  df <- df %>% filter(s_h == e_h ) %>%
    select(-c(s_h,e_h))
  
  for (row in 1:nrow(zle)) {
    s <- zle[row, "start"]
    e  <- zle[row, "end"]
    if (s[,"mday"] != e[,"mday"]){
      new_start <- as.POSIXlt(paste(substr(e,1,10), "00:00:00", sep = " "))
      new_end <- as.POSIXlt(paste(substr(s,1,10), "23:59:59", sep = " "))
      a <- c(as.POSIXlt(new_start) ,as.POSIXlt(e))
      zle[row, "end"] <- data.frame(new_end)
      zle <- rbind(zle,data.frame(start = as.POSIXlt(new_start), end = e, app = zle[row, "app"],st = zle[row, "st"], type = zle[row, "type"]))
    }
  }
  
  zle <- zle %>%
    mutate(s_h = hour(start) ,e_h= hour(end)) %>%
    filter(s_h != e_h )
  
  i = nrow(zle)
  row = 1
  while (row <= i) {
    s <- zle[row, "s_h"]
    e  <- zle[row, "e_h"]
    if(s!=e){
      new_start <- as.POSIXlt(paste(date(zle[row, "start"]), " ",hour(zle[row, "start"]) + 1,":00:00", sep = ""))
      new_end <- as.POSIXlt(paste(date(zle[row, "start"]), " ",hour(zle[row, "start"]) ,":59:59", sep = ""))
      zle <- rbind(zle,data.frame(start = new_start , end = zle[row, "end"], app = zle[row, "app"],st = zle[row, "st"],type = zle[row, "type"], s_h = s+1 , e_h = e))
      zle[row, "end"] <- data.frame(new_end)
      zle[row, "e_h"] <- data.frame(s)
      i = nrow(zle)
    }
    row = row + 1
  }

  
  zle <- zle %>%
    select(-c(s_h,e_h))
  
  df <- rbind(df, zle)
  
  df <- df %>% 
    mutate(duration = end - start) %>%
    filter(duration > 0) %>%
    mutate(week_day = wday(start, week_start=1),
            hour =  hour(start),
           day = date(start))
  df <- df %>%
    mutate(type  = case_when(type == "komputer" ~ "komp",
                             T ~ "tel")) %>%
    mutate(st = case_when(st == "afk" ~ "AFK",
                          T ~ "NOT AFK"))
  return(df)
}

save <-function(name, df){
  write.csv(df, name, row.names=FALSE)
}

ST <- ramka_komputer("skomp.csv","safk.csv")
ST <- ramka_koncowa("stel.json",ST)
save("wykres_4_ST", ST)

WW <- ramka_komputer("wkomp.csv","wafk.csv")
WW <- ramka_koncowa("wtel.json",WW)
save("wykres_4_WW", WW)

MM <- ramka_komputer("mkomp.csv","mafk.csv")
MM <- ramka_koncowa("mtel.json",MM)
save("wykres_4_MM", MM)
