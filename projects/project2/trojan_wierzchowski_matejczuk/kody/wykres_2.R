library(dplyr)
library(plotly)
library(forcats)
library(lubridate)
ramka <-function(name_telefon,name_komputer){
  
  telefon <- jsonlite::fromJSON(name_telefon,)
  telefon <- telefon$buckets$`aw-watcher-android-test`$events %>% 
    select(2:4) %>%
    mutate(title ="", type = "tel")
  telefon$data <- telefon$data$app
  colnames(telefon)[3] <- "app"
  komputer <- read.csv(name_komputer) %>%
    mutate(type = "komp")
  
  df <- rbind(telefon,komputer) %>%
    drop_na(duration) %>%
    mutate(date = substr(timestamp, 1, 10), hour = substr(timestamp, 12, 19)) %>%
    mutate(start = as.POSIXlt(paste(date, hour, sep = " "))) %>% 
    mutate(end = as.POSIXlt(start + as.integer(duration))) %>%
    select(c("start","end","type","app"))

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
  
  df <- df %>%
    mutate(s_h = hour(start) ,e_h= hour(end))
  
  i = nrow(df)
  row = 1
  while (row <= i) {
    s <- df[row, "s_h"]
    e  <- df[row, "e_h"]
    if(s!=e){
      
      new_start <- as.POSIXlt(paste(date(df[row, "start"]), " ",hour(df[row, "start"]) + 1,":00:00", sep = ""))
      new_end <- as.POSIXlt(paste(date(df[row, "start"]), " ",hour(df[row, "start"]) ,":59:59", sep = ""))
      df <- rbind(df,data.frame(start = new_start , end = df[row, "end"], app = df[row, "app"],type = df[row, "type"], s_h = s+1 , e_h = e))
      df[row, "end"] <- data.frame(new_end)
      df[row, "e_h"] <- data.frame(s)
      i = nrow(df)
    }
    row = row + 1
  }
  df <- df %>% 
    mutate(duration = end - start) %>%
    filter(duration > 0) %>%
    mutate(week_day = wday(start, week_start=1),
           hour =  hour(start),
           day = date(start)) %>%
    mutate(app = paste(toupper(substr(app, 1, 1)), substr(app, 2, nchar(app)), sep="")) %>%
    mutate(app = gsub(".exe", "", app, ignore.case = T)) %>%
    select(-c(s_h,e_h))
  return(df)
}

save <-function(name, ramka){
  write.csv(ramka, name, row.names=FALSE)
}
ST <- ramka("stel.json", "skomp.csv")
WW <- ramka("wtel.json", "wkomp.csv")
MM <- ramka("mtel.json", "mkomp.csv")


save("wykres_2_ST",ST)
save("wykres_2_WW",WW)
save("wykres_2_MM",MM)