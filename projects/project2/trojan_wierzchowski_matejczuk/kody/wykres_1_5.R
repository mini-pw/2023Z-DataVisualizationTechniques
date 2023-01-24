library(vistime)
library(rjson)
library(plotly)
library(stringr)
library(tidyverse)
library(lubridate)
library(dplyr)


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

aktywnosci_michal <-function(df){
  work_komp <- c("Firefox", "Explorer", "Rstudio", "MS Word", "Shellexperiencehost", "Idea64", "Teams",
                 "Winrar", "Matlab", "Msteams", "Excel", "Acrobat", "Aw-Qt")
  work_tel <- c("Outlook", "Teams", "Multimedia i urządzenia", "Kalkulator",
            "Dokumenty", "Moje pliki", "Dysk", "Discord", "Pliki",
            "Arkusze", "Write Japanese", "Japanese GG", "Duolingo", "ActivityWatch",
            "Zegar")
  play_tel <- c("Chrome", "Mastodon", "Facebook", "Połącz.", "YouTube", "Aparat",
            "Spotify", "Messenger", "Santander", "Notatki Keep",
            "Telefon", "Mapy", "Bolt", "Instagram", "Genius",
            "Wiadomości", "Galeria", "Gmail", "żappka", "Netflix",
            "Kalendarz", "Dyktafon", "McDonald's", "Going.",
            "Booking.com", "Vinted", "Empik", "H&M", "Odtwarzacz wideo",
            "KFC Polska", "Pyszne", "Uber Eats", "Amazon Shopping", "Mięśnie brzucha w 30 dni",
            "Bandcamp", "Jakdojade")
  play_komp_title<- c("youtube", "facebook", "fmovies", "123movies", "spotify",
                                        "Rate Your Music", "tetris", "netflix", "film", "nowe horyzonty",
                                        "wizzair", "skyscanner", "booking", "ryanair")
  df <- df %>%
    mutate(app = case_when(type == "tel" ~ app,
                           type == "komp" ~ str_to_title(substring(df$app,1, nchar(df$app)-4)))) %>% 
    mutate(app = case_when(type == "komp" & app == "Winword" ~ "Word",
                           type == "komp" & app == "Mtga" ~ "MTG Arena",
                           T ~ app)) %>%
    
    mutate(category = case_when(app %in% work_komp & type == "komp" & grepl(paste(play_komp_title, collapse="|"), title ,ignore.case = TRUE) ~ "play",
                                app %in% work_komp & type == "komp" ~ "work",
                                type == "komp" ~ "play",
                                app %in% work_tel & type == "tel" ~ "work",
                                app %in% play_tel & type == "tel" ~ "play",
                                T ~ "other"))
  return(df)
           
}

aktywnosci_sebastian <-function(df){
  work_tel <- c("ActivityWatch", "Excel", "Kalkulator", "Mobilny USOS PW", "Teams", "Outlook", "Dysk", "Word", "Notatki Samsung", "Arkusze", "OneDrive")
  play_tel <- c("YouTube", "Facebook", "Footroll", "lichess", "Prime Video", "Spotify", "CANAL+", "player", "Discord", "cda.pl", "Archero", "Game Launcher","Netflix")
  work_komp <- c("rstudio.exe", "aw-qt.exe", "EXCEL.EXE", "Teams.exe", "notepad.exe", "MATLAB.exe", "OUTLOOK.EXE","idea64.exe", "WINWORD.EXE", "pycharm64.exe", "chrome.exe", "explorer.exe")
  play_komp <- c("steam.exe","Discord.exe", "steamwebhelper.exe","ProjectZomboid64.exe", "PlateUp.exe")
  df <- df %>%
    mutate(category = case_when(app %in% work_komp & type == "komp" ~ "work",
                                app %in% play_komp & type == "komp" ~ "play",
                                app %in% work_tel & type == "tel" ~ "work",
                                app %in% play_tel & type == "tel" ~ "play",
                                T ~ "other"))
  return(df)
}

aktywnosci_wiktor <-function(df){
  work_tel <- c("ActivityWatch", "Calculator", "Drive", "Mobile USOS PW", "Teams", "Outlook", "Samsung Notes", "Adobe Acrobat", "Duolingo")
  play_tel <- c("Messenger", "YouTube", "BeReal.","Reddit", "Spotify","Facebook","Instagram","FR Legends","Secret Santa 22","Soul Knight" ,"YouTube Music")
  play_komp_title <- c('factorio','lol')
  work_komp_title <- c('java','iad','usos','activitywatch','matlab','ects','outlook','google','bitbucket','smartdivide')
  play_komp <- c('Spotify.exe', 'Discord.exe','LeagueClientUx.exe','League of Legends.exe','Overwolf.exe','steam.exe','factorio.exe')
  work_komp <- c('explorer.exe','rstudio.exe','RStudio-2022.12.0-353.exe','firefox.exe','idea64.exe','java.exe','MATLAB.exe','notepad.exe','Teams.exe','GitHubDesktop.exe','SceneBuilder.exe')
                                                                                                     
  df <- df %>%
    mutate(category = case_when(app %in% work_tel & type == "tel" ~ "work",
                                app %in% play_tel & type == "tel" ~ "play",
                                grepl(paste(play_komp_title, collapse="|"), title ,ignore.case = TRUE) & type == "komp" ~ "play",
                                grepl(paste(work_komp_title, collapse="|"), title ,ignore.case = TRUE) & type == "komp" ~ "work",
                                app %in% work_komp & type == "komp" ~ "work",
                                app %in% play_komp & type == "komp" ~ "play",
                                T ~ "other"))
}


przetwarzanie_ramki <-function(df){
  df <- df %>% mutate(date = substr(timestamp, 1, 10), hour = substr(timestamp, 12, 19)) %>%
    mutate(start = as.POSIXlt(paste(date, hour, sep = " "))) %>% 
    mutate(end = as.POSIXlt(start + as.integer(duration))) %>%
    select(c("start","end","category","type"))
  
  for (row in 1:nrow(df)) {
    s <- df[row, "start"]
    e  <- df[row, "end"]
    if (s[,"mday"] != e[,"mday"]){
      new_start <- as.POSIXlt(paste(substr(e,1,10), "00:00:00", sep = " "))
      new_end <- as.POSIXlt(paste(substr(s,1,10), "23:59:59", sep = " "))
      a <- c(as.POSIXlt(new_start) ,as.POSIXlt(e))
      df[row, "end"] <- data.frame(new_end)
      df <- rbind(df,data.frame(start = as.POSIXlt(new_start), end = e, category = df[row, "category"], type = df[row, "type"]))
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
      df <- rbind(df,data.frame(start = new_start , end = df[row, "end"], category = df[row, "category"],type = df[row, "type"], s_h = s+1 , e_h = e))
      df[row, "end"] <- data.frame(new_end)
      df[row, "e_h"] <- data.frame(s)
      i = nrow(df)
    }
    row = row + 1
  }
  
  df <- df %>%
    mutate(duration = end - start) %>%
    select(-c(s_h,e_h)) %>%
    mutate (day = date(start), hour = hour(start)) %>%
    group_by(hour,day,category,type) %>%
    summarise(duration = sum(duration)/60) %>%
    ungroup() %>%
    mutate(week_day = weekdays(day),
           type = case_when(type == "tel" ~ "telefon",
                            type == "komp" ~ "komputer"),
           category = case_when(category == "other" ~ "inne",
                            category == "play" ~ "rozrywka",
                            category == "work" ~ "praca"))
  return(df)
}

save <-function(name, df){
  write.csv(df, name, row.names=FALSE)
}

ST <- pobierz_dane("stel.json","skomp.csv")
ST <- aktywnosci_sebastian(ST)
ST <- przetwarzanie_ramki(ST)

WW <- pobierz_dane("wtel.json","wkomp.csv")
WW <- aktywnosci_wiktor(WW)
WW <- przetwarzanie_ramki(WW)

MM <- pobierz_dane("mtel.json","mkomp.csv")
MM <- aktywnosci_michal(MM)
MM <- przetwarzanie_ramki(MM)

save("wykres_1_5_ST",ST)
save("wykres_1_5_MM",MM)
save("wykres_1_5_WW",WW)