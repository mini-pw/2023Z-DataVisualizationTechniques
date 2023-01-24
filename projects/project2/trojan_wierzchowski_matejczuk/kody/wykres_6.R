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
                                T ~ "work"))
  df <- df %>% mutate(date = substr(timestamp, 1, 10), hour = substr(timestamp, 12, 19)) %>%
    mutate(start = as.POSIXlt(paste(date, hour, sep = " "))) %>% 
    mutate(end = as.POSIXlt(start + as.integer(duration))) %>%
    select(c("start","end","category"))
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
                                T ~ "work"))
  df <- df %>% mutate(date = substr(timestamp, 1, 10), hour = substr(timestamp, 12, 19)) %>%
    mutate(start = as.POSIXlt(paste(date, hour, sep = " "))) %>% 
    mutate(end = as.POSIXlt(start + as.integer(duration))) %>%
    select(c("start","end","category"))
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
                                T ~ "work"))
  df <- df %>% mutate(date = substr(timestamp, 1, 10), hour = substr(timestamp, 12, 19)) %>%
    mutate(start = as.POSIXlt(paste(date, hour, sep = " "))) %>% 
    mutate(end = as.POSIXlt(start + as.integer(duration))) %>%
    select(c("start","end","category"))
  return(df)
}


ramka_spotify <-function(spotify, df){
  spotify_michal <- jsonlite::fromJSON(spotify)
  spotify_michal$endTime <- as.POSIXct(spotify_michal$endTime)
  spotify_michal <- spotify_michal %>% filter(endTime > as.POSIXct("2022-12-12 00:00:00"), endTime < as.POSIXct("2023-01-10 23:59:59"))
  spotify_michal <- spotify_michal %>% mutate(start = endTime - msPlayed/1000)
  colnames(spotify_michal)[1] <- "end"
  spotify_michal <- as.data.frame(t(rev(as.data.frame(t(spotify_michal)))))
  rownames(spotify_michal) <- NULL
  
  ##ten sam kod co wy¿ej
  work <-rep(as.difftime(0, units = "secs"), nrow(spotify_michal))
  play <-rep(as.difftime(0, units = "secs"), nrow(spotify_michal))
  afk<- round(difftime(spotify_michal$end, spotify_michal$start, units = "secs"), digits = 0)
  spotify_michal<- cbind(spotify_michal, work, play, afk)
  
  
  i <- nrow(spotify_michal)
  j<- nrow(df)
  
  while(i!=0){
    while(j != 0 && spotify_michal[i,"start"]>df[j,"end"]){
      j<-j-1
    }
    if(j==0){break}
    
    if(spotify_michal[i,"start"]>=df[j, "start"] && spotify_michal[i, "end"] <= df[j, "end"]){
      spotify_michal[i, df$category[j]] <- spotify_michal$afk[i]
      spotify_michal$afk[i] <- as.difftime(0, units="secs")
    }
    else if(spotify_michal[i,"start"]>=df[j, "start"]){
      time_not_afk <- difftime(df[j, "end"], spotify_michal[i, "start"], units = "secs")
      spotify_michal[i, df$category[j]] <- spotify_michal[i, df$category[j]] + time_not_afk
      spotify_michal$afk[i]<-spotify_michal$afk[i] - time_not_afk
    }
    else if(df$end[j]>=spotify_michal$end[i] && df$start[j] < spotify_michal$end[i]){
      time_not_afk <- difftime(spotify_michal[i, "end"], df[j, "start"],  units = "secs")
      spotify_michal[i, df$category[j]] <- spotify_michal[i, df$category[j]] + time_not_afk
      spotify_michal$afk[i]<-spotify_michal$afk[i] - time_not_afk
    }
    else if(df$end[j]<spotify_michal$end[i]&& df$start[j] < spotify_michal$end[i]){
      time_not_afk <- difftime(df$end[j], df$start[j], units = "secs")
      spotify_michal[i, df$category[j]] <- spotify_michal[i, df$category[j]] + time_not_afk
      spotify_michal$afk[i]<-spotify_michal$afk[i] - time_not_afk
    }
    i <- i-1
  }
  df <- spotify_michal %>%
    mutate(rozrywka = play, AFK = afk, praca = work,
           hour =  hour(start),
           day = date(start)) %>%
    select(start, end, rozrywka, praca, AFK, hour, day) %>%
    
  return(df)
}

save <-function(name, df){
  write.csv(df, name, row.names=FALSE)
}

MM <- pobierz_dane("mtel.json","mkomp.csv")
MM <- aktywnosci_michal(MM)
MM <- ramka_spotify("mspot.json",MM)
save("wykres_6_MM", MM)

ST <- pobierz_dane("stel.json","skomp.csv")
ST <- aktywnosci_michal(ST)
ST <- ramka_spotify("sspot.json",ST)
save("wykres_6_ST", ST)

WW <- pobierz_dane("wtel.json","wkomp.csv")
WW <- aktywnosci_michal(WW)
WW <- ramka_spotify("wspot.json",WW)
save("wykres_6_WW", WW)