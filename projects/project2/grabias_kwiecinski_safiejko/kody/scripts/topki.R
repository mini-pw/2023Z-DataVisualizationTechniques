library(dplyr)

#top apps
get_top_apps <-  function(df) {
  df %>%
    group_by(app) %>%
    select(app,duration) %>% 
    mutate(duration= sum(duration)) %>% 
    
    filter(app!="loginwindow", app!="LockApp", app!= "ScreenSaverEngine", app != 'ApplicationFrameHost', app != "unknown", app!="Code", app != "Wiadomości") %>% 
    
    mutate(app = case_when(app=="rstudio" ~ "RStudio",
                                app=="chrome" ~ "Google Chrome",
                                app=="WINWORD" ~ "Word",
                                app=="LeagueClientUx" ~ "League of Legends",
                                app=="Leage of Legends" ~ "League of Legends",
                                app=="idea64" ~ "IntelliJ",
                                app=="ONENOTE" ~ "OneNote",
                                app=="Teams" ~ "Teams",
                                app=="Microsoft Teams" ~ "Teams",
                                app=="pycharm64" ~"PyCharm",
                                app=="Code - Insiders" ~ "Visual Studio Code",
                                app=="explorer" ~ "Eksplorator plików",
                                app=="msedge" ~ "Microsoft Edge",
                                T~app)
                                ) -> df1 
    
    df1 %>% 
      unique() %>%
      arrange(desc(duration)) %>%
      distinct(app) %>%
      head(5)
}



#############################
#SPOTIFY
#############################
Sys.setenv(SPOTIFY_CLIENT_ID = 'b4a4fa3315e44ebda8701c684cc5f43e')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '605e78b16bee4a499dbce461cdf57e6d')
access_token <- get_spotify_access_token()


#top artists- images
get_top_artists_img <- function(df) {
  img_url <-  list()
  df1 <- get_top_artists_names(df)
  for (i in 1:5) {
    img_url[[i]]= (search_spotify(df1$artistName[[i]], type="artist", offset=0))$images[[1]]$url[1]
  }
  img_url
}

#top artists- Names
get_top_artists_names <- function(df) {
  
  
  df %>% 
    mutate(artistLink = paste0("https://open.spotify.com/search/", artistName)) %>% 
    select(artistName, artistLink) %>% 
    group_by(artistName, artistLink) %>% 
    summarise(n=n()) %>% 
    arrange(desc(n)) %>% 
    head(5) 
}



#top genres
get_top_genres <- function(df) {
    df %>% 
    select(genre) %>%
    rowwise %>%
    mutate(genre = paste(genre, collapse = ',')) %>%
    mutate(genre = strsplit(as.character(genre), ",")) %>% 
    unnest(genre) %>%
    group_by(genre) %>%
    mutate(count = n()) %>%
    arrange(desc(count)) %>%
    mutate(p = duplicated(genre)) %>%
    filter(p == FALSE) %>%
    select(genre) %>% 
    head(5)
}

#genres images
get_genres_img <- function(df) {
  img_url <-  list()
  df1 <- get_top_genres(df)
  temp_art <- list()
  for (i in 1:5) {
    df %>%
      filter(df$genre %in% df1$genre[[i]],!((df$artistName) %in% (temp_art))) %>%
      select(artistName) %>%
      unique() %>%
      head(1) -> temp
    temp$artistName -> temp_art[[i]]
    
    
    img_url[[i]] = (search_spotify(temp_art[[i]], type = "artist",
                                   offset = 0))$images[[1]]$url[1]
    
  }
  
  img_url
}





