# some scripts helping R
library(dplyr)
library(spotifyr)

data_folder <- "data/"


#TODO
add_categories_spotify <- function(df) {
  # this function categotises songs - this will be to remove since hopefully the new data is categorised
  df %>%  mutate(category = case_when(artist == "Dawid Podsiadło" ~ "muzyka",
                                      TRUE ~ "szit")) -> df
}



read_spotify_data <- function(filename, sufix = "_spotify") {
  # function takes users nick and returns dataframe of their activity on spotify
  # rebuilt for new data
  
  # generate filepath
  filepath <- paste0(data_folder, filename, sufix, ".csv")
  
  
  # reading dataframe
  df <- read.csv(filepath, encoding = "UTF-8") %>%
    mutate(endTime = strptime(endTime, "%Y-%m-%d %H:%M"))
  
  if ("msPlayed" %in% colnames(df)) {
    # to make this function more universal - if we reread the file it doesn't have this column
    df <- df %>%
      mutate(duration = msPlayed / 1000) %>% select(c(-msPlayed))
  }
  
  df %>%
    mutate(startTime = endTime - ceiling(duration)) %>%  # we are naive and assume that song ended exactly at full minute
    mutate(date = as.Date(startTime))
}


# old one
read_spotify_data2 <- function(filename, sufix = "") {
  # function takes users nick and returns dataframe of their activity on spotify
  
  
  # generate filepath
  filepath <- paste0(data_folder, filename, sufix, ".csv")
  
  # we need to change locale in order to propely read the month
  
  orig_locale <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  
  # reading dataframe
  df <- read.csv(filepath, header = FALSE) %>%
    rename(
      artist = V1,
      album = V2,
      track = V3,
      timestamp = V4
    )  %>%
    mutate(timestamp = strptime(timestamp, "%d %b %Y %H:%M")) %>%
    mutate(date = as.Date(timestamp))
  
  Sys.setlocale("LC_TIME", orig_locale) # we're setting the org_locale back
  
  df <- add_categories_spotify(df)
  df
}


#TODO
add_categories_kompik <- function(df) {
  studia <-
    c("RStudio",
      "Google Chrome",
      "PyCharm",
      "Microsoft Excel",
      "Eclipse",
      "GitHub Desktop",
      "rstudio",
      "MATLAB",
      "idea64",
      "chrome",
      "WINWORD",
      "GitHubDesktop",
      "ONENOTE",
      "java",
      "Teams",
      "msedge"
    )
  
  wolne <- c("Spotify", "Messenger", "League of Legends")
  
  
  df %>% mutate(category = case_when(app %in% studia ~ "studies",
                                     app %in% wolne ~ "free time",
                                     TRUE ~ "other"))
}

read_kompik_data <- function(filename, sufix = "_computer") {
  # function takes filename and returns dataframe of their activity on their computer
  # (file needs to be in a folder data and with extension .csv)
  
  filepath <- paste0(data_folder, filename, sufix, ".csv")
  
  df <- read.csv(filepath, encoding = "UTF-8")
  
  if ("timestamp" %in% colnames(df)) {
    df <- df %>%
      mutate(timestamp = strptime(timestamp, "%Y-%m-%dT%H:%M:%S")) %>%
      rename(startTime = timestamp) %>%
      mutate(endTime = startTime + ceiling(duration)) %>%
      mutate(date = as.Date(startTime)) %>%
      mutate(app = gsub("\\..*", "", app))# %>%
      #mutate(app = ifelse(grepl("Messenger", title, fixed = TRUE), "Messenger", app)) # messenger in chrome
  } else {
    df <- df %>%
      mutate(date = as.Date(date)) %>%
      mutate(endTime = strptime(endTime, "%Y-%m-%d %H:%M")) %>%
      mutate(startTime = strptime(startTime, "%Y-%m-%d %H:%M"))
  }
  
  bad_apps <- c("loginwindow", "LockApp")
  
  ## reformat app names
  df %>% filter(!(app %in% bad_apps))
}


read_merged_data <- function(filename, sufix = "_merged") {
  filepath <- paste0(data_folder, filename, sufix, ".csv")
  
  df <- read.csv(filepath, encoding = "UTF-8") %>%
    mutate(startTime = strptime(startTime, "%Y-%m-%d %H:%M")) %>%
    mutate(date = as.Date(startTime)) 
  
}

add_audio_features <-
  function(df,
           track_col_name = "trackName",
           artist_col_name = "artistName") {
    # function adds audio features using idea of wojtek
    
    # Funkcja pomocnicza do odnajdywania indeksu pod którym znajduje się nasza piosenka
    find_id_track_by_artist <-
      function(track_list, track_name, artist_name) {
        return(track_list[["id"]][[1]])
      }
    
    # Funkcja zwracająca unikalne ID po wprowadzeniu nazwy piosenki i artysty
    get_id_by_name_and_artist <- function(track_name, artist_name) {
      track_list <- search_spotify(
        q = paste0("track:", track_name, " artist:", artist_name),
        type = c("track")
      )
      id <-
        find_id_track_by_artist(track_list, track_name, artist_name)
    }
    # Funkcja zwracająca genre autora po nazwie nazwie piosenki i autora
    get_artist_genre <- function(track_name, artist_name) {
      track_list <- search_spotify(
        q = paste0("track:", track_name, " artist:", artist_name),
        type = c("track")
      )
      if(length(track_list)== 0){
        return(NA)
      }
      tryCatch({artist_id <- track_list[[1]][[1]][[2]][[1]]
               cat(artist_id, "\n")
               artist_info <- get_artists(artist_id)
               return(artist_info[[1]][[1]][[1]])},
      error=function(cond){
        return(NA)
      })
      
      #if (class(artist_info[[1]][[1]]) != "character") {
      #  return(NA)
      #}
      
    }
    # Funkcja zwracająca informacje o piosence z jej nazwy i autora
    get_track_info_name_and_artist <-
      function(track_name, artist_name) {
        res <-
          get_track_audio_features(get_id_by_name_and_artist(track_name, artist_name),
                                   authorization = get_spotify_access_token())
        print(track_name) # debbuging print - to check performance
        res
      }
    
    # Przekazujemy kolumny z utworami i artystami i pobieramy df z features
    res <-
      t(mapply(get_track_info_name_and_artist, df[[track_col_name]], df[[artist_col_name]]))
    
    res2 <-
      mapply(get_artist_genre, df[[track_col_name]], df[[artist_col_name]])
    
    res1 <- bind_rows(res)
    
    merged <-
      as.data.frame(bind_cols(df, res1)) %>% select(c(-value)) %>%
      mutate(genre = res2) %>%
      na.omit() 
    
  }
