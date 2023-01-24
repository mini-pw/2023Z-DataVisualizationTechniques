# plik zawiera funkcj? tworz?c? wykres rozk?adu s?uchanej muzyki na spotify
library(dplyr)
library(ggalluvial)
library(data.table) # dla ITime
library(lubridate) # dla within
library(forcats)


library(RColorBrewer)
library(calendR)
source("scripts/SpotifyTheme.R")
source("scripts/MergeTimestamps.R")
source("scripts/heatmap_data.R")

pagorki_color <- function(df, fill=TRUE, 
                    fill_column = "category", weight = TRUE) {
  # df - ramka danych zawieraj?ca dane kt?re nale?y podsumowa?
  # start_date - pocz?tek okresu filtrowania
  # end_date - koniec okresu filtrowania
  # fill - czy dane mają kategorie
  # fill_column - nazwa kolumny z kategoriami
  # weight - czy dane są zależne od siebie i gęstość w sumie powinna wynosić 1
  
  p <- df %>% 
    mutate(time = as.ITime(startTime)) %>% 
    ggplot(aes(x = time)) +
    scale_x_continuous(breaks = seq(0, 86400, 3600*6), labels = paste(seq(0, 24, 6), "00", sep = ":"),
                       limits = c(0, 86400), expand = c(0,160)) +
    theme_spotify_base() +
    geom_density(adjust=1.5, alpha=.4, size=1.5)
    
  
  if (fill == TRUE && fill_column %in% colnames(df)) {
    p <- p + aes(color=!!sym(fill_column))
    if(weight == TRUE) {
      p <- p + aes(y = after_stat(count))
    }
  } else {
    p <- p + aes(color = "")
  }
  p <- p + scale_y_continuous(expand=c(0,0))
  p # troche trzeba ten desity opisac co to dokladnie jest 

}


pagorki_fill <- function(df, fill=TRUE, 
                         fill_column = "category", weight = TRUE) {
  # df - ramka danych zawieraj?ca dane kt?re nale?y podsumowa?
  # start_date - pocz?tek okresu filtrowania
  # end_date - koniec okresu filtrowania
  # fill - czy dane mają kategorie
  # fill_column - nazwa kolumny z kategoriami
  # weight - czy dane są zależne od siebie i gęstość w sumie powinna wynosić 1
  
  p <- df %>% 
    mutate(time = as.ITime(startTime)) %>% 
    ggplot(aes(x = time)) +
    scale_x_continuous(breaks = seq(0, 86400, 3600*6), labels = paste(seq(0, 24, 6), "00", sep = ":"),
                       limits = c(0, 86400), expand = c(0,160)) +
    theme_spotify_base() +
    geom_density(adjust=1.5, alpha=.4)
  
  
  if (fill == TRUE && fill_column %in% colnames(df)) {
    p <- p + aes(fill = !!sym(fill_column))
    if(weight == TRUE) {
      p <- p + aes(y = after_stat(count))
    }
  } else {
    p <- p + aes(fill = "")
  }
  p <- p + scale_y_continuous(expand=c(0,0))
  p # troche trzeba ten desity opisac co to dokladnie jest 
  
}


generate_githeatmap <- function(df,title="title", subtitle = "", 
                                start_date = as.Date("2022-12-01"), 
                                end_date = as.Date("2023-01-31"), 
                                legend_title = "", type = "spotify") {
  
  Sys.setlocale("LC_ALL","English.utf8")
  k <- ifelse(type == "spotify", -1, -2)
  
  df <- df %>% group_by(date) %>% 
    summarise(N = n())
  
  days <- rep(0, end_date - start_date+1)
  
  
  max_N <- floor(max(df$N))
  first_tick <- round(max_N/3, k)
  
  df$N <- scale_fn(df$N)
  max_n <- max(df$N)
  days[df$date - start_date] = (df)$N
  
  
  p<-calendR(start_date = start_date,  # Start date
          end_date = end_date,    # End date
          title = title,  # Title
          subtitle = subtitle,  # Title
          start = "M",          # Start on Mondays 
          special.days = days,
          low.col = "black",
          gradient = TRUE,
          months.pos = 0.5,
          special.col = "#31a354", #przedostatni z gradientu green_colors
          day.size = 5,
          font.family = "Gotham",
          days.col = 'white',
          col = 'grey80',
          legend.pos = "right") +
    theme_spotify_calendar() +
    scale_fill_gradient(limits = c(0, max_n),
                        breaks = c(0, max_n/3, 2*max_n/3, max_n),
                        labels = c(0, first_tick, 2*first_tick, 3*first_tick),
                        low = "black",
                        high = "#219648",
                        name = legend_title,
                        guide = guide_colourbar(ticks = FALSE))
  #Sys.setlocale("LC_ALL", "Polish")
  p
}
# ?calendR
# generate_githeatmap(df_spotify)
# df_kompik <- read_kompik_data("tymciurymciu_kompik")
# df_spotify <- read_spotify_data("tymciurymciu")
# generate_githeatmap(df_spotify)

#df_kompik <- read_kompik_data("Wojtasauce (1)")
alluvial_merged <- function(df_merged) {
  df_merged %>%
    mutate(artistName = ifelse(substring(artistName,1,4) == "Jos,", "Jose Feliciano", artistName)) %>%
    group_by(energy_level, artistName, category) %>% 
    summarize(duration = sum(duration)) -> df1
  
    sum_duration <- sum(df1$duration)
    
    df1 %>% 
      group_by(artistName) %>%
      summarise(sum(duration)) %>%
      mutate(isGrouped = ifelse(`sum(duration)` >= sum_duration/10, FALSE,TRUE)) -> df_sum
    
    df_sum %>%
      inner_join(df1, by = 'artistName') -> df_pregroup
    
    #To musi by? takie g?upie bo artysta mo?e mie? r??ne energie :0
    df_pregroup %>%
      filter(isGrouped == TRUE) %>%
      group_by(isGrouped, energy_level, artistName) %>%
      summarise(suma = sum(duration)) -> df_postgroup
    #Wiem ze brzydko ale ladniej nie umiem
    Top_fast <- subset(df_postgroup, energy_level == 'fast')$artistName[which.max(subset(df_postgroup, energy_level == 'fast')$suma)]
    Top_medium <- subset(df_postgroup, energy_level == 'medium')$artistName[which.max(subset(df_postgroup, energy_level == 'medium')$suma)]
    Top_slow <- subset(df_postgroup, energy_level == 'slow')$artistName[which.max(subset(df_postgroup, energy_level == 'slow')$suma)]

    df_postgroup %>%
      group_by(energy_level) %>%
      summarise(liczba = n()) -> df_counter
    
    No_fast <- as.integer(df_counter[df_counter$energy_level=='fast',]$liczba[1]) - 1
    No_medium <-as.integer(df_counter[df_counter$energy_level=='medium',]$liczba[1]) - 1
    No_slow <-as.integer(df_counter[df_counter$energy_level=='slow',]$liczba[1]) - 1
    
    Block_fast <- paste0(Top_fast, " and ", No_fast, " others...")
    Block_medium <- paste0(Top_medium, " and ", No_medium, " others...")
    Block_slow <- paste0(Top_slow, " and ", No_slow, " others...")
    
    
    df_pregroup %>%
      mutate(artistName = ifelse(isGrouped == TRUE,
                                 case_when(energy_level == 'fast' ~ Block_fast,
                                           energy_level == 'medium' ~ Block_medium,
                                           energy_level == 'slow' ~ Block_slow),
                                 artistName)) -> dfinal_split #Gra s?owna
    dfinal_split %>%
      group_by(artistName, energy_level, category)%>%
      summarise(duration = sum(duration)) -> dfinal
    dfinal %>% 
    ggplot(aes(y = duration, axis1 = energy_level, axis2 = artistName)) +
    geom_alluvium(aes(fill = category), width = 1/24) +
    geom_stratum(width = 1/12, fill = "black", color = "grey") +
    geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
    scale_x_continuous(breaks = seq(1,2), labels = c("Music pace", "Artist Name"))+
    ylab("Seconds")+
    theme_spotify_base() 
  
}

energy_plot <- function(df_merged) {
  
  df <- df_merged %>% group_by(energy_level) %>% 
    summarize(n = n(), minutes = sum(duration)/60) %>% 
    mutate(windows_per_minute = n/minutes)
  
  df %>% 
    ggplot(aes(y=energy_level, x = windows_per_minute)) +
    geom_col(fill = green_colors[5]) +
    theme_spotify_base() +
    labs(title = "Frequency of changing windows and \n the energy of the listened music") +
    ylab("Energy level") +
    xlab("Average number of viewed windows per minute") +
    scale_x_continuous(expand=c(0,0), limits = c(0, 1.5))
    
}





