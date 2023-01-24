# in this file we're gonna write a function that merges two dataframes by timestamps and intervals

library(dplyr)
library(data.table)
library(stringi)

t <- 1

add_rows <- function(df) {
  df.expanded <- df[rep(row.names(df), df$duration / t), ] 
  df.expanded %>% mutate(iter = row.names(df.expanded)) %>% 
    mutate(iter = ifelse(stri_locate(pattern =".", iter, fixed = TRUE)[, 1] == -1, 0, iter)) %>% 
    mutate(iter = gsub("(.*)\\.","", iter)) %>% 
    mutate(iter = strtoi(iter)*t) %>% 
    mutate(duration_startTime = startTime + iter) %>% 
    select(c(-iter, -endTime)) # proly endTime is not necessary here anymore 
}

merge_by_timestamp <- function(df1, df2) {
  #df1 and df2 needs to have columns timestamp and duration
  
  df1 <- add_rows(df1)
  df2 <- add_rows(df2)
  df1 %>% 
    inner_join(df2, by=c("date", "duration_startTime")) %>%
    group_by(artistName, category, trackName, energy, startTime.x, energy_level, genre) %>%
    summarise(duration = n()) %>% 
    rename(startTime = startTime.x)
}


# df1 <- read_spotify_data("tymciurymciu")
# df2 <- read_kompik_data("tymciurymciu_kompik")
# 
# df1$duration <- 30
# 
# df <- merge_by_timestamp(df1, df2)
# 
# 
# length(unique(df2$timestamp))
