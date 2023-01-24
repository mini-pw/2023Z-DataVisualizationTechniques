# w tym pliku b?d? rzeczy zeby przygotowac odpowiednio pliki do opublikowania


library(dplyr)
library(plotly)
library(lubridate) 
library(spotifyr)

source("scripts/scripts_tymek.R")
source("scripts/MergeTimestamps.R")



#####################
# spotify 



Sys.setenv(SPOTIFY_CLIENT_ID = 'b4a4fa3315e44ebda8701c684cc5f43e')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '605e78b16bee4a499dbce461cdf57e6d')

access_token <- get_spotify_access_token()

# ?adujemy pliki

PERSON_CHOICES <- c("Natalia", "Wojtek", "Tymek")
PERSON <- "Wojtek"


START_DATE <- as.Date("2022-12-01")
END_DATE <- as.Date("2023-01-31")


energy_levels <- seq(from = 0, to = 1, length.out = 4)
energy_labels <- c("slow", "medium", "fast")

df_spotify <- read_spotify_data(PERSON, sufix = "_raw_spotify") %>%
    filter(date %within% interval(START_DATE, END_DATE))


# dodajemy gatunki i inne rzeczy

# --------------------------------
# SKONSULTUJ Z GRABIASEM CZY ZMIENI? WARUNKI BO S? ZA OSTRE
# ---------------------------------
df_spotify <- add_audio_features(df_spotify) 

df_spotify <- df_spotify %>% 
  select(c(-uri, -type, -id, -track_href, -analysis_url, -duration_ms, -time_signature)) %>% 
  mutate(energy_level = cut(energy, breaks = energy_levels, 
                          labels = energy_labels, include.lowest = TRUE)) # add energy levels


write.csv(df_spotify, paste0("data/", PERSON, "_spotify.csv"), fileEncoding = "UTF-8")


########## 
# kompik



df_kompik <- read_kompik_data(PERSON, sufix = "_computer")
df_kompik <- add_categories_kompik(df_kompik) %>% 
  filter(date %within% interval(START_DATE, END_DATE)) #%>% 
  #select(c(-title))
  


write.csv(df_kompik, paste0("data/", PERSON, "_computer.csv"), fileEncoding = "UTF-8")




## dodawanie kategorii ju? jest w read_kompik_data


######### 
# merged


df_merged <- merge_by_timestamp(df_spotify, df_kompik)
write.csv(df_merged, paste0("data/", PERSON, "_merged.csv"), fileEncoding = "UTF-8")


#######
# dataframe for merged bumps


#ZLICZAMY MINUTY PRZED KOMPEM

df_afk1 <- read.csv("data/Natalia_afk2.csv")

df_afk1 %>% 
  filter(status=="not-afk") %>% 
  summarise(suma=sum(duration)) %>% 
  mutate(suma=suma/(3600))->df_afk1



#ZLICZAMY MINUTY PRZESLUCHANEJ MUZY

df_spotify1 <- read.csv("data/Wojtek_raw_spotify.csv")
df_spotify1 %>% 
  filter(substr(endTime, 1, 4)=='2022' & substr(endTime,6,7)=='12') %>% 
  summarise(suma= sum(msPlayed)) %>% 
  mutate(suma=suma/3600000)->df_spotify1

df_spotify1
