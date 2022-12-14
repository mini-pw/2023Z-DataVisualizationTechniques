
library(dplyr)
library(ggplot2)
library(ggrepel)
library(shinycssloaders)
library(plotly)
library(tidyr)



df_uni <- read.csv("graduates-institution-data.csv", sep=';')
df_stu <- read.csv("graduates-major-data.csv", sep=';')

df_stu$P_ME_ZAR <- as.double(gsub(",", ".",df_stu$P_ME_ZAR))
df_stu$P_E_ZAR_P5 <- as.double(gsub(",", ".",df_stu$P_E_ZAR_P5))
df_stu$P_E_ZAR_P3 <- as.double(gsub(",", ".",df_stu$P_E_ZAR_P3))
df_stu$P_E_ZAR <- as.double(gsub(",", ".",df_stu$P_E_ZAR))


df_uni$U_E_ZAR_P5 <- as.double(gsub(",", ".",df_uni$U_E_ZAR_P5))
df_uni$U_E_ZAR_P3 <- as.double(gsub(",", ".",df_uni$U_E_ZAR_P3))


df_to_show <-
  df_stu %>% 
  select(P_KIERUNEK_NAZWA,P_UCZELNIA_SKROT, P_ROKDYP,P_E_ZAR_P3, P_E_ZAR_P5, P_E_ZAR)  %>%
  filter(!(is.na(P_KIERUNEK_NAZWA) | P_KIERUNEK_NAZWA==""))  %>% 
  group_by(P_KIERUNEK_NAZWA, P_ROKDYP) %>% 
  summarise(srednia_p3 = mean(P_E_ZAR_P3, na.rm = TRUE),
            srednia_p5 = mean(P_E_ZAR_P5, na.rm = TRUE),
            srednia_p = mean(P_E_ZAR, na.rm = TRUE)) %>% 
  arrange(P_KIERUNEK_NAZWA) 

df_to_show <- df_to_show %>%  drop_na()