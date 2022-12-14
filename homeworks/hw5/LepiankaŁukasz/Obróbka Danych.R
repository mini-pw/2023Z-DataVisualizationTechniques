library(dplyr)
library(ggplot2)

df <- read.csv(file = "data/graduates-major-data.csv", sep = ";",encoding = "UTF-8", dec = ",")

df %>%
  select(P_NAZWA_UCZELNI, P_UCZELNIA_SKROT, P_KIERUNEK_NAZWA, P_KIERUNEK_ID,
         P_POZIOM, P_FORMA, P_WOJ, P_E_ZAR_ETAT, P_E_ZAR_ETAT_DOSW, P_E_ZAR_ETAT_NDOSW, P_NAZWA_JEDN, P_ROKDYP) -> wynik
  mutate(wynik,Jednostka = paste(P_NAZWA_UCZELNI,P_NAZWA_JEDN, sep = "\n")) %>% 
    mutate(Jednostka.Skrot= paste(P_NAZWA_JEDN, P_UCZELNIA_SKROT, sep = "-")) %>% 
    mutate(Kierunek.Rok = paste(P_KIERUNEK_NAZWA, Jednostka.Skrot, P_POZIOM, P_ROKDYP, sep = "\n"))->wynik

#Testowanie
wynik %>%
  filter(P_WOJ %in% list(8)) %>% 
  group_by(Jednostka, P_NAZWA_UCZELNI) %>%
  summarise(Srednia = mean(P_E_ZAR_ETAT)) %>% 
  arrange(-Srednia) %>% 
  head(10) %>% 
  inner_join(y = wynik, by = "Jednostka") %>%  
  filter(P_WOJ %in% list(8))-> tmp
rm(tmp,df)