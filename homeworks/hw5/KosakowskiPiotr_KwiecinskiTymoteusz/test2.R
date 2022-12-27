getwd()

library(dplyr)
library(ggplot2)

rotatedAxisElementText = function(angle,position='x'){
  angle     = angle[1]; 
  position  = position[1]
  positions = list(x=0,y=90,top=180,right=270)
  if(!position %in% names(positions))
    stop(sprintf("'position' must be one of [%s]",paste(names(positions),collapse=", ")),call.=FALSE)
  if(!is.numeric(angle))
    stop("'angle' must be numeric",call.=FALSE)
  rads  = (angle - positions[[ position ]])*pi/180
  hjust = 0.5*(1 - sin(rads))
  vjust = 0.5*(1 + cos(rads))
  element_text(angle=angle,vjust=vjust,hjust=hjust)
}

# ładujemy dane z pliku graduates-major-data
# poniewaz plik ten jest zbyt duzy nie uwzględniamy go w rozwiązaniu
graduates_major <- read.csv("data/graduates-major-data.csv", sep=";", dec = ",")


# wstępne przygotowanie danych do wykresów
# df <- graduates_major %>% select(P_ROKDYP, P_CZAS_PRACA_DOSW, P_CZAS_PRACA_NDOSW, ) %>% 
#   merge(woj) %>% 
#   filter(P_DZIEDZINA != "")
woj <- data.frame(P_WOJ = c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32),
                  WOJ_NAME = c("dolnośląskie", "kujawsko-pomorskie", "lubelskie", 
                               "lubuskie", "łódzkie", "małopolskie", "mazowieckie",
                               "opolskie", "podkarpackie", "podlaskie",
                               "pomorskie", "śląskie", "świętokrzyskie", 
                               "warmińsko-mazurskie", "wielkopolskie", "zachodniopomorskie"))

# wstępne przygotowanie danych do wykresów
df <- graduates_major %>% select(P_CZAS_PRACA_DOSW, P_CZAS_PRACA_NDOSW, P_WOJ, P_ROKDYP, P_POZIOM, 
                                 P_KIERUNEK_NAZWA, P_NAZWA_UCZELNI, P_UCZELNIA_SKROT, P_DZIEDZINA) %>% 
  merge(woj) %>% 
  filter(P_DZIEDZINA != "")

write.csv(df, file = "prepared_data2.csv")



df %>% 
  mutate(dosw = "Tak",
         czas_praca = P_CZAS_PRACA_DOSW) %>% 
  select(!c(P_CZAS_PRACA_DOSW, P_CZAS_PRACA_NDOSW)) -> df_dosw
df %>% 
  mutate(dosw = "Nie",
         czas_praca = P_CZAS_PRACA_NDOSW) %>% 
  select(!c(P_CZAS_PRACA_DOSW, P_CZAS_PRACA_NDOSW)) -> df_ndosw

df <- rbind(df_dosw, df_ndosw)


# df %>% 
#   filter("Administracja" %in% as.list(strsplit(P_KIERUNEK_NAZWA, " ")[[1]]) || 
#            "Technologia" %in% as.list(strsplit(P_KIERUNEK_NAZWA, " ")[[1]]) ) -> x
region <- c(14,22,2)
rok <- c(2020, 2018, 2017)
write.csv(df, "prepared_data_plot2.csv")

df %>% 
  # filter(P_WOJ %in% region,
  #        P_ROKDYP %in% rok) %>% 
  group_by(P_DZIEDZINA, P_ROKDYP, dosw, P_WOJ, WOJ_NAME) %>% 
  summarise(sredni_czas = mean(czas_praca, na.rm = T)) %>% 
  mutate(P_DZIEDZINA = substring(P_DZIEDZINA, 10)) -> df

df %>% 
  ggplot(aes(fill = dosw, y = sredni_czas, x = P_DZIEDZINA)) +
  geom_col(position="dodge") +
  labs(
    title = "Średni czas (w miesiącach) od uzyskania dyplomu do podjęcia pierwszej pracy po 
             uzyskaniu dyplomu wśród absolwentów studiów z danej dziedziny",
    x = "Dziedzina",
    y = "Średni czas (w miesiącach)"
  ) + 
  theme_bw() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
  # theme(axis.text.x = rotatedAxisElementText(-30,'x'))
  
  
  
  


summary(students)