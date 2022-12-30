getwd()

library(dplyr)
library(ggplot2)

# ładujemy dane z pliku graduates-major-data
# poniewaz plik ten jest zbyt duzy nie uwzględniamy go w rozwiązaniu
graduates_major <- read.csv("data/graduates-major-data.csv", sep=";", dec = ",")


woj <- data.frame(P_WOJ = c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32),
                  WOJ_NAME = c("dolnośląskie", "kujawsko-pomorskie", "lubelskie", 
                           "lubuskie", "łódzkie", "małopolskie", "mazowieckie",
                           "opolskie", "podkarpackie", "podlaskie",
                           "pomorskie", "śląskie", "świętokrzyskie", 
                           "warmińsko-mazurskie", "wielkopolskie", "zachodniopomorskie"))

# wstępne przygotowanie danych do wykresów
df <- graduates_major %>% select(P_E_ZAR, P_WOJ, P_ROKDYP, P_POZIOM, P_KIERUNEK_NAZWA, P_NAZWA_UCZELNI, P_UCZELNIA_SKROT, P_DZIEDZINA) %>% 
  merge(woj) %>% 
  filter(P_DZIEDZINA != "")

write.csv(df, file = "prepared_data.csv")



## testowanie wykresu
write.csv(woj, file = "woj.csv")

str(df$P_ROKDYP)

woj[woj$id == 2, ]$name

as.list(woj)
list("10" = "dupa")
as.vector(t(woj))


unique(df$P_POZIOM)
unique(df$P_KIERUNEK_NAZWA)


df %>% 
  filter(P_)
  group_by(P_ROKDYP, P_DZIEDZINA) %>% 
  summarise(srednie_wynagrodzenie = mean(P_E_ZAR, na.rm=TRUE)) %>% 
  filter(P_DZIEDZINA != "") %>% 
  ggplot(aes(x = P_ROKDYP, y=srednie_wynagrodzenie, color = P_DZIEDZINA)) +
  geom_line() +
  labs(
    title = "Średnie wynagrodzenie absolwentów studiów z danej dziedziny",
    x = "Rok uzyskania dyplomu",
    y = "Średnie wynagrodzenie (w PLN)"
  ) + theme_bw()

summary(students)
