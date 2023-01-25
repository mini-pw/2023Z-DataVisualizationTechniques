library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)

dane_pc <- read_csv("dane komputer.csv")
dane_tel <- read_csv("dane telefon.csv")

dane_pc%>%
  #mo¿na wymieniac rozne rzeczy oddzielajac je |
  mutate(type = case_when(
      str_detect(Name,'YouTube|ELDEN|DiRT|League|TIDAL') ~ "Rozrywka",
      str_detect(Name,'MATLAB|Microsoft|pdf|insta|Zoom|Wikipedia|Index|diagrams.net|Rstudio|Word') ~ "Praca",
      str_detect(Name,'Facebook|Messenger') ~ "Komunikacja",
      str_detect(Name,'Allegro') ~ "Zakupy",
      str_detect(Name,'Chrome') ~ "Internet",
      TRUE ~ "Inne"),
      # tworze odpowiednik dla danych na telefon gdy jak w nazwie jest jakas czesto uzywana strona czy inny wariant tej samej aplikacji
      # to w ta kolumne daje t¹ nazwe wpp jest Process
    `App name` = case_when(
        str_detect(Name,'Messenger') ~ "Messenger",
        str_detect(Name,'Facebook') ~ "Facebook",
        str_detect(Name,'YouTube') ~ "YouTube",
        str_detect(Name,'League') ~ "League of Legends",
        str_detect(Name,'Allegro') ~ "Allegro",
        str_detect(Name,'Microsoft') ~ "Teams",
        str_detect(Name,'Chrome') ~ "Chrome",
        TRUE ~ Process),
    Time = format(as.POSIXct(Start), format = "%H:%M:%S"),
    day = as.numeric(format(as.POSIXct(Start), format = "%d")),
    hour = as.numeric(format(as.POSIXct(Start), format = "%H")),
    Duration = as.numeric(Duration),
    dev = "Komputer",
    student = "Student 1")-> dane_pc


dane_tel%>%
  mutate(type = case_when(
      str_detect(`App name`,'YouTube|Netflix|TIDAL|Twitch') ~ "Rozrywka",
      str_detect(`App name`,'Multimedia|Teams|Outlook|Gmail|Pliki') ~ "Praca",
      str_detect(`App name`,'WhatsApp|Messenger|Telefon|Wiadomoœci') ~ "Komunikacja",
      str_detect(`App name`,'Allegro') ~ "Zakupy",
      str_detect(`App name`,'Chrome|Google') ~ "Internet",
      TRUE ~ "Inne"),
      # tu tlumacze te ekrany
      `App name` = case_when(
        `App name` == "Ekran w³¹czony (odblokowany)" ~ "Screen on (unlocked)",
        `App name` == "Ekran wy³¹czony (zablokowany)" ~ "Screen off (locked)",
        `App name` == "Ekran w³¹czony (zablokowany)" ~ "Screen on (locked)",
        `App name` == "Ekran wy³¹czony " ~ "Screen off",
        TRUE ~ `App name`),
    day = as.numeric(format(as.Date(Date,format="%d.%m.%Y"), format = "%d")),
    Time = as.character(Time),
    hour = as.numeric(sub(":.*", "", Time)),
    Duration = as.numeric(Duration),
    dev = "Smartphone",
    student = "Student 1")-> dane_tel

#co dziwne typy Time sie nie zgadzaja wiec wyszlo na to ze najszybciej jest zapisac obie dane do csv i skleic po ponownym wczytaniu
write.csv(select(dane_tel,`App name`, Time, Duration, day, hour, type, dev, student), "dane1.csv", row.names=FALSE )
write.csv(select(dane_pc, `App name`, Time, Duration, day, hour, type, dev, student), "dane2.csv", row.names=FALSE )

read.csv("dane1.csv")->dane_tel
read.csv("dane2.csv")->dane_pc
bind_rows(dane_tel, dane_pc) -> dane
write.csv(dane, "dane.csv", row.names = FALSE)
