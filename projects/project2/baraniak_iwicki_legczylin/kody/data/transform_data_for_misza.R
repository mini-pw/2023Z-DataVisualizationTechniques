library(readr)

dfPC <- read_csv("./data/sources/Legczylin/data_pc_source.csv")
dfTel <- read_csv("./data/sources/Legczylin/data_tel_source.csv")

dfPC %>%
  mutate(
    type = case_when(
      str_detect(Name, 'YouTube|ELDEN|DiRT|League|TIDAL') ~ "Rozrywka",
      str_detect(Name, 'MATLAB|Microsoft|pdf|insta|Zoom|Wikipedia|Index|diagrams.net|Rstudio|Word') ~ "Praca",
      str_detect(Name, 'Facebook|Messenger') ~ "Komunikacja",
      str_detect(Name, 'Allegro') ~ "Zakupy",
      str_detect(Name, 'Chrome') ~ "Internet",
      TRUE ~ "Inne"
    ),
    `App name` = case_when(
      str_detect(Name, 'Messenger') ~ "Messenger",
      str_detect(Name, 'Facebook') ~ "Facebook",
      str_detect(Name, 'YouTube') ~ "YouTube",
      str_detect(Name, 'League') ~ "League of Legends",
      str_detect(Name, 'Allegro') ~ "Allegro",
      str_detect(Name, 'Microsoft') ~ "Teams",
      str_detect(Name, 'Chrome') ~ "Chrome",
      TRUE ~ Process
    ),
    Time = format(as.POSIXct(Start), format = "%H:%M:%S"),
    day = as.numeric(format(as.POSIXct(Start), format = "%d")),
    hour = as.numeric(format(as.POSIXct(Start), format = "%H")),
    Duration = as.numeric(Duration),
    dev = "Komputer",
    student = "Student 2"
  ) -> dfPC

print(head(dfPC))

dfTel %>%
  mutate(
    type = case_when(
      str_detect(`App name`, 'YouTube|Spotify|Instagram|Brave Nine|AFK Arena|Camera|Epic Seven') ~ "Rozrywka",
      str_detect(`App name`, 'ReadEra|TotalCommander|Calendar|Ukrainian-English dictionary|BlackNote|Translate|Bitwarden|Drive') ~ "Praca",
      str_detect(`App name`, 'Discord|Messenger|Phone|Messages|Facebook') ~ "Komunikacja",
      str_detect(`App name`, 'GOmobile') ~ "Zakupy",
      str_detect(`App name`, 'Chrome|Google|Opera') ~ "Internet",
      TRUE ~ "Inne"
    ),
    day = as.numeric(format(
      as.Date(Date, format = "%d/%m/%Y"), format = "%d"
    )),
    Time = as.character(Time),
    hour = as.numeric(sub(":.*", "", Time)),
    Duration = as.numeric(Duration),
    dev = "Smartphone",
    student = "Student 2"
  ) -> dfTel

print(head(dfTel))

write.csv(
  select(dfTel, `App name`, Time, Duration, day, hour, type, dev, student),
  "dane1.csv",
  row.names = FALSE
)
write.csv(
  select(dfPC, `App name`, Time, Duration, day, hour, type, dev, student),
  "dane2.csv",
  row.names = FALSE
)

read.csv("dane1.csv") -> dfTel
read.csv("dane2.csv") -> dfPC
bind_rows(dfTel, dfPC) -> dane

write.csv(dane, "./data/cleared/dane2.csv", row.names = FALSE)