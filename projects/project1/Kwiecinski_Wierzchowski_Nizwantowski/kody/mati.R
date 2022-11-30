# co to za kod jezeli nie jest podjebany

getwd()
options(stringsAsFactors = FALSE)

data_set_names <- c("circuits", "constructor_results", "constructor_standings", 
                    "constructors", "driver_standings", "drivers", 
                    "lap_times", "pit_stops", "qualifying", "races",
                    "results", "seasons", "sprint_results", "status")

for (el in data_set_names ){
  path <- paste("data/", el,".csv", sep = "")
  assign(el, read.csv(path))
}

library(dplyr)
library(ggplot2)



#  jakie wykresy chce zrobic

# ilosc wygranych wyscigow unormowane do rozpoczecia kariery
# srednie predkosci bolidow ferrari redbulla mclaren i mercedesa na przestrzeni lat (kolorem zaznaczone sezony w ktorych dany bolid byl najszybszy)
# najbogatsi kierowcy (slupkowy obr?cony)
# jak duze znaczenie ma poczatkowa pozycja (brak pomyslu na wykres) (moze finalne miejsce w zaleznosci od poczatkowego miejca)
# kto ma rekord na danym torze i kiedy zostal ustanowiony (fajnie sie laczy z srednie predkosci bolidow)
# jak wyglada statystycznie miejsce wygranego podczas wysciugu np od polowy malo juz sie dzieje bo pierwszy prawie zawsze zostaje pierwszy
# jak wygl?daja patterny pitstopow w sezonie 2021 ferrari redbulla mclaren i mercedesa

View(results)

#miejsce startowe a koncowa pozycja od 1950 do teraz
results %>%
  select(raceId, driverId, positionOrder, grid) %>%
  group_by(grid) %>%
  summarise(mean(positionOrder)) -> tmp

plot(tmp,xlab="pozycja startowa", ylab="finalnie miejsce", xlim=c(0, 27), ylim=c(0, 25))
abline(a=0, b=1, color = "r")


#miejsce startowe a koncowa pozycja od 2000 do teraz

races %>%
  select(raceId, year) -> lata

results %>%
  left_join(lata, by = "raceId") %>%
  filter(year < 2000) %>%
  select(raceId, driverId, positionOrder, grid) %>%
  group_by(grid) %>%
  summarise(mean(positionOrder)) -> miejsca_przed_2000

results %>%
  left_join(lata, by = "raceId") %>%
  filter(year > 2000) %>%
  select(raceId, driverId, positionOrder, grid) %>%
  group_by(grid) %>%
  summarise(mean(positionOrder)) -> miejsca_po_2000


plot(miejsca_przed_2000,xlab="pozycja startowa", ylab="finalnie miejsce", xlim=c(1, 27), 
     ylim=c(1, 21),col="blue", pch = 19)
points(miejsca_po_2000,col="red", pch = 19)
abline(a=0, b=1)

#mapka torow

install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)


circuits["rand_col"] = sample(1:4, length(circuits$name), replace=T)
# tak powinny wygladac kolory (po id kierowcow)
# colors <- c("1" = "#00c2cb", "20" = "#004b20", "830" = "#004aad", "30" = "#e20404")

# ale z racji ze jest ta randomowa implementacja do mapki to wygladaja tak
colors <- c("1" = "#00c2cb", "2" = "#004b20", "3" = "#004aad", "4" = "#e20404")
ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Mapa torow") + 
  geom_point(data= nowe_tory,aes(x=lng, y=lat, color = factor(rand_col)), size = 4)+
  scale_x_continuous(limits = c(-10, 31)) +
  scale_y_continuous(limits = c(35, 60)) + 
  scale_color_manual(values = colors) +  # tu ogarniam kolory
  theme_void() +
  theme(legend.position = "none")

nowe_wyscigi <- races %>%
  filter(year>1990)

najczestsze_tory <- nowe_wyscigi %>%
  count(circuitId) %>%
  arrange(desc(n))

najczestsze_tory <- head(najczestsze_tory,18)
najczestsze_tory

wybrane_tory <- circuits %>%
  filter(circuitId %in% najczestsze_tory$circuitId)

# tenisowy 

najlepsi_id <- drivers %>%
  filter(surname %in% c("Hamilton", "Schumacher", "Vettel", "Verstappen"))

#------------WYKRES ZWYCI?STW W KARIERZE----------------

# hamilton niebieski id=1
# schumacher czerwony id=30
# verstappen granatowy id=830
# vettel zielony id=20

# filtruje z results tylko tych kierowc?w co nas interesuj? i tylko pierwsze 
# miejsca
results %>%
  filter(driverId %in% c(1, 30, 830, 20) & position == 1) %>%
  select(driverId, raceId) -> df

# merge z races po raceid zeby miec rok
races %>%
  select(raceId, year) %>%
  merge(df) -> df

# grupuje sezonami i kierowcami, zliczam
df %>%
  group_by(year) %>%
  count(driverId) -> df

# dodanie danych prymitywnie ale szybko

# schumacher
df[nrow(df) + 1,] <- list(1991,30,0)
df[nrow(df) + 1,] <- list(2007,30,0)
df[nrow(df) + 1,] <- list(2008,30,0)
df[nrow(df) + 1,] <- list(2009,30,0)
df[nrow(df) + 1,] <- list(2010,30,0)
df[nrow(df) + 1,] <- list(2011,30,0)
df[nrow(df) + 1,] <- list(2012,30,0)

# vettel
df[nrow(df) + 1,] <- list(2007,20,0)
df[nrow(df) + 1,] <- list(2014,20,0)
df[nrow(df) + 1,] <- list(2016,20,0)
df[nrow(df) + 1,] <- list(2020,20,0)
df[nrow(df) + 1,] <- list(2021,20,0)
df[nrow(df) + 1,] <- list(2022,20,0)

# verstappen
df[nrow(df) + 1,] <- list(2015,830,0)

#hamilton
df[nrow(df) + 1,] <- list(2022,1,0)


df %>%
  arrange(year) %>%
  group_by(driverId) %>%
  mutate(cumsum(n)) -> df

#popraweczka
df$driverId <- as.character(df$driverId)

source("theme.R")

# hamilton niebieski id=1
# schumacher czerwony id=30
# verstappen granatowy id=830
# vettel zielony id=20
rok_urodzenia <- c("30" = 1969,"1" = 1985, "830" = 1997, "20" = 1987)
df["rok_urodzenia"] = rok_urodzenia[df$driverId]
df["wiek"] = df["year"] - df["rok_urodzenia"]
colors <- c("1" = "#00c2cb", "20" = "#004b20", "830" = "#004aad", "30" = "#e20404")

drivers <- drivers %>% 
  filter(driverId %in% driver_standings$driverId) %>% 
  mutate(driver.name = paste(forename, surname, sep = " ")) %>% 
  select(driverId, code, number, driver.name, dob, nationality, url) %>% 
  rename(driver.number = number)

#goodgame plot
df %>%
  merge(drivers) %>% 
  ggplot(aes(x = wiek, y = `cumsum(n)`, colour = driver.name)) + 
  geom_line(size = 1.5) + 
  geom_point(size = 3) +
  scale_color_manual(values = driver_colors) +
  theme_minimal() +
  labs(title = "Total numer of wins by age since career start", 
       x = "Age",
       y = "Number of wins by age", 
       caption = "- end of career")+
  theme_form() +
  theme(plot.caption = element_text(size=20, hjust = 1))-> zwyciestwa


zwyciestwa
ggsave('ostateczne/zwyciestwa.png', zwyciestwa, bg='transparent')

