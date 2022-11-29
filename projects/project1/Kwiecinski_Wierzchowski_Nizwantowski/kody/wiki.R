# sprobuje tenisistowy graf zrobic

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

najlepsi_id <- drivers %>%
  filter(surname %in% c("Hamilton", "Schumacher", "Vettel", "Verstappen"))

#------------WYKRES ZWYCIÊSTW W KARIERZE----------------

# hamiltonId =1
# schumacher Id=30
# verstappen Id=830
# vettel Id=20
head(df)

# filtruje z results tylko tych kierowców co nas interesuj¹ i tylko pierwsze 
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
  
df %>%
  group_by(driverId) %>%
  mutate(cumsum(n)) -> df

# okazuje sie ze numeryczne driverId sie nie spisuje X,D 
df %>%
  ggplot(aes(x = year, y = `cumsum(n)`, colour = driverId)) + 
  geom_line() + 
  geom_point() 

#popraweczka
df$driverId <- as.character(df$driverId)

#goodgame plot
df %>%
  ggplot(aes(x = year, y = `cumsum(n)`, colour = driverId)) + 
  geom_line() + 
  geom_point() +
  labs(title = "liczba zwyciêzonych wyœcigów na przestrzeni kariery", 
       x = "sezony",
       y = "sumaryczna liczba zwyciêstw") +
  xlim(1990, 2022)

#------------WYKRES DLUGOSCI PITSTOPÓW----------------
head(df)
head(x)
head(y)

# merge resoults i races po raceid
races %>%
  select(raceId, year) -> x

results %>%
  select(raceId, driverId, constructorId) -> y

x %>%
  merge(y) -> df

# filter 2021
df %>%
  filter(year == 2021) %>%
  select(raceId, driverId, constructorId)-> df

# merge df i constructors po constructorid
constructors %>%
  select(constructorId, name) -> x

df %>%
  merge(x) %>%
  select(raceId, driverId, name)-> df

# merge df i pitstops po driver id
pit_stops %>%
  select(raceId, driverId, milliseconds) -> y
  
df %>%
  merge(y) %>%
  select(name, milliseconds) -> df

# select name(constructor), duration
df %>%
  mutate(len = milliseconds/1000) %>%
  select(name, len) %>%
  filter(len < 100) -> df

df %>% 
  ggplot(aes(x = len, y = name, fill = name)) +
  geom_violin()

df %>% 
  ggplot(aes(x = len, y = name, fill = name)) +
  geom_boxplot()
# ===========================================TORY
head(circuits)

circuits %>%
  filter(name == "Circuit of the Americas") %>%
  select(circuitId)
cota <- 69

circuits %>%
  filter(circuitRef == "imola") %>%
  select(circuitId)
imola <- 21

circuits %>%
  filter(name == "Autódromo José Carlos Pace") %>%
  select(circuitId)
interlagos <- 18

circuits %>%
  filter(name == "Circuit Gilles Villeneuve") %>%
  select(circuitId)
montreal <- 7

circuits %>%
  filter(name == "Autodromo Nazionale di Monza") %>%
  select(circuitId)
monza <- 14

circuits %>%
  filter(name == "Red Bull Ring") %>%
  select(circuitId)
redBullRing <- 70

circuits %>%
  filter(name == "Silverstone Circuit") %>%
  select(circuitId)
silverstone <- 9

circuits %>%
  filter(name == "Circuit de Spa-Francorchamps") %>%
  select(circuitId)
spa <- 13

circuits %>%
  filter(circuitRef == "suzuka") %>%
  select(circuitId)
suzuka <- 22

circuits %>%
  filter(circuitRef == "zandvoort") %>%
  select(circuitId)
zandvoort <- 39

myCircuits <- c(cota, imola, interlagos, montreal, monza, redBullRing, silverstone,
                suzuka, spa, zandvoort)

hamiltonId = 1
schumacherId = 30
verstappenId = 830
vettelId = 20

myDrivers <- c(hamiltonId, schumacherId, verstappenId, vettelId)

head(races)
head(results)

myData <- full_join(races, results, by = "raceId") %>%
  select(raceId, circuitId, driverId, position) %>%
  filter(driverId %in% myDrivers) %>%
  filter(circuitId %in% myCircuits)

head(myData)

won <- myData %>%
  filter(position == 1) %>%
  group_by(circuitId) %>%
  count(driverId) %>%
  rename(won = n)

total <- myData %>%
  group_by(circuitId) %>%
  count(driverId) %>%
  rename(total = n)

answ <- full_join(won, total, by = c('circuitId' = 'circuitId', 
                                     'driverId' = 'driverId')) %>%
  mutate(ratio = won / total)

answ
