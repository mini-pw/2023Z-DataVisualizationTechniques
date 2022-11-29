# siemanko witam w mojej kuchni

# tutaj robimy triki jakies no

getwd()
source("mega_wazny_plik.R")
source("theme.R")

library(dplyr)
library(ggplot2)
library(scales)
library(tidyquant)
library(ggthemes)
library(tidyr)


# pobieranie zbioróW
options(stringsAsFactors = FALSE)

data_set_names <- c("drivers", "driver_standings", "races", "results",
                    "seasons", "sprint_results", "status", "lap_times", 
                    "earnings_2021", "total_earnings")

el <- data_set_names[1]

for (el in data_set_names ){
  path <- paste("data/", el, ".csv", sep = "")
  assign(el, read.csv(path))
}


drivers <- drivers %>% 
  filter(driverId %in% driver_standings$driverId) %>% 
  mutate(driver.name = paste(forename, surname, sep = " ")) %>% 
  select(driverId, code, number, driver.name, dob, nationality, url) %>% 
  rename(driver.number = number)

results <- results %>% 
  select(resultId, raceId, driverId, milliseconds, position)

# czyscimy te dziwne \N
results <- results[!is.na(as.numeric(as.character(results$milliseconds))),]
results <- results[!is.na(as.numeric(as.character(results$position))),]
#df %>% transform(position = ifelse(position == "\N", ))
results$position <- as.numeric(results$position)
results$milliseconds <- as.numeric(results$milliseconds)



# pierwsza analiza testowa
# zrobimy sobie dla 4 wybranych zawodników porównanie œrednich prêdkosci w ka¿dym sezonie

# SREDNIE PREDKOSCI SEZONOWE


df <- results %>%  merge(drivers) %>% 
  filter(driver.name %in% our_drivers) %>% 
  merge(races, by.x = "raceId", by.y="raceId") %>% 
  mutate(date = as.Date(date))
  #mutate(date = lubridate::floor_date(as.Date(date), "month"))

str(df$date)

df <- df %>% 
  group_by(driver.name, date) %>% 
  summarize(avg_time = mean(milliseconds/(1000)), avg_position=mean(position))

df %>% 
  ggplot() +
  aes(y=avg_time, x=date, color=driver.name) %>% 
  geom_ma(size = 1.5, se=FALSE, linetype=1, n=50) +
  labs(title="Circuits domination",
        y = "œredni czas w sekundach",
        x = "rok", caption = "Races won / total driven races") +
  theme_form() +
  scale_color_manual(values = driver_colors) +
  # xlim(2005, 2022) +
  scale_y_reverse()



head(df)

# porownanie srednich pozycji zawodników

source("theme.R")
df %>% filter(date >= c(as.Date("01/01/98", "%d/%m/%y"))) %>%  #, as.Date("01/01/25", "%d/%m/%y"))) %>% 
  ggplot() +
  aes(y = avg_position, x = date, color = driver.name) +
  geom_ma(n=12, linetype = 1, size=1.5) +
  labs(title = "Average positions",
       subtitle = "at Grand Prix competitions over the years",
       y = "Average position",
       x = "Year",
       color = "Driver") +
  scale_color_manual(values = driver_colors) +
  scale_y_reverse(breaks = c(10, 8, 6, 4, 2)) +
  theme_form() -> srednia_poz


srednia_poz
ggsave('ostateczne/srednia_final.png', srednia_poz, bg='transparent')

# ZAROBKI


head(earnings_2021) # te dane sa z https://www.statista.com/statistics/1255926/formula-one-salaries/
head(total_earnings) # te dane s¹ z https://www.scmp.com/magazines/style/celebrity/article/3182964/10-richest-f1-drivers-all-time-net-worths-ranked-lewis
# oraz dla verstappena https://www.spotrac.com/formula1/oracle-red-bull-racing/max-verstappen-47373/cash-earnings/

total_earnings %>% 
  merge(drivers, by.x="name", by.y="driver.name") %>% 
  mutate(name = reorder(name, earnings)) -> df_prep
df_prep %>% 
  ggplot() +
  aes(x=earnings, y=name, fill=name) +
  geom_bar(stat="identity", width = ifelse(df_prep$name %in% our_drivers, 0.9,0.9)) +
  # zarzucona zmiana szerokoœci s³upków - wygl¹da to dziwnie
  labs(title="Net worths of 10 richest F1 drivers",
       subtitle = "for year 2022",
       y="",
       x = "Net worths (in mln $)") +
  scale_fill_manual(values = driver_colors, na.value = "#333333") +
  theme_form() +
  theme(panel.grid.major.y = element_blank())  -> piniondz


piniondz

ggsave('ostateczne/piniondz.png', piniondz, bg='transparent')



# wyprzedzanie
races %>% 
  filter(year == 2021) %>% 
  rename(circuit = name) -> races

drivers <- drivers %>% 
  filter(driverId %in% driver_standings$driverId) %>% 
  mutate(driver.name = paste(forename, surname, sep = " ")) %>% 
  select(driverId, code, number, driver.name, dob, nationality, url) %>% 
  rename(driver.number = number)

gp <- races %>% select(raceId, round, circuit)
dr <- drivers %>% select(driverId, driver.name, code, driver.number)

lap_times %>%  left_join(gp, by = "raceId") %>%
  left_join(dr, by = "driverId")%>% 
  select(-raceId, -driverId) -> lp

lp %>% filter(driver.name == "Robert Kubica", circuit != "<NA>")

lp %>% 
  filter(circuit == "Abu Dhabi Grand Prix") %>% # wybieramy sobie grand prix
  mutate(colours = case_when( # dodajemy kolorki
    driver.name == "Lewis Hamilton" ~ Hamilton,
    driver.name == "Max Verstappen" ~ Verstappen,
    driver.name == "Sebastian Vettel" ~ Vettel,
    TRUE ~ "#f0f0f0"
  )) %>% 
  ggplot(aes(x=lap, y=position, group=driver.name, color=colours, alpha)) +
  geom_line(size=2) +
  labs(title = "Pozycje zawodników na Abu Dhabi Grand Prix 2021", 
       y = "Pozycja",
       x = "Okr¹¿enie") 
  
