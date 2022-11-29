library("dplyr") 
library("ggplot2")
library("sf")
library("spData")
library("maps")
library("mapdata")
library("usmap") 
library("ggmap")
library("colorspace")

PlayerData <- read.csv(file = "./data/zbiorLata50/player_data.csv")
Players <- read.csv(file = "./data/zbiorLata50/Players.csv")
SeasonsStats <- read.csv(file = "./data/zbiorLata50/Seasons_Stats.csv")
AllSeasonsStats <- read.csv(file = "./data/all_seasons.csv")
WorldBMI <- read.csv(file = "./data/worldBMI.csv")


View(PlayerData)
View(Players)
View(SeasonsStats)
View(AllSeasonsStats)
View(WorldBMI)

meanBMIyearNBA <- AllSeasonsStats %>% 
  select(player_weight, player_height, draft_year) %>% 
  filter(draft_year != "Undrafted") %>% 
  summarise(BMI = player_weight/((player_height/100)^2), year = draft_year) %>% 
  group_by(year) %>% 
  summarise(meanBMI = mean(BMI)) %>% 
  arrange(year)
  
View(meanBMIyearNBA)

meanBMIyearWorld <- WorldBMI %>% 
  select(Year, Sex, Mean.BMI) %>% 
  filter(Sex == "Men") %>% 
  select(Year, Mean.BMI) %>% 
  arrange(Year)

View(meanBMIyearWorld)
data <- merge(meanBMIyearWorld, meanBMIyearNBA, by.x = "Year", by.y = "year")
View(data)

data %>% 
ggplot(aes(x=Year)) +
  geom_line(aes(y= Mean.BMI, color = "world"), size=1)+
  geom_line(aes(y=meanBMI, color = "players"), size=1)+
  scale_color_manual(values = c("world"="darkblue", "players"="red"))+
  labs(title = "BMI among NBA players and world average", 
       x = "Year",
       y = "BMI") +
  theme_minimal()
  

### US MAP ###
register_google(key = "AIzaSyAvtTWN4sLVMp-Hs9W_KdFpQwOKf5NCCoc", account_type = "standard")


playersUni <- PlayerData %>% 
  select(college) %>% 
  filter(college != "") %>% 
  group_by(college) %>% 
  mutate( amount = n()) %>% 
  distinct() %>% 
  arrange(desc(amount))

#wczytywanie wszystkich wspolrzednych
#coordUni <- geocode(playersUni$college, output = "latlon", source = c("google"))

naUnis <- data.frame(playersUni, coordUni) %>% 
  filter_at(vars(lon, lat), all_vars(is.na(.)))

coordUniNames <- data.frame(playersUni, coordUni) %>% 
  filter_at(vars(lon, lat), all_vars(!is.na(.))) 

playersUniLimited <- playersUni %>% 
  head(10)
#kampusy w roznych miejscach
#coordUniLimited <- geocode(playersUniLimited$college, output = "more", source = c("google"))


#zapisywanie i odczyt ramki z googla
save(coordUniNames,file="coordUniNames.Rda")
load("coordUniNames.Rda")

coordUniNamesUsa <- coordUniNames %>% 
  filter(lon > -140 & lon < -65)


### STANY ###

## pointsDF: A data.frame whose first column contains longitudes and
##           whose second column contains latitudes.
##
## states:   An sf MULTIPOLYGON object with 50 states plus DC.
##
## name_col: Name of a column in `states` that supplies the states'
##           names.
lonlat_to_state <- function(pointsDF,
                            states = spData::us_states,
                            name_col = "NAME") {
  ## Convert points data.frame to an sf POINTS object
  pts <- st_as_sf(pointsDF, coords = 1:2, crs = 4326)
  
  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  states <- st_transform(states, crs = 3857)
  pts <- st_transform(pts, crs = 3857)
  
  ## Find names of state (if any) intersected by each point
  state_names <- states[[name_col]]
  ii <- as.integer(st_intersects(pts, states))
  state_names[ii]
}

## Use the function
View(coordUniNamesUsa)

testPoints <- coordUniNamesUsa %>% select(lon, lat)
us_states <- data.frame(lonlat_to_state(testPoints)) %>% 
  mutate(region=tolower(lonlat_to_state.testPoints.)) %>% 
  select(region)
View(us_states)  

#mutate_all(us_states, .funs=tolower)

View(coordUniNamesUsa)
View(us_states)
View(result)
result <- bind_cols(coordUniNamesUsa, us_states) %>% 
  rename("region" = 5) %>% 
  group_by(region) %>% 
  summarise(totalAmount = sum(amount)) %>%
  filter(!is.na(region))

### RZECZY Z TUTORIALU ###

View(result)
States <- map_data("state")
View(States)
MergedStatesAmount <- inner_join(States, result, by = "region")
View(MergedStatesAmount)

mapka <- ggplot() + 
  geom_polygon( data=MergedStatesAmount, 
                aes(x=long, y=lat, group=group, fill = totalAmount),
                color="white", size = 0.2 ) +
  scale_fill_continuous(name="Total Number per State",
                        low = "lightblue",
                        high = "darkblue",
                        limits = c(0,360),
                        breaks=c(60,120,180,240,300,360), na.value = "grey50") +
  # colorspace::scale_fill_continuous_sequential(palette = "Reds3",
  #                      #direction = 1,
  #                      limits = c(0,360),
  #                      breaks=c(40,80,120,160,200,240,280,320, 360), 
  #                      na.value = "grey50") +
  labs(title="Athletes from University to NBA") +
  geom_point(data=coordUniNamesUsa, aes(x=lon, y=lat,size = amount),
             color = "red", alpha = 0.5) +
  scale_size(name="Particular University \n Graduated") + 
  #theme_bw() +
  theme_void() +
  theme(axis.text.x=element_blank(),
                                                     axis.ticks.x=element_blank(),
                                                     axis.text.y=element_blank(),
                                                     axis.ticks.y=element_blank())
mapka
  
