zawodnicy <-read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_players.csv")
kody <- read.csv("https://raw.githubusercontent.com/johnashu/datacamp/master/medals/Summer%20Olympic%20medalists%201896%20to%202008%20-%20IOC%20COUNTRY%20CODES.csv")
atp01<-read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2001.csv")
atp02<-read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2002.csv")
atp03<-read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2003.csv")
atp04<-read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2004.csv")
atp05<-read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2005.csv")
atp06<-read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2006.csv")
atp07<-read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2007.csv")
atp08<-read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2008.csv")
atp09<-read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2009.csv")
atp10<-read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2010.csv")
atp11<-read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2011.csv")
atp12<-read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2012.csv")
atp13<-read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2013.csv")
atp14<-read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2014.csv")
atp15<-read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2015.csv")
atp16<-read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2016.csv")
atp17<-read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2017.csv")
atp18<-read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2018.csv")
atp19<-read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2019.csv")
atp20<-read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2020.csv")
atp21<-read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2021.csv")
head(atp17)

library(ggplot2)
library(dplyr)
library(tidyverse)
library(data_map)


df_list <- list(atp21, atp20, atp19,atp18,atp17,atp16,atp15,atp14,atp13,atp12,atp11,atp10,atp09,atp08,atp07,atp06,atp05,atp04,atp03,atp02,atp01)
x <-Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
#przefiltrujemy zawodnikóW, aby mieć tylko tych, których pochodzenie znamy.
zawodnicy_kraje <- zawodnicy%>%
  filter(ioc != "")
#teraz potrzebujemy polaczyc zawodnikow po id z nazwami ich krajów
zawodnicy_kody <- merge(zawodnicy_kraje,kody, by.x = "ioc", by.y = "NOC", all = TRUE)
#teraz wyciagamy z zawodow kraje zawodnikow ktorzy byli zwyciezcami 
merged2 <- merge(zawodnicy_kody,x,by.x = "player_id", by.y = "winner_id")
zwyciezcy <- merged2[,c(1,2,9)]
#merged2[, .(count = .N, var = sum(player_id)), by = Country]

values <-zwyciezcy %>% group_by(Country) %>%tally()
values <- values[order(values$n, decreasing = TRUE),] #liczba zwyciezcow z danego kraju (we wszystkich meczach)
values <- values%>%drop_na(Country)
values$Country[values$Country == "United States"] <- "USA"
values$Country[values$Country == "United Kingdom"] <- "UK"

?map_data
worldData <- map_data('world') %>% filter(region != "Antarctica") 
view(worldData)
#mapData <- mapData[order(c(mapData$region, mapData$order), decreasing = FALSE),]
mapData <- left_join(worldData,values, by = c("region" = "Country"))



 theme_set(theme_minimal())
 
map <-  ggplot(mapData, aes( x = long, y = lat, group = group)) +
   geom_polygon(aes(fill = n))
map <- map + scale_fill_continuous(low="#f1f1f2",high="#56B1F7",
 na.value = "grey50",name = "number of wins in 2000-2021",horizontal = TRUE)
map + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
