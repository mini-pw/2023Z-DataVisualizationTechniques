#https://www.kaggle.com/datasets/hwaitt/horse-racing

horses_2020 <- read.csv("horses_2020.csv")
horses_2020 <- horses_2020[, -28]
horses_files <- list.files(pattern = "horses_201[0-9]+\\.csv")
horses_data <- lapply(horses_files, read.csv)
horses_data <- rbind(horses_data, horses_2020)
horses_data <- data.table::rbindlist(horses_data)

library("ggplot2")
library("dplyr")
library("grid")
library("scales")
library(gridExtra)
library(showtext)
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("tidyr")

  

#Wykres zale¿noœci ukoñczonych wyœcigów od wieku konia oraz liczba koni bior¹ca udzia³ w wyœcigach

u <- horses_data %>%
  select(age) %>%
  filter(age > 1) %>%
  group_by(age) %>%
  summarize(n = n())

x <- horses_data %>%
  select(age, position) %>%
  filter(age > 1, position  == 40) %>%
  transmute(age = ifelse(age >= 12, "12+", age)) %>%
  group_by(age) %>%
  summarize(n = n())

y <- horses_data %>%
  select(age, position) %>%
  filter(age > 0, age <= 12, position != 40) %>%
  transmute(age = ifelse(age >= 12, "12+", age)) %>%
  group_by(age) %>%
  summarize(n = n())

z <- inner_join(x, y, by = "age") %>%
  mutate(percentage = (n.x/n.y)*100)

options(scipen = 999)
french = function(x) format(x, big.mark = " ")

g1 <- ggplot(y, aes(x = age, y = n)) +
  geom_bar(stat = "identity", fill = "#60789b") + 
  labs(x ="") +
  scale_x_discrete(limit = c(as.character(2:11), "12+"),expand = c(0,0)) +
  theme(plot.title = element_text(size = 30, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill='transparent'), 
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        plot.margin = unit(c(1,-1,1,0), "mm")) +
  scale_y_reverse(labels = scales::label_number()) +
  coord_flip()



g2 <- ggplot(z, aes(age, percentage)) +
  geom_bar(stat = "identity", fill = "#ff9829") +
  scale_x_discrete(limit = c(as.character(2:11), "12+"), expand = c(0,-0.6)) +
  scale_y_continuous(limits = c(0,40), breaks = c(0, 10, 20, 30, 40), labels = function(x) paste0(x, "%")) +
  xlab(NULL) + 
  theme(plot.title = element_text(size = 30, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 16, face = "bold"),
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        plot.margin = unit(c(1,0,1,-1), "mm")) +
  coord_flip()



wykres <- grid.arrange(g1, g2, ncol=2)


#Mapa iloœci wyœcigów w poszczególnych krajach

read.csv("races_2010.csv") %>% select(c(rid, countryCode)) -> d2010
read.csv("races_2011.csv") %>% select(c(rid, countryCode)) -> d2011
read.csv("races_2012.csv") %>% select(c(rid, countryCode)) -> d2012
read.csv("races_2013.csv") %>% select(c(rid, countryCode)) -> d2013
read.csv("races_2014.csv") %>% select(c(rid, countryCode)) -> d2014
read.csv("races_2015.csv") %>% select(c(rid, countryCode)) -> d2015
read.csv("races_2016.csv") %>% select(c(rid, countryCode)) -> d2016
read.csv("races_2017.csv") %>% select(c(rid, countryCode)) -> d2017
read.csv("races_2018.csv") %>% select(c(rid, countryCode)) -> d2018
read.csv("races_2019.csv") %>% select(c(rid, countryCode)) -> d2019
read.csv("races_2020.csv") %>% select(c(rid, countryCode)) -> d2020

df <- rbind(d2010,d2011,d2012,d2013,d2014,d2015,d2016,d2017,d2018,d2019,d2020)
df %>% group_by(countryCode) %>% summarise(count = n())-> df
df %>% filter(count > 100) -> df

df[df == "IE"] <- "IRL"
df[df == "JP"] <- "J"
df[df == "GER"] <- "D"
df[df == "SAF"] <- "ZA"
df[df == "ARAB"] <- "AE"
df[df == "CAN"] <- "CA"
df[df == "JER"] <- "JE"
df[df == "ARG"] <- "AR"
df[df == "BRZ"] <- "BR"
df[df == "IT"] <- "I"
df[df == "FR"] <- "F"
df2 <- df %>% mutate(log = log(count,10))

theme_set(theme_bw())

world <- ne_countries(scale = "small", returnclass = "sf")
world <- world %>%  filter(name != "Antarctica")
left_join(world,df2,by =c("postal"="countryCode") ) -> w1

ggplot(data = w1) + geom_sf(aes(fill = log)) + theme_void() +
  theme(legend.position = c(0.095, 0.32),legend.title = element_blank()) +
  theme(legend.background = element_rect(fill = "#BCDDF2",colour= 1,color = "#BCDDF2"))+
  #theme(legend.margin = element_rect(colour= 1,color = "grey95"))+
  scale_fill_gradientn(colours=c('#FFFFD4','#FED98E','#FE9929','#CC4C02'),na.value = "grey95",
                       breaks=c(2,3,4,5),labels=c("100","1000","10000","100000"),
                       limits=c(1.75,5.04)) + 
  ggtitle("  Popularnoœæ wyœcigów konnych w latach 2010-2020",
          subtitle = "  mierzona przez liczbê odbytych gonitw na terenie kraju")+
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold")) + 
  theme(plot.subtitle = element_text(size = 14, face = "bold", hjust = 0.5)
  )-> wykresmapa
wykresmapa
ggsave('wykresmapa.png',wykresmapa, bg = 'transparent')


#Szansa na zwyciêstwo faworyta

Sys.setlocale(category = "LC_ALL", locale = "Polish")
df2 <- horses_data %>%
  filter(isFav == 1,age > 0,position > 0) %>%
  filter(horseName %in% c("Winx", "Pour La Victoire", "Un De Sceaux", "Cirrus Des Aigles", "Captain Lars")) %>%
  group_by(horseName) %>%
  mutate(position = case_when(position > 1 ~ 2,TRUE ~ as.numeric(position))) %>%
  count(position) %>%
  mutate(percentage = (n/sum(n)*100)) %>%
  filter(position == 1) %>%
  select(horseName, percentage) %>%
  ungroup()%>%
  ggplot(aes(reorder(horseName, -percentage), y = percentage)) +
  geom_point(size=6, color="#feb24c", fill=alpha("#fdbb84", 1), alpha=1, shape=21, stroke=2) +
  geom_segment( aes(x=reorder(horseName, -percentage), xend = reorder(horseName, -percentage), y=0,yend=percentage), color = "#feb24c", lwd = 1.5) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = c(0,0),breaks = scales::breaks_extended(n=5),
                     limits = c(0,105))+
  theme(
    axis.text = element_text(size = 13, face = "bold"),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent",
                                   color = NA),
    panel.grid.major.x = element_line(size = 0.65),
    panel.grid.minor.x = element_line(size=0.3),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.subtitle = element_text(size = 13,hjust = 0.45),
    plot.title = element_text(size = 15, face = "bold",hjust=0)) +
  ggtitle("Szansa na zajecie pierwszego miejsca dla pieciu najczestszych faworytów",
          subtitle = "ilosc wygranych/ilosc startów jako faworyt") +
  labs(x = "",y="")+
  coord_flip()













  

