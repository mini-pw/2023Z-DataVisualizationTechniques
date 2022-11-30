df1 <- read.csv("HistoricalEsportData.csv")
df2 <- read.csv("GeneralEsportData.csv")
countries <- read.csv("countries.csv", sep = ";")
income <- read.csv("daily-median-income.csv")

library("dplyr")
library("stringr")
library("ggplot2")
library("maps")
library("scales")
library(ggtext)
library(patchwork)

world_coordinates <- map_data("world")

countries %>% mutate(Total.Prize.Money..Year. = gsub(",", ".", Total.Prize.Money..Year.)) %>% 
  mutate(Total.Prize.Money..Year. = as.numeric(Total.Prize.Money..Year.), Country = str_sub(Country, 2)) %>%
  mutate(Country = ifelse(Country == "Viet Nam", "Vietnam", Country), Country = ifelse(Country == "Russian Federation", "Russia", Country)) -> countries

countries %>%
  filter(Year == 2018) %>%
  mutate(s = Total.Prize.Money..Year. / Number.of.Players) -> c

income %>%
  filter(Year == 2018) %>%
  mutate(IncomePerYear = Median.income.or.expenditure.per.day * 365) -> inc

c %>%
  inner_join(inc, by = c("Country" = "Entity", "Year" = "Year")) %>%
  filter(!is.na(Code)) %>%
  mutate(k = s/IncomePerYear) %>%
  mutate(Country = ifelse(Country == "United States", "USA", Country), Country = ifelse(Country == "United Kingdom", "UK", Country)) -> map_frame

mapdata <- left_join(world_coordinates, filter(select(map_frame, Country, k), k < 5), by=c("region"="Country"))

#------------------------

map1 <- ggplot(mapdata, aes(x = long, y = lat, group=group)) +
  geom_polygon(aes(fill = k), color = "black")
map1

map2 <- map1 + scale_fill_gradient(low = "white", high = "#7fd00f", na.value = "#cbd6cc", labels = c("<1", "[1, 2)", "[2, 3)", "[3, 4]"), breaks = c(1, 2, 3, 4)) +
  labs(title = "Mapa zaleznosci srednich zarobkow w esporcie do sredniej krajowej, 2018r.", caption = "* wzgledny zarobek oznacza srednie zarobki esportowca w danym kraju dzielone przez srednie zarobki w calym kraju") +
  scale_y_continuous(expand = c(0, 0, 0, 0)) +
  scale_x_continuous(expand = c(0, 0, 0, 0)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill='transparent', color=NA),
        plot.caption = element_blank(),
        legend.title = element_text(size = 20, hjust = 0.3, face = "bold", colour = "white"),
        legend.key.size = unit(1.2, 'cm'),
        legend.background = element_rect(fill='transparent', color=NA),
        legend.text = element_text(size = 20, colour = "white"),
        legend.title.align = -0.5) + 
  guides(fill=guide_legend(title="Wzgledny zarobek*"))
map2


ggsave('map.png', map2, bg = 'transparent', width = 21, height = 11)

#--------------------------


df1 %>%
  mutate(Year = as.numeric(substr(Date, 1, 4))) %>%
  inner_join(df2, by=c("Game"="Game")) %>%
  filter(!is.na(Genre), Year >= 2000, Year <= 2020) %>%
  select(Game, Genre, Year, Earnings) %>%
  group_by(Genre, Year) %>%
  filter(Earnings != 0, Genre != "Battle Royale", Genre != "Puzzle Game") %>%
  summarise(SumEarnings = sum(Earnings)) %>%
  mutate(Genre = factor(Genre, levels = c("Multiplayer Online Battle Arena",
                                          "First-Person Shooter",
                                          "Strategy",
                                          "Sports",
                                          "Fighting Game",
                                          "Racing",
                                          "Collectible Card Game",
                                          "Third-Person Shooter",
                                          "Role-Playing Game"))) -> test1


ggplot(data = test1, mapping = aes(x = Year, y = SumEarnings)) +
  geom_point(size = 3.5) +
  geom_smooth(span = 0.5, color = "#54eb72", fill = "#9cf7ae", size = 2) +
  facet_wrap(~ Genre) +
  labs(title = "Suma zarobkow w turnejach esportowych w zaleznosci od gatunku gry od 2000r.", y = "Suma nagrod (USD)") +
  theme_bw() +
  theme(plot.title = element_text(size = 26, face = "bold", colour = "white"),
        axis.text.x = element_text(size = 38, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 40, face = "bold"),
        axis.title.y = element_text(size = 37, face = "bold"),
        strip.text = element_text(size = 31, face = "bold", colour = "white"),
        strip.background = element_rect(fill = "#7fd00f", color = "white"),
        axis.title = element_markdown(color = "white"),
        axis.text = element_markdown(color = "white"),
        panel.grid.major = element_line(color = "white"),
        panel.grid = element_line(color = "white"),
        panel.border = element_rect(color = "white", size= 2),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        axis.line = element_line(size = 1.3, color="white"),
        panel.spacing = unit(3, "lines")) +
  scale_x_continuous(expand = c(0.02, 0), labels = c(2000, 2010, 2020), breaks = c(2000, 2010, 2020)) +
  scale_y_log10(expand = c(0, 0), breaks = trans_breaks("log10", function(x) 10^x)(c(1, 1e8)),
                labels = trans_format("log10", math_format(10^.x))) -> plot9
plot9

ggsave('plot.png', plot9, bg = 'transparent', width = 26, height = 16)



