library(dplyr)
library(ggplot2)
library(forcats)
library(ggflags)
library(countrycode)

df <- read.csv("athlete_events.csv")
top_athletes <- df %>% filter(!is.na(Medal), Season == "Summer") %>% count(Name) %>% arrange(-n) %>% head(10) %>% select(Name)
top_athletes <- unlist(top_athletes)

gold <- df %>% filter(Medal == "Gold", Name %in% top_athletes) %>% add_count(Name) %>% distinct(Name, n, Medal)
colnames(gold)[2] = "Medals"


silver <- df %>% filter(Medal == "Silver", Name %in% top_athletes) %>% add_count(Name) %>% distinct(Name, n, Medal)
colnames(silver)[2] = "Medals"

bronze <- df %>% filter(Medal == "Bronze", Name %in% top_athletes) %>% add_count(Name) %>% distinct(Name, n, Medal)
colnames(bronze)[2] = "Medals"

top_athletes <- gold %>% add_row(silver) %>% add_row(bronze)

df1 <- mutate(top_athletes, newNames=case_when(
  Name=="Michael Fred Phelps, II"~"Michael Phelps",
  Name=="Larysa Semenivna Latynina (Diriy-)"~"Larisa Latynina",
  Name=="Nikolay Yefimovich Andrianov"~"Nikolai Adrianov",
  Name=="Borys Anfiyanovych Shakhlin"~"Boris Shakhlin",
  Name=='Jennifer Elisabeth "Jenny" Thompson (-Cumpelik)'~"Jenny Thompson",
  Name=="Dara Grace Torres (-Hoffman, -Minas)"~"Dara Torres",
  Name=="Birgit Fischer-Schmidt"~"Birgit Schmidt",
  Name=="Aleksey Yuryevich Nemov"~"Alexei Nemov",
  TRUE~as.character(Name)
))

df1

top_athletes$Name = df1$newNames

colnames(top_athletes)[1] = "Name"
top_athletes

top_athletes <- top_athletes %>% mutate(Name = fct_reorder(Name, n, .fun = sum))

plot <- ggplot(top_athletes, aes(fill=factor(Medals, levels = c("Bronze", "Silver", "Gold")), y=n, x=Name, label = n)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(size = 10, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c('brown','gray','gold')) +
  labs(title = "", x = "", y = "", fill = "") +
  coord_flip() +
  theme_bw() +
  theme_minimal() +
  theme(axis.line = element_line(color = 'black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "black"),
        legend.position = "none",
        axis.text = element_text(size = 30))

plot

ggsave(filename = "Medalists.png", bg = "transparent", width = 25, height = 20, units = "cm")

medals <- df %>% filter(!is.na(Medal)) %>% count(Team)
colnames(medals)[2] = "Number_of_medals"

competitors <- df[!duplicated(df$Name), ]
competitors <- competitors %>% count(Team)
colnames(competitors)[2] = "Number_of_competitors"


olympics <- df %>% select(Team, Games) %>% distinct() %>% count(Team)
colnames(olympics)[2] = "Number_of_olympics"

bubble <- merge(medals, competitors)
bubble <- merge(bubble, olympics)
bubble <- bubble %>% arrange(-Number_of_competitors) %>% head(10)

columns <- bubble$Team
bubble <- bubble[!grepl("[0-9]", columns),]
bubble$Country_code <- tolower(countrycode(bubble$Team, origin = "country.name", destination = "genc2c"))

plot <- ggplot(bubble, aes(x=Number_of_competitors, y=Number_of_medals, size = Number_of_olympics, country = Country_code)) +
  geom_flag() +
  scale_country() +
  scale_size(range = c(10, 20), name = "Number of Olympics") +
  labs(title = "", x = "", y = "", fill = "") +
  theme_bw() +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "black"),
        axis.text = element_text(size = 30))+
  scale_country(guide ="none")

plot

ggsave(filename = "Bubble.png", bg = "transparent", width = 25, height = 20, units = "cm")
