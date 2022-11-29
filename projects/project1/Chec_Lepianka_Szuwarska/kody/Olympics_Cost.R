library(dplyr)
library(ggplot2)

data <- as.data.frame(read.csv("immakym.csv"))
colnames(data) <- c("Olympics", "Number_of_Athletes", "Accredited_Media","Ticketing_Revenue"
                    ,"Broadcast_Revenue", "International_Sponsorship_Revenue", "Domestic_Sponsoring",
                    "Cost_of_venues", "Cost_of_organisation", "Season"   )

df_winter <- data %>% 
  filter(Season == " winter") %>% 
  mutate(koszt = Cost_of_venues + Cost_of_organisation) %>% 
  mutate(przychod = Ticketing_Revenue + Broadcast_Revenue + International_Sponsorship_Revenue) %>% 
  mutate(Rok = substring(Olympics,0, 4)) %>% 
  mutate(Dochod = przychod-koszt)

limit_winter = c(-750000000,75000000)
limit_winter_2 = c(-12000000000,-10000000000)
options(scipen=999)

ggplot(df_winter,aes(x = Rok, y = Dochod)) + 
  geom_point(size=5,colour ="#fcb22d") + 
 scale_y_continuous(breaks = scales::pretty_breaks(n = 10),
                    limits = limit_winter,
                    labels = scales::unit_format(unit = "", scale = 1e-6),
                    minor_breaks = 100)+
 labs(
  title = "Przychód organizatora igrzysk zimowych",
  subtitle = "Lata 1964-2018",
  y = "Przychody (mln USD)",
  x = "Rok igrzysk") +  theme_bw() + 
  theme(panel.background = element_rect(fill = "#323232"),
        plot.background = element_rect(fill = "#323232"),
        panel.border = element_rect(colour = "#fcb22d"),
        panel.grid.major = element_line( colour ="#fcb22d"),
        panel.grid.minor = element_line( colour ="#fcb22d")
        )+
  theme(axis.text=element_text(size=16, colour ="#fcb22d"),
        axis.title=element_text(size=20,face="bold", colour ="#fcb22d"),
        title=element_text(size=28, face="bold", colour ="#fcb22d"))

#wykres czêœciowy 
df_winter %>% 
  filter(Rok == 2014 ) %>% 
  ggplot(aes(x = Rok, y = Dochod)) + 
  geom_point(size=8,colour ="#fcb22d") + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),
                     limits = limit_winter_2,
                     labels = scales::unit_format(unit = "", scale = 1e-6),
                     minor_breaks = 100)+
  labs(
    title = "Przychód organizatora igrzysk zimowych",
    subtitle = "Lata 1964-2018",
    y = "Przychody (mln USD)",
    x = "Rok igrzysk") +  theme_bw() + 
  theme(panel.background = element_rect(fill = "#323232"),
        plot.background = element_rect(fill = "#323232"),
        panel.border = element_rect(colour = "#fcb22d"),
        panel.grid.major = element_line( colour ="#fcb22d"),
        panel.grid.minor = element_line( colour ="#fcb22d")
  )+
  theme(axis.text=element_text(size=36, colour ="#fcb22d"),
        axis.title=element_text(size=20,face="bold", colour ="#fcb22d"),
        title=element_text(size=28, face="bold", colour ="#fcb22d"))





df_summer <- data %>% 
  filter(Season == "  summer") %>% 
  mutate(koszt = Cost_of_venues + Cost_of_organisation) %>% 
  mutate(przychod = Ticketing_Revenue + Broadcast_Revenue + International_Sponsorship_Revenue) %>% 
  mutate(Rok = substring(Olympics,0, 4)) %>% 
  mutate(Dochod = przychod-koszt) 
  
limit_summer = c(-20000000000,2000000000)
limit_summer_2 = c(-1400000000000,-1000000000)

ggplot(df_summer,aes(x = Rok, y = Dochod)) + 
  geom_point(size=5, colour ="#fcb22d") +  
  geom_segment(aes(x = Rok, y = 0, xend = Rok, yend = Dochod))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),
                     labels=scales::unit_format(unit = "", scale = 1e-9),
                     limits = limit_summer,
                     minor_breaks = 200)+
  labs(
    title = "Przychód organizatora igrzysk letnich",
    subtitle = "Lata 1964-2016",
    y = "Przychody (mld USD)",
    x = "Rok igrzysk") + theme_bw() +
  theme(panel.background = element_rect(fill = "#323232"),
        plot.background = element_rect(fill = "#323232"),
        panel.border = element_rect(colour = "#fcb22d"),
        panel.grid.major = element_line( colour ="#fcb22d"),
        panel.grid.minor = element_line( colour ="#fcb22d")
  )+
  theme(axis.text=element_text(size=16, colour ="#fcb22d"),
        axis.title=element_text(size=20,face="bold", colour ="#fcb22d"),
        title=element_text(size=28, face="bold", colour ="#fcb22d"))

df_summer %>% 
  filter(Rok==1988 | Rok == 1992) %>% 
  ggplot(aes(x = Rok, y = Dochod)) + 

  geom_point(size=5, colour ="#fcb22d") + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),
                     labels=scales::unit_format(unit = "", scale = 1e-9),
                     limits = limit_summer_2,
                     minor_breaks = 200)+
  labs(
    title = "Przychód organizatora igrzysk letnich",
    subtitle = "Lata 1964-2016",
    y = "Przychody (mld USD)",
    x = "Rok igrzysk") + theme_bw() +
  theme(panel.background = element_rect(fill = "#323232"),
        plot.background = element_rect(fill = "#323232"),
        panel.border = element_rect(colour = "#fcb22d"),
        panel.grid.major = element_line(size = 1, colour ="#fcb22d"),
        panel.grid.minor = element_line( colour ="#fcb22d",
       )
  )+
  theme(axis.text=element_text(size=36, colour ="#fcb22d"),
        axis.title=element_text(size=20,face="bold", colour ="#fcb22d"),
        title=element_text(size=28, face="bold", colour ="#fcb22d"))

