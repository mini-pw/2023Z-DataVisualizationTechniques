library(tidyr)
library(dplyr)
library(ggplot2)

zimowe_medale <- read.csv("Winter_Olympic_Medals.csv")
kraje <- read.csv("kraje.csv")

laczna_suma_medali_zima <- zimowe_medale %>% 
  group_by(Year) %>% 
  summarise(laczna_suma = sum(Gold)+sum(Silver)+sum(Bronze))

host_medale_zima <- zimowe_medale %>% 
  group_by(Country_Name,Year) %>% 
  summarise(suma_medali = sum(Gold[Country_Name == Host_country])+
              sum(Silver[Country_Name == Host_country])+
              sum(Bronze[Country_Name == Host_country])) %>% 
  ungroup() %>% 
  filter(suma_medali>0) %>% 
  right_join(laczna_suma_medali_zima, by = "Year") %>% 
  mutate(procent_medali = suma_medali/laczna_suma) %>%
  group_by(Country_Name) %>% 
  summarise(sredni_proc_medali_host = mean(procent_medali))

zima <- zimowe_medale %>% 
  group_by(Country_Name,Year) %>% 
  summarise(suma_medali = sum(Gold)+sum(Silver)+sum(Bronze)) %>% 
  ungroup() %>% 
  right_join(laczna_suma_medali_zima, by = "Year") %>% 
  mutate(procent_medali = suma_medali/laczna_suma) %>%
  group_by(Country_Name) %>% 
  summarise(sredni_proc_medali = mean(procent_medali)) %>% 
  right_join(host_medale_zima, by = "Country_Name") %>% 
  left_join(kraje,by = c("Country_Name"="ENG")) %>% 
  select(-Country_Name) %>% 
  rename(Country_Name = PL)

zima_wykres <- zima %>% 
  ggplot() +
  geom_segment( aes(x=reorder(Country_Name,sredni_proc_medali),
                    xend=reorder(Country_Name,sredni_proc_medali),
                    y=sredni_proc_medali, yend=sredni_proc_medali_host),
                color = "#ffffff") +
  geom_point( aes(x=reorder(Country_Name,sredni_proc_medali),
                  y=sredni_proc_medali, color="sredni_proc_medali"), size=3 ) +
  geom_point( aes(x=reorder(Country_Name,sredni_proc_medali),
                  y=sredni_proc_medali_host, color="sredni_proc_medali_host"), size=3 ) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,0.4,0.02)) +
  labs(title = "Średni odstetek zdobytych medali", 
       subtitle = "Lata 1924 - 2018, zima", 
       y = "Procent zdobytych medali", 
       x = "Kraj") +
  scale_color_manual(values = c("#0082c9","#ef304d"),labels = c("Ogólnie", "W przypadku bycia hostem"), guide = guide_legend(),name = "") +
  theme(plot.background = element_rect(fill = "#323232",color = "#323232"),
        panel.background = element_rect(fill = "#323232"),
        legend.position = "none",
        text = element_text(colour = "#fcb22d"),
        axis.line = element_line(colour = "#fcb22d"),
        axis.text = element_text(colour = "#fcb22d", size = 12),
        axis.title = element_text(size = 14, colour = "#fcb22d", face = "bold"),
        axis.title.y.right = element_text(angle = 0,vjust = 1.05,hjust = 0,margin=margin(l=-100)),
        panel.grid = element_line(colour = "#fcb22d",size = 0.125),
        panel.border = element_blank(),
        title = element_text(size = 18, face = "bold", colour = "#fcb22d"),
        plot.subtitle = element_text(margin = margin(0,0,10,0)),
        plot.margin = unit(c(1, 3, 1, 1), "cm")
  ) +
  coord_flip() +
  scale_x_discrete(position = "top")
zima_wykres
