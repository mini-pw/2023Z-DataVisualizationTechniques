library(tidyr)
library(dplyr)
library(ggplot2)

letnie_medale <- read.csv("Summer_olympic_Medals.csv")
kraje <- read.csv("kraje.csv")

laczna_suma_medali_lato <- letnie_medale %>% 
  group_by(Year) %>% 
  summarise(laczna_suma = sum(Gold)+sum(Silver)+sum(Bronze))

host_medale_lato <- letnie_medale %>% 
  group_by(Country_Name,Year) %>% 
  summarise(suma_medali = sum(Gold[Country_Name == Host_country])+
              sum(Silver[Country_Name == Host_country])+
              sum(Bronze[Country_Name == Host_country])) %>% 
  ungroup() %>% 
  filter(suma_medali>0 & Country_Name != "") %>% 
  right_join(laczna_suma_medali_lato, by = "Year") %>% 
  mutate(procent_medali = suma_medali/laczna_suma) %>%
  group_by(Country_Name) %>% 
  summarise(sredni_proc_medali_host = mean(procent_medali))

lato <- letnie_medale %>% 
  group_by(Country_Name,Year) %>% 
  summarise(suma_medali = sum(Gold)+sum(Silver)+sum(Bronze)) %>% 
  ungroup() %>% 
  right_join(laczna_suma_medali_lato, by = "Year") %>% 
  mutate(procent_medali = suma_medali/laczna_suma) %>%
  group_by(Country_Name) %>% 
  summarise(sredni_proc_medali = mean(procent_medali)) %>% 
  right_join(host_medale_lato, by = "Country_Name") %>%
  drop_na() %>% 
  left_join(kraje,by = c("Country_Name"="ENG")) %>% 
  select(-Country_Name) %>% 
  rename(Country_Name = PL)

lato_wykres <- lato %>% 
  ggplot() +
  geom_segment( aes(x=reorder(Country_Name,sredni_proc_medali), 
                    xend=reorder(Country_Name,sredni_proc_medali), 
                    y=sredni_proc_medali, 
                    yend=sredni_proc_medali_host),
                color = "#ffffff") +
  geom_point( aes(x=reorder(Country_Name,sredni_proc_medali), y=sredni_proc_medali, color="sredni_proc_medali"), size=3 ) +
  geom_point( aes(x=reorder(Country_Name,sredni_proc_medali), y=sredni_proc_medali_host, color="sredni_proc_medali_host"), size=3 ) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,0.4,0.05)) +
  labs(title = "Średni odsetek zdobytych medali", 
       subtitle = "Lata 1896-2020, lato", 
       y = "Procent zdobytych medali", 
       x = "Kraj",
       color = '') +
  scale_color_manual(values = c("#0082c9","#ef304d"),
                     labels = c("Ogólnie", "Jako organizator"), 
                     guide = guide_legend(),name = "") +
  theme(plot.background = element_rect(fill = "#323232",color = "#323232"),
        panel.background = element_rect(fill = "#323232"),
        legend.background = element_rect(fill = "#323232"),
        legend.key = element_rect(fill = "#323232"),
        legend.text = element_text(size = 14, face = "bold"),
        text = element_text(colour = "#fcb22d"),
        axis.line = element_line(colour = "#fcb22d"),
        axis.text = element_text(colour = "#fcb22d", size = 12),
        axis.title = element_text(size = 14, colour = "#fcb22d", face = "bold"),
        axis.title.y = element_blank(),
        axis.title.y.left = element_text(angle = 0,vjust = 1.05,hjust = 0,margin=margin(r=-100)),
        panel.grid = element_line(colour = "#fcb22d", size = 0.125),
        panel.border = element_blank(),
        title = element_text(size = 18, face = "bold", colour = "#fcb22d"),
        plot.subtitle = element_text(margin = margin(0,0,10,0)),
        plot.margin = unit(c(1, 1, 1, 10), "cm"),
        legend.margin = margin(t = 0, unit='cm')
  ) +
  coord_flip() +
  theme(legend.position = c(-.43,.60))

lato_wykres

