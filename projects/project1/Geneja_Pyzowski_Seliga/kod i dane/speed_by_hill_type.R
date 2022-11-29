path<-"C:/Users/kubas/RProjects/TWD_lab"

all_names<-read.csv(paste(path,"/Ski_jumping_data_center-main/all_names.csv", sep=""))
all_comps<-read.csv(paste(path,"/Ski_jumping_data_center-main/all_comps.csv", sep=""))
all_ratings<-read.csv(paste(path,"/Ski_jumping_data_center-main/all_ratings.csv", sep=""))
all_results<-read.csv(paste(path,"/Ski_jumping_data_center-main/all_results.csv", sep=""))
all_stats<-read.csv(paste(path,"/Ski_jumping_data_center-main/all_stats.csv", sep=""))
improved_names<-read.csv(paste(path,"/Ski_jumping_data_center-main/improved_names.csv", sep=""))

library(dplyr)
library(ggplot2)

colors3<-c("#a3e0ea","#4baac6","#0a71a7")

all_comps <- mutate(all_comps, id = trimws(id)) #There are spaces at the end of each tournament ids, but there were no spaces in other dataframes

real_comps <- filter(all_comps, training == 0)

speed <- merge(all_results, improved_names, by = 'codex') %>%
  merge(real_comps, by = 'id') %>%
  filter(gender.x == 'M', speed > 0, training==0) %>%
  mutate(hill_type=case_when(
    k.point<100 ~ 'normal',
    k.point<170 ~ 'large',
    TRUE ~ 'ski flying hill')) %>%
  mutate(hill_type1=factor(hill_type, levels=c('normal', 'large', 'ski flying hill')))


speed %>% 
  select(speed, hill_type1) %>%
  ggplot(mapping = aes(x = speed, fill=hill_type1)) +
  geom_density(alpha=0.4)+
  scale_fill_discrete(name = "Type of hill")+
  scale_x_continuous(name='Speed distribution', limits = c(75,110))+
  scale_y_continuous(name='Density')+
  scale_fill_manual(values=colors3)+
  theme_ipsum() +
  theme(panel.spacing = unit(0.1, "lines",),
        strip.text.x = element_text(size = 8),
        legend.title = element_text(),
        plot.title = element_text(size = 20, face = "bold"),
        legend.position = c(0.13, 0.75),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(title = "Distribution of take-off speed by type of hill")+
  guides(fill=guide_legend(title="Type of hill"))+
  scale_y_continuous(expand=c(0,0))
