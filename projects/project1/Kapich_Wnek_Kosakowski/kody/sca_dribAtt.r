library(ggplot2)
library(ggrepel)
library(dplyr)
library(hrbrthemes)

sca <- read.csv("./sca.csv")
time_threshold <- round(max(sca$X90s)/3, 1)
sca <- sca %>% 
  filter(X90s>max(X90s)/3) %>%
  select(Player, Squad, SCA90)


dribAtt <- read.csv("dribAtt.csv")
dribAtt <- dribAtt %>% 
  filter(X90s>max(X90s)/3) %>% 
  mutate(dribAtt90 = round((Att/X90s), 2)) %>% 
  select(Player, dribAtt90)

dataset <- unique(merge(sca, dribAtt, by = "Player", all=TRUE))

threshold1 <- quantile(dataset$SCA90,
                       probs = seq(.1, .9, by = .1),
                       na.rm=TRUE)[["90%"]]
threshold2 <- quantile(dataset$dribAtt90,
                       probs = seq(.1, .9, by = .1),
                       na.rm=TRUE)[["90%"]]

top <- dataset %>% 
  filter(SCA90>=threshold1|dribAtt90>=threshold2)
top_sca <- top %>%
  filter(SCA90>=threshold1 & dribAtt90<threshold2) %>% 
  arrange(desc(SCA90))
top_dribAtt <- top %>% 
  filter(dribAtt90>=threshold2 & SCA90<threshold1) %>% 
  arrange(desc(dribAtt90))
top_both <- top %>%
  filter(SCA90>=threshold1&dribAtt90>=threshold2) %>% 
  na.omit()



Lorient <- dataset %>% 
  filter(Player %in% c("Terem Moffi", "Julien Ponceau"))

plot <- ggplot(dataset, aes(x=.data$SCA90, y=.data$dribAtt90)) +
  geom_point() +
  xlab("") +
  ylab("") +
  scale_x_continuous(breaks = seq(0, 8, 1), expand=c(0,0)) +
  scale_y_continuous(breaks = seq(0, 8, 1), expand=c(0,0)) +
  coord_cartesian(xlim=c(0,7.2), ylim=c(0,7.5)) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),  
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  geom_point(data=dataset, col="white") +
  geom_point(data=top_both, col="forestgreen") +
  geom_point(data=dataset[dataset$Squad=="Lorient",], col="coral", size=3.0) +
  geom_vline(xintercept = threshold1, linetype="dashed", color="gray12") +
  geom_text(aes(x=threshold1+0.23,y=0.23,label="Top 90%"), colour="gray30") +
  geom_hline(yintercept=threshold2, linetype="dashed", color="gray12") +
  geom_text(aes(x=0.23,y=threshold2+0.23,label="Top 90%"), colour="gray30") +
  geom_text_repel(data=top_both,
                  aes(.data$SCA90,.data$dribAtt90,label=.data[["Player"]]),
                  size=4, colour="gray20") +
  geom_text_repel(data=head(top_sca, 5),
                  aes(.data$SCA90,.data$dribAtt90,label=.data[["Player"]]),
                  size=4, colour="gray20") +
  geom_text_repel(data=head(top_dribAtt, 5),
                  aes(.data$SCA90,.data$dribAtt90,label=.data[["Player"]]),
                  size=4, colour="gray20") +
  geom_text_repel(data=Lorient,
                  aes(.data$SCA90,.data$dribAtt90,label=.data[["Player"]]),
                  size=4, colour="gray20") +
  labs(caption='') +
  theme(axis.text = element_text(size = 12, color = 'black'))
plot(plot)

ggsave(filename = "SCAvsDribAtt.png",
       plot=plot,
       bg='transparent',
       width=11.7,
       height=6)
