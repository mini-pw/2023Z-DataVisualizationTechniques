dane <- read.csv("dane.csv")
dane$Date = as.Date(dane$Date)
library("ggplot2")
ggplot(dane, aes(x=Date, y=Hours)) +
  scale_y_continuous(expand = c(0,1), limits = c(0, 520))+
  scale_x_date(date_minor_breaks = "1 year")+
  geom_line(color = "red") +
  geom_point()+
  labs(title = "Hours of video uploaded to Youtube every minute", y="Hours of video")+
  theme_linedraw()
ggsave("nowy-wykres.png", device = "png")
