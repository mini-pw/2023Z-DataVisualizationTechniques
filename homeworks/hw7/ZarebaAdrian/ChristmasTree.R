library(ggplot2)
library(dplyr)

#Funkcja generujaca wartosci potrzebne do narysowania
#naszej choinki
generate_Values <- function(n) {
  values <- c(1)
  for(i in 2:(n)) {
    values <- c(values, rep(i, times = (n-i)))
  }
  return(values)
}

#Generowanie wartosci
val = generate_Values(30)
ChristmasTree <- data.frame(values = val,
                            name = rep("MerryChristmas", 
                                       times=length(val)))

#Tworzenie naszego swiatecznego drzewka
ggplot(ChristmasTree, aes(x=name, y = values)) +
  geom_violin(fill = "green") +
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize=0.7, 
               stackratio=1.05, 
               fill = "blue") +
  theme(axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_point(aes(x="MerryChristmas", y=30), shape=8, size = 20) +
  geom_point(aes(x="MerryChristmas", y=30), shape=23, size = 9, bg = "yellow") +
  geom_point(aes(x="MerryChristmas", y=30), shape=8, size = 5) +
  geom_point(aes(x="MerryChristmas", y=0), shape=15, size = 10) +
  scale_y_continuous(limits = c(-1, 37))

