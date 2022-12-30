library(dplyr)
library(ggplot2)
library(ggstar)
library(ggbeeswarm)
library(randomcoloR)
library(cowsay)
library(stringr)
library(ggpubr)
library(jpeg)

img <- readJPEG("bg.jpg")

Sepal.Length<-c(7.1)
Species<-c("versicolor")
star_df<-data.frame(Sepal.Length, Species)

randomColor(10, luminosity="light")

colrz <-  randomColor(47, luminosity = "bright")
whiters <- rep("#ffffff", 52)
colrz<-append(colrz, whiters)

text<-str_remove(say("wesołych świąt 2023!",
          by = "cat", type = "string"), "jgs")

iris %>% 
  mutate(Sepal.Length = ifelse(Species != "versicolor", 5, Sepal.Length )) %>% 
  filter(Species!="setosa", Sepal.Length>=5) %>% 
  ggplot(aes(x=Species, y=Sepal.Length))+
  background_image(img)+
  geom_violin(trim = T, adjust = 1, fill = "darkgreen")+
  scale_x_discrete(expand=c(0.6, 0.6))+
  scale_y_continuous(expand=c(0.13,0))+
  theme(line = element_blank(),
  text = element_blank(),
  title = element_blank())+
  geom_quasirandom(color = colrz, size = 6)+
  geom_star(data = star_df, fill = "gold", size = 32)+
  annotate("text", x='virginica', y=6, label= text , fontface = 10, size=6)
