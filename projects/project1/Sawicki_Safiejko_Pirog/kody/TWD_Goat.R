#Wczytaywanie bibliotek

library(dplyr)
library(ggplot2)
library(stringi)
library(forcats)
library(RColorBrewer)
library(showtext)
source("themes.r")

#Wczytywanie danych

KlayThompson <- read.csv("KlayThompson.csv")
StephenCurry <- read.csv("StephenCurry.csv")
JamesHarden <- read.csv("JamesHarden.csv")
DamianLillard <- read.csv("DamianLillard.csv")
KyleLowry <- read.csv("KyleLowry.csv")
PaulGeorge <- read.csv("PaulGeorge.csv")
JRSmith <- read.csv("JRSmith.csv")
JJRedick <- read.csv("JJRedick.csv")
CJMcCollum <- read.csv("CJMcCollum.csv")
WesleyMatthews <- read.csv("WesleyMatthews.csv")

#Obrabianie ramek danych


trojki_w_meczach <- function(df){
  
  df %>% select(G,X3P) -> df
  apply(df, 2, function(x) sum(is.na(x)))
  df %>% mutate(trojki = as.numeric(X3P)) %>% select(trojki)->df
  df[is.na(df)] = 0
  df %>% mutate(trojki = cumsum(trojki),GP = c(1:82)) ->df
  df
}

KlayThompson %>% trojki_w_meczach() %>% mutate(Name = "Klay Thompson") -> KlayThompson
StephenCurry %>% trojki_w_meczach() %>% mutate(Name = "Stephen Curry") -> StephenCurry
JamesHarden %>% trojki_w_meczach() %>% mutate(Name = "James Harden") -> JamesHarden
DamianLillard %>% trojki_w_meczach() %>% mutate(Name = "Damian Lillard") -> DamianLillard
KyleLowry %>% trojki_w_meczach() %>% mutate(Name = "Kyle Lowry") -> KyleLowry
PaulGeorge %>% trojki_w_meczach() %>% mutate(Name = "Paul George") -> PaulGeorge
JRSmith %>% trojki_w_meczach() %>% mutate(Name = "J.R Smith") -> JRSmith
JJRedick %>% trojki_w_meczach() %>% mutate(Name = "JJ Redick") -> JJRedick
CJMcCollum %>% trojki_w_meczach() %>% mutate(Name = "CJ McCollum") -> CJMcCollum
WesleyMatthews %>% trojki_w_meczach() %>% mutate(Name = "Wesley Matthews") -> WesleyMatthews


imiona <- c("Stephen Curry","Klay Thompson", "James Harden", "Damian Lillard", "Kyle Lowry", "Paul George", "J.R Smith",
            "JJ Redick","CJ McCollum","Wesley Matthews")


mypalette<-brewer.pal(10,"Spectral")
mypalette[1] <- "#f8b31a"
mypalette[2] <- "#A7A7AD"
mypalette[3] <- "#824A02"
mypalette[4] <- brewer.pal(10,"Spectral")[1]
mypalette[5] <- brewer.pal(10,"Spectral")[3]
mypalette[6] <- brewer.pal(10,"Spectral")[5]
mypalette[7] <- brewer.pal(10,"RdYlGn")[7]
mypalette[8:10] <- brewer.pal(10,"Spectral")[8:10]



Shooters <- rbind(KlayThompson,StephenCurry,JamesHarden,
                  DamianLillard, KyleLowry, PaulGeorge,
                  JRSmith,JJRedick,CJMcCollum,WesleyMatthews) %>% mutate(Name = factor(Name)) %>% 
                   mutate(Name = fct_relevel(Name, imiona))

#Rysowanie wykresu

ggplot(data = Shooters, mapping = aes(x = GP, y = trojki, colour = Name)) +
  geom_point() +  geom_line() + 
  labs(title = "3 pointers made throughout NBA 2015/16 season",
                                    x = "Games played", 
                                    y = "3 pointers made", fill = "tytu³ legendy") + 
  scale_x_continuous(expand = c(0,1))+ 
  scale_y_continuous(expand = c(0,1), limits = c(0, 450))+ 
  scale_color_manual(values = mypalette)+
  theme_dark_blue() + 
  theme(legend.title = element_text(face = "bold"))+
  theme(plot.title = element_text(size=22))+
  theme(axis.title=element_text(size=18), axis.text=element_text(size=14, colour = "white"))+
  theme(legend.text = element_text(size =12))+
  theme(plot.title = element_text(colour ="#f8b31a"))
  



?geom_point

