#wczytywanie przygotowanych danych danych

plywanie <- read.csv("runners.txt", sep = ";")
mlot <- read.csv("hammer_throwers.txt", sep = ";")
biegi <- read.csv("swimmers.txt", sep = ";")

#wczytywanie potrzebnych bibliotek

library(fmsb)
library(dplyr)

# ustalanie kolorów wykresów

colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9))
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4))


#biegi
biegi %>% filter(record == "yes") -> biegi_record
biegi %>% filter(record == "no") -> biegi_reszta

rbind(colMeans(biegi_reszta[sapply(biegi_reszta, is.numeric)]),
colMeans(biegi_record[sapply(biegi_record, is.numeric)])) ->df1
rownames(df1) <- c("average runner", "record holder")

rbind(biegi %>% summarise_if(is.numeric, max),
      biegi %>% summarise_if(is.numeric, min),
 df1) -> df1


radarchart(df1, seg = 4, 
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1)
legend(x=0.7, y=1,legend = rownames(df1[-c(1,2),]),bty = "n", 
       pch=20 ,col=colors_in,text.col = "grey", cex=1.2, pt.cex=3)


#plywanie
plywanie %>% filter(record == "yes") -> plywanie_record
plywanie %>% filter(record == "no") -> plywanie_reszta

rbind(colMeans(plywanie_reszta[sapply(plywanie_reszta, is.numeric)]),
      colMeans(plywanie_record[sapply(plywanie_record, is.numeric)])) ->df1
rownames(df1) <- c("average swimmer", "record holder")

rbind(plywanie %>% summarise_if(is.numeric, max),
      plywanie %>% summarise_if(is.numeric, min),
      df1) -> df1

radarchart(df1, seg = 4, 
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1)
legend(x=0.7, y=1,legend = rownames(df1[-c(1,2),]),bty = "n", 
       pch=20 ,col=colors_in,text.col = "grey", cex=1.2, pt.cex=3)


#rzut mlotem
mlot %>% filter(record == "yes") -> mlot_record
mlot %>% filter(record == "no") -> mlot_reszta

rbind(colMeans(mlot_reszta[sapply(mlot_reszta, is.numeric)]),
      colMeans(mlot_record[sapply(mlot_record, is.numeric)])) ->df1
rownames(df1) <- c("average thrower", "record holder")

rbind(mlot %>% summarise_if(is.numeric, max),
      mlot %>% summarise_if(is.numeric, min),
      df1) -> df1

radarchart(df1, seg = 4, 
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,axislabcol = "grey",caxislabels = TRUE)
legend(x=0.7, y=1,legend = rownames(df1[-c(1,2),]),bty = "n", 
       pch=20 ,col=colors_in,text.col = "grey", cex=1.2, pt.cex=3)
