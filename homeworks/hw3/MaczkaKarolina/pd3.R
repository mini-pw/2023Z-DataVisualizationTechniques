library(dplyr)
library("readxl")
library(ggplot2)

df <- read_excel("Wykresy test (Responses).xlsx")
odp<-c("Warmińsko-mazurskim",70481, "Dolnośląskie",92,"supermarketów")

#trafność dla róznych wykresów kolumnowych
#3d
colnames(df)<-c("data","score","pyt1","pyt2","pyt3","pyt4","pyt5","pyt11","pyt21","pyt31","pyt41","pyt51","pyt6","pyt7")
head(df)
  
df %>% 
  ggplot(aes(fill=factor(ifelse(pyt1=="Warmińsko-mazurskim","Poprawna odpowiedź","Zła odpowiedź"))))+
  geom_histogram(aes(x=pyt1,), stat = "count")+
  scale_fill_manual(name="Odpowiedzi", values = c("red","grey60"))+
  labs(y="Ilość odpowiedzi", x = "Województwa")+
  ggtitle("W którym województwie (spośród podanych) liczba ludności przypadająca na jeden sklep jest najmniejsza?")
  
df %>% 
  ggplot(aes(fill=factor(ifelse(pyt11=="Warmińsko-mazurskim","Poprawna odpowiedź","Zła odpowiedź"))))+
  geom_histogram(aes(x=pyt11,), stat = "count")+
  scale_fill_manual(name="Odpowiedzi", values = c("red","grey60"))+
  labs(y="Ilość odpowiedzi", x = "Województwa")+
  ggtitle("W którym województwie (spośród podanych) liczba ludności przypadająca na jeden sklep jest najmniejsza?")

# odczyt danych numerycznych 3d
df %>% 
  mutate(bl_wzgledny2=abs(pyt2-70481)/70481) %>% 
  mutate(bl_wzgledny21=abs(pyt21-70481)/70481) %>% 
  filter(bl_wzgledny2<2) %>% 
  filter(bl_wzgledny21<2) %>% 
  ggplot()+
  geom_point(aes(x=bl_wzgledny2, y=bl_wzgledny21))+
  geom_abline(linetype="dashed", color = "black",size=0.5)+
  xlim(0, 2)+
  ylim(0, 2)+
  labs(x="Wykres 3D", y="Wykres 2D")+
  ggtitle("Błąd względny w odczycie danych numerycznych")

df %>% 
  mutate(bl_wzgledny4=abs(pyt4-92)/92) %>% 
  mutate(bl_wzgledny41=abs(pyt41-92)/92) %>% 
  filter(bl_wzgledny4<0.2) %>% 
  filter(bl_wzgledny41<0.2) %>% 
  ggplot()+
  geom_point(aes(x=bl_wzgledny4, y=bl_wzgledny41))+
  geom_abline(linetype="dashed", color = "black",size=0.5)+
  xlim(0, 0.2)+
  ylim(0, 0.2)+
  labs(x="Wykres 3D", y="Wykres 2D")+
  ggtitle("Błąd względny w odczycie danych numerycznych")

df_sd<-df %>% 
  mutate(bl_wzgledny2=abs(pyt2-70481)/70481) %>% 
  mutate(bl_wzgledny21=abs(pyt21-70481)/70481) %>% 
  filter(bl_wzgledny2<2) %>% 
  filter(bl_wzgledny21<2) %>% 
  mutate(bl_wzgledny4=abs(pyt4-92)/92) %>% 
  mutate(bl_wzgledny41=abs(pyt41-92)/92) %>% 
  filter(bl_wzgledny4<0.2) %>% 
  filter(bl_wzgledny41<0.2) %>%
  summarise(sd_2=sd(pyt2,na.rm=TRUE)/70481,sd_21=sd(pyt21,na.rm=TRUE)/70481,sd_4=sd(pyt4,na.rm=TRUE)/92,sd_41=sd(pyt41,na.rm=TRUE)/92)
  
tab <- data.frame(wykres = c("wykres 3D","wykres 2D","wykres 3D","wykres 2D"), wart = as.numeric(as.vector(df_sd[1,])),
                  pyt=rep(c("pytanie 1", "pytanie 2"), each=2))

tab %>% 
  ggplot(aes(factor(pyt), wart, fill = wykres)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set2")+
  ggtitle("Odchylenie standardowe względem poprawnej wartości")+
  labs(x="", y="wartość")

# podsumowanie poprawnych odpowiedzi

sum(df$pyt1 == "Warmińsko-mazurskim")
wyk_podsumowanie<-df %>% 
  summarise(pytanie_1_3D=sum(df$pyt1 == "Warmińsko-mazurskim"),
            pytanie_1_2D=sum(df$pyt11 == "Warmińsko-mazurskim"),
            pytanie_2_3D=sum(df$pyt3 == "Dolnośląskie"),
            pytanie_2_2D=sum(df$pyt31 == "Dolnośląskie"),
            pytanie_3_3D=sum(df$pyt5 == "supermarketów"),
            pytanie_3_2D=sum(df$pyt51 == "supermarketów")) 

wyk_podsumowanie<-data.frame(pytanie=colnames(wyk_podsumowanie), liczba=as.numeric(as.vector(wyk_podsumowanie[1,])))
wyk_podsumowanie %>% 
  ggplot(aes(fill=factor(ifelse(pytanie %in% wyk_podsumowanie$pytanie[c(1,3,5)],"Wykres 3D","Wykres 2D"))))+
  geom_col(aes(x=pytanie, y=liczba))+
  scale_fill_manual(name="Rodzaj wykresu", values = c("brown1","dark red"))+
  labs(x="", y="liczba poprawnych odpowiedzi")+
  ggtitle("Podsumowanie poprawnych odpowiedzi")

#które wykresy wolimy

ktore_wol<-df %>%
  summarise(pytanie_6_2D=sum(df$pyt6 == "Po lewej"),
            pytanie_6_3D=sum(df$pyt6 == "Po prawej"),
            pytanie_7_2D=sum(df$pyt7 == "Po prawej"),
            pytanie_7_3D=sum(df$pyt7 == "Po lewej")
            ) 
ktore_wol<-data.frame(pytanie=colnames(ktore_wol), liczba=as.numeric(as.vector(ktore_wol[1,])))
ktore_wol %>% 
  ggplot(aes(fill=factor(ifelse(pytanie %in% ktore_wol$pytanie[c(1,3)],"Wykres 2D","Wykres 3D"))))+
  geom_col(aes(x=pytanie, y=liczba))+
  scale_fill_manual(name="Rodzaj wykresu", values = c("brown1","dark red"))+
  labs(x="", y="Liczba osób preferujący dany typ wykresu")+
  ggtitle("Podsumowanie preferencji badanych osób")
  
