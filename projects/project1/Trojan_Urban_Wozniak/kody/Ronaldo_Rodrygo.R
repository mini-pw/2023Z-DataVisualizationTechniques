library(dplyr)
library(fmsb)
library(ggplot2)
#zaczytanie ramek
real22 <- read.csv2("reality21-22.csv")
fifa23 <- read.csv("players_fifa23.csv")
##font_add("knul", "C:/Users/sebas/OneDrive/Dokumenty/R/Projek_1_TWD/Knul-Regular.otf")
##showtext_auto()
my_white <- rgb(255, 255, 255, 150, maxColorValue = 255)
#ograniczenie fify tylko do TOP5
fifa23TOP5 <- fifa23 %>%
  filter(Club == "Real Madrid CF"|Club == "FC Barcelona"|Club == "Sevilla FC"|Club == "Atlético de Madrid"|Club == "Villarreal CF"|Club == "RC Celta de Vigo"|Club == "Real Betis Balompié"|Club == "Athletic Club de Bilbao"|Club == "Real Sociedad"|Club == "Getafe CF"|
           Club =="RCD Espanyol de Barcelona"|Club == "Valencia CF"|Club =="Cádiz CF"|Club == "Elche CF"|Club == "CA Osasuna"|Club == "Rayo Vallecano"|Club == "Deportivo Alavés"|Club == "RCD Mallorca"|Club == "Levante Unión Deportiva"|Club == "Granada CF"|
           Club =="Manchester City"|Club == "Liverpool"|Club =="Manchester United"|Club == "Chelsea"|Club == "Tottenham Hotspur"|Club == "Leicester City"|Club == "Arsenal"|Club == "Aston Villa"|Club == "Newcastle United"|Club == "West Ham United"|
           Club =="Wolverhampton Wanderers"|Club == "Crystal Palace"|Club =="Everton"|Club == "Southampton"|Club == "Leeds United"|Club == "Brighton & Hove Albion"|Club == "Brentford"|Club == "Watford"|Club == "Burnley"|Club == "Norwich City"|
           Club =="Paris Saint-Germain"|Club == "AS Monaco"|Club =="OGC Nice"|Club == "Olympique de Marseille"|Club == "Olympique Lyonnais"|Club == "Montpellier Hérault SC"|Club == "Racing Club de Lens"|Club == "Stade Rennais FC"|Club == "FC Nantes"|Club == "LOSC Lille"|
           Club =="FC Girondins de Bordeaux"|Club == "AS Saint-Étienne"|Club =="FC Lorient"|Club == "FC Metz"|Club == "ESTAC Troyes"|Club == "Stade de Reims"|Club == "Stade Brestois 29"|Club == "Clermont Foot 63"|Club == "Angers SCO"|Club == "RC Strasbourg Alsace"|
           Club =="FC Bayern München"|Club == "Eintracht Frankfurt"|Club =="RB Leipzig"|Club == "Bayer 04 Leverkusen"|Club == "Borussia Dortmund"|Club == "Borussia Mönchengladbach"|Club == "VfL Wolfsburg"|Club == "TSG Hoffenheim"|Club == "Sport-Club Freiburg"|Club == "1. FC Köln"|
           Club =="SpVgg Greuther Fürth"|Club == "DSC Arminia Bielefeld"|Club =="Hertha BSC"|Club == "FC Augsburg"|Club == "1. FSV Mainz 05"|Club == "1. FC Union Berlin"|Club == "VfB Stuttgart"|Club == "VfL Bochum 1848"|
           Club =="AC Milan"|Club == "Inter"|Club =="Juventus"|Club == "Lazio"|Club == "Roma"|Club == "U.S. Sassuolo Calcio"|Club == "Atalanta"|Club == "Napoli"|Club == "Bologna"|Club == "Fiorentina"|
           Club =="Udinese Calcio"|Club == "US Salernitana 1919"|Club =="Genoa"|Club == "Venezia FC"|Club == "Cagliari"|Club == "Spezia"|Club == "U.C. Sampdoria"|Club == "Torino F.C."|Club == "Hellas Verona"|Club == "Empoli")

  
#ramka pomocniczna do obliczania centyli Ronaldo w fifie23

wykres <- function(zawodnik_real,zawodnik_fifa, klub, real22, fifa23TOP5){
CR7real <- real22 %>%
  filter(Player == zawodnik_real, Squad == klub)
CR7fifa <- fifa23 %>%
  filter(Name == zawodnik_fifa)  

#obliczenie centyli wszyskich zawodnik?w z fify23
centyle_zawodnikow_ofensywnych_fifa <- fifa23TOP5 %>%
  filter(ClubPosition == "ST" |ClubPosition == "FW"|ClubPosition=="LW"|ClubPosition=="CF" |ClubPosition=="RW") %>%
  summarise(ShootingTotal = quantile((ShootingTotal),probs = seq(0.01, 1, 1/100)),
            PassingTotal = quantile((PassingTotal),probs = seq(0.01, 1, 1/100)),
            PhysicalityTotal = quantile((PhysicalityTotal),probs = seq(0.01, 1, 1/100)),
            DefendingTotal = quantile((DefendingTotal),probs = seq(0.01, 1, 1/100)),
            DribblingTotal = quantile((DribblingTotal),probs = seq(0.01, 1, 1/100)))


#obliczanie centyli zawodnika w fifie23
centyle_Ronaldo_fifa23 <- centyle_zawodnikow_ofensywnych_fifa %>%
  mutate(ShootingTotal = case_when(as.numeric(CR7fifa$ShootingTotal) < ShootingTotal  ~ "mniej",
                                 TRUE ~ "wi?cej"),
         PassingTotal = case_when(as.numeric(CR7fifa$PassingTotal) < PassingTotal  ~ "mniej",
                                   TRUE ~ "wi?cej"),
         PhysicalityTotal = case_when(as.numeric(CR7fifa$PhysicalityTotal) < PhysicalityTotal  ~ "mniej",
                                   TRUE ~ "wi?cej"),
         DefendingTotal = case_when(as.numeric(CR7fifa$DefendingTotal) < DefendingTotal  ~ "mniej",
                                   TRUE ~ "wi?cej"),
         DribblingTotal = case_when(as.numeric(CR7fifa$DribblingTotal) < DribblingTotal  ~ "mniej",
                                   TRUE ~ "wi?cej"))

#obliczanie centyli strza??w zawodnik?w w rzeczywisto?ci
centyle_strzalow_real <- real22 %>%
  filter((Pos == "FW"|Pos == "FWDF"| Pos == "FWMF")) %>%
  summarise(kwantyl_gol = quantile(as.numeric(Goals),probs = seq(0.01, 1, 1/100)), 
            kwantyl_gol_strzal = quantile(as.numeric(G.Sh),probs = seq(0.01, 1, 1/100)),
            kwantyl_strzal_celny = quantile(as.numeric(SoT.),probs = seq(0.01, 1, 1/100)))

centyle_karnych_real <- real22 %>%
  filter(ShoPK!=0) %>%
  mutate(skutecznosc_karny = as.numeric(ShoPK)/as.numeric(PKatt)) %>%
  summarise(skutecznosc_karny = quantile(skutecznosc_karny,probs = seq(0.01, 1, 1/100)))

#obliczanie centyli strza??w Ronaldo w rzeczywisto?ci
centyle_strzalow_Ronaldo_real <-  centyle_strzalow_real %>%
  mutate(kwantyl_gol = case_when(as.numeric(CR7real$Goals) < kwantyl_gol  ~ "mniej",
                                TRUE ~ "wi?cej"),
        kwantyl_gol_strzal = case_when(as.numeric(CR7real$G.Sh) < kwantyl_gol_strzal  ~ "mniej",
                                      TRUE ~ "wi?cej"), 
        kwantyl_strzal_celny = case_when(as.numeric(CR7real$SoT.) < kwantyl_strzal_celny  ~ "mniej",
                                        TRUE ~ "wi?cej"))

CR7real <- CR7real %>%
  mutate(skutecznosc_karny = as.numeric(ShoPK)/as.numeric(PKatt))

centyle_karnych_Ronaldo_real <- centyle_karnych_real %>%
  mutate(skutecznosc_karny = case_when( CR7real$skutecznosc_karny < skutecznosc_karny  ~ "mniej",
                                 TRUE ~ "wi?cej"))


centyle_strzlow_Ronaldo_suma_real <- mean(c(max(which(centyle_strzalow_Ronaldo_real$kwantyl_gol %in% c("wi?cej"))),
     max(which(centyle_strzalow_Ronaldo_real$kwantyl_gol_strzal %in% c("wi?cej"))),
     max(which(centyle_strzalow_Ronaldo_real$kwantyl_strzal_celny %in% c("wi?cej"))),
     max(which(centyle_karnych_Ronaldo_real$skutecznosc_karny %in% c("wi?cej")))))

#obliczanie centyli strza??w zawodnik?w w rzeczywisto?ci
centyle_podan_real <- real22 %>% 
  filter((Pos == "FW"|Pos == "FWDF"| Pos == "FWMF")) %>%
  summarise(kwantyl_cel_podan = quantile(as.numeric(PasTotCmp.),probs = seq(0.01, 1, 1/100)), 
            kwantyl_podanie_w_3_tercje = quantile(as.numeric(Pas3rd),probs = seq(0.01, 1, 1/100)),
            kwantyl_podanie_total = quantile(as.numeric(PasTotCmp),probs = seq(0.01, 1, 1/100)))



#obliczanie centyli poda? Ronaldo w rzeczywisto?ci
centyle_podan_Ronaldo_real <-  centyle_podan_real %>%
  mutate(kwantyl_cel_podan = case_when(as.numeric(CR7real$PasTotCmp.) < kwantyl_cel_podan  ~ "mniej",
                                 TRUE ~ "wi?cej"),
         kwantyl_podanie_w_3_tercje = case_when(as.numeric(CR7real$Pas3rd) < kwantyl_podanie_w_3_tercje  ~ "mniej",
                                        TRUE ~ "wi?cej"), 
         kwantyl_podanie_total = case_when(as.numeric(CR7real$PasTotCmp) < kwantyl_podanie_total  ~ "mniej",
                                          TRUE ~ "wi?cej"))
centyle_podan_Ronaldo_suma_real <- mean(c(max(which(centyle_podan_Ronaldo_real$kwantyl_cel_podan %in% c("wi?cej"))),
                                            max(which(centyle_podan_Ronaldo_real$kwantyl_podanie_w_3_tercje %in% c("wi?cej"))),
                                            max(which(centyle_podan_Ronaldo_real$kwantyl_podanie_total %in% c("wi?cej")))))

#obliczanie centyli dryblingu zawodnik?w w rzeczywisto?ci
centyle_dryblingu_real <- real22 %>% 
  filter((Pos == "FW"|Pos == "FWDF"| Pos == "FWMF")) %>%
  summarise(kwantyl_udany_drybling = quantile(as.numeric(DriSucc.),probs = seq(0.01, 1, 1/100)), 
            kwantyl_drybligi_strzal = quantile(as.numeric(ScaDrib),probs = seq(0.01, 1, 1/100)),
            kwantyl_dryblig_total = quantile(as.numeric(DriSucc),probs = seq(0.01, 1, 1/100)))



#obliczanie centyli dryblingu Ronaldo w rzeczywisto?ci
centyle_dryblingu_Ronaldo_real <-  centyle_dryblingu_real %>%
  mutate(kwantyl_udany_drybling = case_when(as.numeric(CR7real$DriSucc.) < kwantyl_udany_drybling  ~ "mniej",
                                       TRUE ~ "wi?cej"),
         kwantyl_drybligi_strzal = case_when(as.numeric(CR7real$ScaDrib) < kwantyl_drybligi_strzal  ~ "mniej",
                                                TRUE ~ "wi?cej"), 
         kwantyl_dryblig_total = case_when(as.numeric(CR7real$DriSucc) < kwantyl_dryblig_total  ~ "mniej",
                                           TRUE ~ "wi?cej"))
centyle_dryblingu_Ronaldo_suma_real <- mean(c(max(which(centyle_dryblingu_Ronaldo_real$kwantyl_udany_drybling %in% c("wi?cej"))),
                                          max(which(centyle_dryblingu_Ronaldo_real$kwantyl_drybligi_strzal %in% c("wi?cej"))),
                                          max(which(centyle_dryblingu_Ronaldo_real$kwantyl_dryblig_total %in% c("wi?cej")))))


#obliczanie centyli fizyczno?ci zawodnik?w w rzeczywisto?ci
centyle_fizycznosci_real <- real22 %>% 
  filter((Pos == "FW"|Pos == "FWDF"| Pos == "FWMF")) %>%
  summarise(kwantyl_srednio_minut = quantile(as.numeric(Min),probs = seq(0.01, 1, 1/100)), 
            kwantyl_pressing = quantile(as.numeric(Press),probs = seq(0.01, 1, 1/100)),
            kwantyl_wygrane_powietrze = quantile(as.numeric(AerWon.),probs = seq(0.01, 1, 1/100)))



#obliczanie centyli fizycznosci Ronaldo w rzeczywisto?ci
centyle_fizycznosci_Ronaldo_real <-  centyle_fizycznosci_real %>%
  mutate(kwantyl_srednio_minut = case_when(as.numeric(CR7real$Min) < kwantyl_srednio_minut  ~ "mniej",
                                            TRUE ~ "wi?cej"),
         kwantyl_pressing = case_when(as.numeric(CR7real$Press) < kwantyl_pressing  ~ "mniej",
                                             TRUE ~ "wi?cej"), 
         kwantyl_wygrane_powietrze = case_when(as.numeric(CR7real$AerWon.) < kwantyl_wygrane_powietrze  ~ "mniej",
                                           TRUE ~ "wi?cej"))
centyle_fizycznosci_Ronaldo_suma_real <- mean(c(max(which(centyle_fizycznosci_Ronaldo_real$kwantyl_srednio_minut %in% c("wi?cej"))),
                                              max(which(centyle_fizycznosci_Ronaldo_real$kwantyl_pressing %in% c("wi?cej"))),
                                              max(which(centyle_fizycznosci_Ronaldo_real$kwantyl_wygrane_powietrze %in% c("wi?cej")))))

#obliczanie centyli obrony zawodnik?w w rzeczywisto?ci
centyle_obrony_real <- real22 %>% 
  filter((Pos == "FW"|Pos == "FWDF"| Pos == "FWMF")) %>%
  summarise(kwantyl_wygrane_wslizgi = quantile(as.numeric(TklW),probs = seq(0.01, 1, 1/100)), 
            kwantyl_zablokowane_podanie = quantile(as.numeric(Blocks),probs = seq(0.01, 1, 1/100)),
            kwantyl_obrona_strzal= quantile(as.numeric(ScaDef),probs = seq(0.01, 1, 1/100)))



#obliczanie centyli obrony Ronaldo w rzeczywisto?ci
centyle_obrony_Ronaldo_real <-  centyle_obrony_real %>%
  mutate(kwantyl_wygrane_wslizgi = case_when(as.numeric(CR7real$TklW) < kwantyl_wygrane_wslizgi  ~ "mniej",
                                           TRUE ~ "wi?cej"),
         kwantyl_zablokowane_podanie = case_when(as.numeric(CR7real$Blocks) < kwantyl_zablokowane_podanie  ~ "mniej",
                                      TRUE ~ "wi?cej"), 
         kwantyl_obrona_strzal = case_when(as.numeric(CR7real$ScaDef) < kwantyl_obrona_strzal  ~ "mniej",
                                               TRUE ~ "wi?cej"))
centyle_obrony_Ronaldo_suma_real <- mean(c(max(which(centyle_obrony_Ronaldo_real$kwantyl_wygrane_wslizgi %in% c("wi?cej"))),
                                                max(which(centyle_obrony_Ronaldo_real$kwantyl_zablokowane_podanie %in% c("wi?cej"))),
                                                max(which(centyle_obrony_Ronaldo_real$kwantyl_obrona_strzal %in% c("wi?cej")))))
#rysowanie wykresu
statystyki <- data.frame(
  Strzaly = c(max(which(centyle_Ronaldo_fifa23$ShootingTotal %in% c("wi?cej"))), centyle_strzlow_Ronaldo_suma_real), 
  Podania = c(max(which(centyle_Ronaldo_fifa23$PassingTotal %in% c("wi?cej"))), centyle_podan_Ronaldo_suma_real),
  Fizycznosc = c(max(which(centyle_Ronaldo_fifa23$PhysicalityTotal %in% c("wi?cej"))), centyle_fizycznosci_Ronaldo_suma_real),
  Obrona = c(max(which(centyle_Ronaldo_fifa23$DefendingTotal %in% c("wi?cej"))), centyle_obrony_Ronaldo_suma_real), 
  Drybling = c(max(which(centyle_Ronaldo_fifa23$DribblingTotal %in% c("wi?cej"))), centyle_dryblingu_Ronaldo_suma_real)
)
statystyki <- statystyki %>% 
  transmute(Shooting=Strzaly, Dribbling = Drybling, Defending = Obrona,  Fiz = Fizycznosc, Passnig= Podania)
max_min <- data.frame(row.names = c("Ronaldo_fifa","Ronaldo_real"),
                      Shooting = c(100, 0), Passnig = c(100, 0), Fiz = c(100, 0),
                      Defending = c(100, 0), Dribbling = c(100, 0))
rownames(max_min) <- c("Max", "Min")
rownames(statystyki) <- c("Fifa", "Rzeczywistosc")
df <- rbind(max_min, statystyki)
return(df)
}
######

CR<-wykres("Cristiano Ronaldo","Cristiano Ronaldo",  "Manchester Utd" ,real22,fifa23TOP5)
VJ <- wykres("Vinicius J\xfanior", "Vinícius Jr.", "Real Madrid" ,real22,fifa23TOP5)


par(bg="#1b2033")
radarchart(
  # VJ
  CR, axistype = 1,
  # Customize the polygon
  pcol = c("#ccf53f", "#cb3df0"), pfcol = scales::alpha(c("#ccf53f", "#cb3df0"), 0.7), plwd = 2, plty = 1,
  # Customize the grid
  cglcol = "white", cglty = 1, cglwd = 1.2,
  # Customize the axis
  axislabcol = "#1b2033", 
  # Variable labels
  vlcex = 0.7, vlabels = colnames(CR),
  caxislabels = c(0, 25, 50, 75, 100),
)




