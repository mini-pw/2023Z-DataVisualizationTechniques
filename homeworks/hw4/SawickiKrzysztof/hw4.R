library("dplyr")
library("plotly")
df_kraggle<-read.csv("Properties_philly_Kraggle_v2.csv")

#Przygotowanie ramki danych

df <- df_kraggle %>% filter(!(is.na(Avg.Walk.Transit.score))) %>% 
  rename("Transit" = "Avg.Walk.Transit.score") %>% 
  mutate(Transit = case_when(
    0 <= Transit & Transit < 40 ~ "Minimal Transit",
    40 <= Transit & Transit < 60 ~ "Some Transit",
    60 <= Transit & Transit < 75 ~ "Good Transit",
    75 <= Transit & Transit < 90 ~ "Excellent Transit",
    90 <= Transit & Transit < 100 ~ "Rider's Paradise"
  )) %>% mutate(Transit = factor(Transit, levels = c("Minimal Transit", 
                                            "Some Transit", "Good Transit",
                                            "Excellent Transit", "Rider's Paradise"))) %>% 
mutate(Violent.Crime.Rate = case_when(
    0 <= Violent.Crime.Rate & Violent.Crime.Rate < 0.3 ~ "Very low Crime Rate",
    0.3 <= Violent.Crime.Rate & Violent.Crime.Rate < 0.5 ~ "Low Crime Rate",
    0.5 <=Violent.Crime.Rate & Violent.Crime.Rate < 0.8 ~ "Medium  Crime Rate",
    0.8 <= Violent.Crime.Rate & Violent.Crime.Rate < 1.3 ~ "High Crime Rate",
    1.3 <= Violent.Crime.Rate & Violent.Crime.Rate < 2 ~ "Very high Crime Rate"
  )) %>% 
  mutate(Violent.Crime.Rate = factor(Violent.Crime.Rate, 
                                 levels = c(
                                   "Very low Crime Rate", 
                                    "Low Crime Rate", 
                                    "Medium  Crime Rate",
                                    "High Crime Rate", 
                                    "Very high Crime Rate")))



#Wykres

fig <- plot_ly(
  data = df,
  x = ~Transit,
  type = "histogram",
  color = ~Violent.Crime.Rate, 
  frame = ~PropType
)  %>% layout(
  title = "Plot of number of real estates with diffrent transit categories",
  xaxis = list(title = "Transit Description"),
  yaxis = list(title = "Number of real estates"),
  legend = list(x = 1, y = 0.8, title = list(text = "Crime rate"))
)

fig_animated <- fig %>% 
  animation_slider(currentvalue = list(prefix = "PropType: ")) 

fig_animated

