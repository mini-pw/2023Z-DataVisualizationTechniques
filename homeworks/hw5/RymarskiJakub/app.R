library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(dplyr)
library(rsconnect)
library(packrat)

#Wybieram interesujące mnie dane

#setwd("C:/Users/Kuba/Studia/PW/R/Pracadomowa/app")
df <-read.csv("dane.csv", sep=";", header=T)
Uczelnie=c("UW", "PW", "SGHW")
Uczelnie2=c("PW", "PG", "PP", "PO", "PB")
df2<-df %>% filter(P_UCZELNIA_SKROT %in% Uczelnie & P_POZIOM==1)

df3<-df2 %>% select(P_ROKDYP,P_UCZELNIA_SKROT, P_E_ZAR, P_ME_ZAR)  
df3$P_E_ZAR=as.numeric(sub(',', '.', df3$P_E_ZAR))
df3$P_ME_ZAR=as.numeric(sub(',', '.', df3$P_ME_ZAR))


df3 <- df3 %>% group_by(P_ROKDYP, P_UCZELNIA_SKROT) %>% 
  summarise(Średnia_płaca = mean(P_E_ZAR, na.rm=T),
            Mediana_płacy = mean(P_ME_ZAR, na.rm=T), .groups="drop")
#df3
df4<-df %>% filter(P_UCZELNIA_SKROT %in% Uczelnie2, P_POZIOM=="1") %>% 
  select(P_UCZELNIA_SKROT, P_N, P_PROC_DOSW, P_CZAS_PRACA) %>% 
  mutate(Liczba_absolwentów_kierunku=P_N,
         Procent_osób_które_pracowały_przed_uzyskaniem_dyplomu=P_PROC_DOSW)
df4$P_CZAS_PRACA=as.numeric(sub(',', '.', df4$P_CZAS_PRACA))
#df4



ui1 <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "united"),
  titlePanel("Zarobki absolwentów uczelni."),
  
  
  fluidRow(
    column(6, 
           checkboxGroupInput("uczelnia",
                              "Którą uczelnię wybierasz?",
                              unique(df3$P_UCZELNIA_SKROT)),
           
           sliderInput("zakres",
                       "Rok",
                       value = c(min(df3$P_ROKDYP), max(df3$P_ROKDYP)),
                       min = min(df3$P_ROKDYP),
                       max = max(df3$P_ROKDYP),
                       step = 1),
    ),
    column(10,
           plotlyOutput("pointPlot")
    ),
  )
)





ui2 <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "united"),
  titlePanel("Średni czas znalezienia pracy po ukończeniu studiów pierwszego stopnia dla politechnik."),
  
  fluidRow(
    column(6, 
           radioButtons("uczelnia2",
                              "Którą politechnikę wybierasz?",
                              unique(df4$P_UCZELNIA_SKROT))
    ),
    column(10,
           plotlyOutput("pointPlot2")
    ),
  )
  
)

server <- function(input, output) {

  
  output$pointPlot <- renderPlotly({
    plot_ly(df3 %>% filter(P_UCZELNIA_SKROT %in% input$uczelnia, 
                           P_ROKDYP<=input$zakres[2],
                           P_ROKDYP>=input$zakres[1]),
            x = ~P_ROKDYP, 
            y = ~Średnia_płaca, 
            color=~P_UCZELNIA_SKROT, 
            type = 'scatter', 
            mode = 'markers',
            colors = "Set1") %>% 
      layout(title = "Wykres średniej płacy absolwenta w zależności od roku otrzymania dyplomu", 
             xaxis = list(title = 'Rok'), 
             yaxis = list(title = 'Średnia miesięczna płaca', range=c(0,8000)))
    
    
  })
  
  output$pointPlot2 <- renderPlotly({
    
    plot_ly(df4 %>% filter(P_UCZELNIA_SKROT %in% input$uczelnia2), 
           x = ~P_CZAS_PRACA,
           type='histogram',
           nbinsx=30) %>% 
      layout(title = "Wykres średniego czasu od uzyskania dyplomu do znalezienia pierwszej pracy po studiach", 
             xaxis = list(title = 'Średni czas w miesiącach', range=c(0,30)),
             yaxis = list(title = 'Ilość kierunków na przestrzeni lat 2014-2020', range=c(0,60)))
    
  })
}

app_ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "united"),
  
  title = "Analiza ekonomicznych losów absolwentów",
  tabPanel("Zarobki", ui1,
           icon = icon("dollar-sign")
  ),
  tabPanel("Czas znaleznienia pracy", ui2,
           icon = icon("clock")
  ),
  
)




shinyApp(ui = app_ui, server = server)

