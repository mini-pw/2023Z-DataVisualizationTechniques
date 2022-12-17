library(shiny)
library(dplyr)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(shinycssloaders)
library(tidyr)
library(plotly)
library(rsconnect)


df1 <- read.csv("JD.csv")
colnames(df1)
colnames(df1) <- c("Rok"      ,          "P_KIERUNEK_ID"    ,         
                   "P_CZAS_PRACA"         ,      "P_E_ZAR_STUD"      ,        
                   "P_E_ZAR_STUD_P1"     ,       "P_E_ZAR_STUD_P2"  ,         
                   "P_E_ZAR_STUD_P3"      ,      "P_E_ZAR_STUD_P4"  ,         
                   "P_E_ZAR_STUD_P5")

df1 <- sapply(df1, extract_numeric)
df1 <- as.data.frame(df1)




ui <- shinyUI(fluidPage(
  
  titlePanel("Analiza losow studentow"),
  
  sidebarLayout(position = "right",
                sidebarPanel(
                  
                  selectInput("rok", "Wybierz rok po studiach", choices = c(
                    "Zaraz po studiach" = "P_E_ZAR_STUD",
                    "Pierwszy rok" = "P_E_ZAR_STUD_P1",
                    "Drugi rok" = "P_E_ZAR_STUD_P2",
                    "Trzeci rok" = "P_E_ZAR_STUD_P3",
                    "Czwarty rok" = "P_E_ZAR_STUD_P4",
                    "Piaty rok" = "P_E_ZAR_STUD_P5"))
                ),
                mainPanel(
                  plotlyOutput("distPlot"),
                  textOutput("tekst")
                )
  )
))
ui2 <- shinyUI(fluidPage(
  
  titlePanel("Analiza losow studentow"),
  
  sidebarLayout(position = "left",
                sidebarPanel(
                  
                  sliderInput("rokSuwak", "Wybierz lata", min = 2015, max = 2020, value = c(2015,2020))
                ),
                mainPanel(
                  plotlyOutput("Plot2"),
                  textOutput("tekst2")
                )
  )
))
server <- function(input, output) {
  # output$tekst <- renderText({
  #   "Zaleznosc zarobkow w danym roku po uzyskaniu dyplomu\nmiedzy osobami z doswiadczeniem w pracy z roznych kierunkow\nna wybranych kierunkach"
  #   
  # })
  output$distPlot <- renderPlotly({
    p1 <- ggplotly(df1 %>% 
                     select(Rok, input$rok, P_KIERUNEK_ID) %>% 
                     na.omit() %>% 
                     ggplot(aes_string(x = input$rok, y = "P_KIERUNEK_ID")) +
                     geom_point(size = 1) + 
                     ylab("Kierunki") + xlab("Zarobki") + ylim(c(0,10000))  + xlim(c(0,12000))+ 
                     theme(axis.text.y = element_blank())
    )
    ggplotly(p1)
    
    
  })
  output$tekst2 <- renderText({"Sredni czas (w miesiacach) od uzyskania dyplomu\ndo podjęcia pierwszej pracy po uzyskaniu dyplomow poszczegolnych latach"})
  output$tekst <- renderText({"Zaleznosc zarobkow w danym roku po uzyskaniu dyplomu\nmiedzy osobami z doswiadczeniem w pracy z roznych kierunkow\nna wybranych kierunkach"})
  output$Plot2 <- renderPlotly({
    
    p2 <- df1 %>% 
      filter(P_KIERUNEK_ID<15000) %>% 
      filter(Rok  >= input$rokSuwak[1], Rok<=input$rokSuwak[2]) %>% 
      ggplot(aes(x = P_KIERUNEK_ID, y = P_CZAS_PRACA, text = paste(
        "ID kierunku ", P_KIERUNEK_ID,"<br>",
        "Miesiace ", P_CZAS_PRACA))
      )+geom_point(size = 1) + ylab("Czas [msc]") + xlab("Kierunek") + theme(axis.text.x = element_blank())
    
    
    ggplotly(p2, tooltip = c("text"))
  })}



app_ui <- navbarPage(
  title = "Analiza danych o studentach",
  
  tabPanel("Czas znalezienia pracy", ui2),
  tabPanel("Zarobki po studiach", ui),
  theme = bslib::bs_theme(bootswatch = "cosmo"))
shinyApp(app_ui, server)

