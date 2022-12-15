# Przygotowanie danych

library(shiny)
library(ggplot2)
library(dplyr)
library(packrat)
library(rsconnect)

dane <- read.csv("https://www.ela.nauka.gov.pl/dataExperts/students/students-major-data.csv", sep = ";",
                 stringsAsFactors = F, encoding = "UTF-8")

head(dane)

# Shiny App

ui1 <- fluidPage(
  
  # Tytul
  titlePanel("Ilosc studentow uczacych sie na wybranym kierunku"),
  
  # Interfejs uzytkownika
  sidebarLayout(
    sidebarPanel(
      selectInput("uczelnia", "Uczelnia:",
                  c("Politechnika Warszawska" = "PW",
                    "Politechnika Krakowska" = "PK",
                    "Uniwersytet Warszawski" = "UW",
                    "Uniwersytet Jagiellonski" = "UJK")),
      selectInput("kierunek", "Kierunek:",
                  c("Informatyka",
                    "Ekonomia",
                    "Zarzadzanie"))
    ),
    mainPanel(
      plotOutput("plot")
    )
  ))

ui2 <- fluidPage(
  
  # Application title
  titlePanel("Procent studentow, ktorzy terminowo osiagneli okreslone lata studiow"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("P_ROK_OD",
                  "Rok rozpoczecia studiow",
                  min = 2015,
                  max = 2019,
                  value = c(2015, 2019),
                  step = 1),
      
      checkboxGroupInput("P_POZIOM",
                         "Stopien studiow",
                         choiceNames = c("pierwszy", "drugi", "JM"),
                         choiceValues = c("1", "2", "JM"),
                         selected = c("1", "2", "JM")),
      
      checkboxGroupInput("P_FORMA",
                         "Forma studiow",
                         choiceNames = c("stacjonarna", "niestacjonarna"),
                         choiceValues = c("S", "N"),
                         selected = c("S", "N"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("passing")
    )
  )
)

# Serwer

server <- function(input, output) {
  # Wykres 1: Udzial w kategoriach wynagrodzen
  output$plot <- renderPlot({
    df <- dane %>%
      filter(P_UCZELNIA_SKROT == input$uczelnia &
               P_KIERUNEK_NAZWA == input$kierunek)
    ggplot(data = df, aes(x = P_POZIOM, y = P_N)) +
      geom_col() +
      labs(x = "Stopien, studiow", y = "Liczba osob")
  })
  
  df_simple <- dane %>%
    select(1, 3:10,-5)
  
  output$passing <- renderPlot({
    df_simple %>%
      filter(P_ROK_OD >= input$P_ROK_OD[1], P_ROK_OD <= input$P_ROK_OD[2]) %>%
      filter(P_POZIOM == input$P_POZIOM) %>%
      filter(P_FORMA == input$P_FORMA) %>%
      summarise(
        two = mean(as.numeric(P_TERM_2), na.rm = T),
        three = mean(as.numeric(P_TERM_3), na.rm = T),
        four = mean(as.numeric(P_TERM_4), na.rm = T),
        five = mean(as.numeric(P_TERM_5), na.rm = T),
      ) -> res
    
    y <- c(res$two, res$three, res$four, res$five)
    x <- c("drugi", "trzeci", "czwarty", "piaty")
    x <- factor(x, levels = x)
    A <- data.frame(x, y)
    
    A %>%
      ggplot(aes(x = x, y = y)) +
      geom_col() +
      labs(x = "Rok",
           y = "Procent studentow")
  })
}

app_ui <- navbarPage(title = "Analiza danych: Losy studentow w Polsce",
                     tabPanel("Pierwszy wykres", ui1),
                     tabPanel("Drugi wykres", ui2))

# Uruchomienie aplikacji
shinyApp(ui = app_ui, server = server)
