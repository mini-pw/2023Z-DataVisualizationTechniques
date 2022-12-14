library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(rlang)
library(stringr)

df <- read.csv("graduates-major-data.csv", sep = ";")

ui <- fluidPage(

    titlePanel("Zarobki po studiach"),

    
    sidebarLayout(
      
      sidebarPanel(
        
        sliderInput(
          "year",
          "Rok ukonczenia studiow",
          min = 2014,
          max = 2020,
          value = c(2017, 2019),
          step = 1
        ),
        selectInput(
          "month",
          "Miesiac po ukonczeniu studiow",
          choices = 1:60
        )
        
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Wykres 1", plotOutput("plot1")),
          tabPanel("Wykres 2", plotOutput("plot2"))
        )
      )
      
    )
)


server <- function(input, output) {

  output$plot1 <- renderPlot({
    
    month <- paste("P_WWB_MIES_", as.character(input$month), sep = "")
    month1 <- paste("P_WWZ_MIES_", as.character(input$month), sep = "")
    
    expr3 <- paste("df4 %>% dplyr::mutate(", month1, " = as.numeric(", month1, "))", sep = "")
    expr2 <- paste("df3 %>% dplyr::mutate(", month1, " = str_replace(", month1, ", ',', '.'))", sep = "")
    expr1 <- paste("df2 %>% dplyr::mutate(", month, " = as.numeric(", month, "))", sep = "")
    expr <- paste("df1 %>% dplyr::mutate(", month, " = str_replace(", month, ", ',', '.'))", sep = "")
    df %>% 
      filter(P_ROKDYP <= input$year[2], P_ROKDYP >= input$year[1], as.name(month1) != "", as.name(month) != "") -> df1
    eval(parse_expr(expr)) -> df2
    eval(parse_expr(expr1)) -> df3
    eval(parse_expr(expr2)) -> df4
    eval(parse_expr(expr3)) -> df5
    df5 %>% filter(as.name(month1) > 0, as.name(month) > 0) -> df6
    
    ggplot(df6,
           aes_string(x = month, y = month1, color = "P_POZIOM")) +
             geom_point() +
             theme_bw() +
      labs(x = paste("WWB po ", as.character(input$month), " miesiecu", sep = ""),
           y = paste("WWZ po ", as.character(input$month), " miesiecu", sep = ""),
           color = "Stopien ukonczonych studiow") +
      scale_x_continuous(expand = c(0, 0), limits = c(0, 5)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 3))
  })
  
  output$plot2 <- renderPlot({
    
    ggplot(df %>% 
             filter(P_ROKDYP <= input$year[2], P_ROKDYP >= input$year[1], P_ZAR_ETAT_MAX != "") %>%
             mutate(P_ZAR_ETAT_MAX = str_replace(P_ZAR_ETAT_MAX, ',', '.')) %>%
             mutate(P_ZAR_ETAT_MAX = as.numeric(P_ZAR_ETAT_MAX), P_ROKDYP = as.character(P_ROKDYP)) %>%
             group_by(P_ROKDYP) %>%
             summarise(m = max(P_ZAR_ETAT_MAX)),
           aes(x = P_ROKDYP, y = m)) + 
      geom_bar(stat = 'identity', fill = '#3bc6f5') + 
      theme_bw() +
      scale_y_continuous(expand = c(0, 0)) +
      labs(x = "Rok ukonczenia studiow", 
           y = "Maksymalne srednie miesieczne wynagrodzenie wsrod absolwentow")
    
  })
}

shinyApp(ui = ui, server = server)
