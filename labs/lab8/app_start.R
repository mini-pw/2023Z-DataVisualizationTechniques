library(shiny)
library(palmerpenguins)
library(ggplot2)
library(plotly)
library(bslib)
library(dplyr)
data("penguins")


ui <- fluidPage(
  
  titlePanel("Analiza danych o pingwinach"),
  
  
  fluidRow(
    column(6, 
           
    ),
    column(6,

    )
  ),
  fluidRow(
    column(6,
           plotlyOutput("pointPlot")
    ),
    column(6,
           plotlyOutput("histPlot")
    )
  ),
  fluidRow(
    column(6, 
            tableOutput("table")
    )
  )
)


server <- function(input, output) {
  
  output$table <- renderTable({
   
  })
  
  output$text <- renderText({
    
  })
  
  output$pointPlot <- renderPlotly({
   
  })
  
  output$histPlot <- renderPlotly({
   
  })
  
}


shinyApp(ui = ui, server = server)

