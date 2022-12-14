library(shiny)
library(plotly)

df <-
  as.data.frame(read.csv("graduates-national-data.csv", sep = ";"))

ui <- fluidPage(
  titlePanel(title = "Praca domowa 5", windowTitle = "Homework 5"),
  
  sidebarLayout(
    position = "right",
    
    sidebarPanel(),
    
    mainPanel(
      fluidRow(column(8, plotlyOutput(outputId = "plot01")),
               column(
                 2,
                 checkboxGroupInput(inputId = "checkbox", "Ukończony poziom studiów", unique(df$PL_POZIOM)),
               )),
      fluidRow(column(
        8, plotlyOutput(outputId = "plot02")
      ),
      column(
        2,
        sliderInput(
          inputId = "slider",
          label = "Liczba absolwentów, występujących w ZUS",
          min = min(df$PL_N_WZUS),
          max = max(df$PL_N_WZUS),
          value = c(min(df$PL_N_WZUS), max(df$PL_N_WZUS)),
          step = 1
        )
      )),
      fluidRow()
    )
  )
)

server <- function(input, output, session) {
  output$plot01 <- renderPlotly({
    plot_ly(
      data = df[df$PL_POZIOM %in% input$checkbox, ][input$slider[1] <= df$PL_N_WZUS &
                                                      df$PL_N_WZUS <= input$slider[2], ],
      x =  ~ PL_ROKDYP,
      y =  ~  PL_N,
      type = "bar",
      color =  ~ PL_POZIOM
    ) %>% layout(
      title = "Zależność ilości absolmentów od roku",
      xaxis = list(title = 'Rok'),
      yaxis = list(title = "Ilość absolwentów")
    )
  })
  
  output$plot02 <- renderPlotly({
    plot_ly(
      data = df[df$PL_POZIOM %in% input$checkbox, ][input$slider[1] <= df$PL_N_WZUS &
                                                      df$PL_N_WZUS <= input$slider[2], ],
      x =  ~ PL_ROKDYP,
      y =  ~ PL_PROC_MIES_BEZR,
      type = "box",
      color =  ~ PL_POZIOM
    ) %>% layout(
      title = "Zależność wskaźnija bezrobocia od roku",
      xaxis = list(title = "Rok"),
      yaxis = list(title = "Wskaźnik bezrobocia")
    )
  })
}

shinyApp(ui, server)