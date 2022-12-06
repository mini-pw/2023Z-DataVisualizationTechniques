#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(dplyr)
df<-read.csv(file = "Properties_philly_Kraggle_v2.csv", sep = ',')


ui <- fluidPage(
  

  titlePanel("Philadelphia Real Estate"),
  

  sidebarLayout(
    sidebarPanel(
      selectInput(
        "Prop",
        "Choose a property type:",
        sort(as.character(unique(df$PropType))),
        selected = "Condominium"
      ),
      checkboxInput(
        "checked",
        "Include only estates with school score above 30?",
        FALSE
      )
    ),
    
    mainPanel(
      plotlyOutput("distPlot")
    )
  )
)


server <- function(input, output) {
  

  
  output$distPlot <- renderPlotly({

    if(input$checked){
      df<-df[df$School.Score>30,]
    }
    df<-df[df$year>0,]
    
    #ggplot(df[df$PropType == input$Prop,], aes(x = yearBuilt,y = Opening.Bid))+
     # geom_point()
    
    plot_ly(
      data = df[df$PropType == input$Prop,], 
      x = ~yearBuilt, 
      y = ~Opening.Bid,
      type="scatter",
      mode="markers",
      text = ~School.Score,
      hovertemplate = paste('<br><b>year</b>: %{x}</br>',
                            '<br><b>Opening bid</b>: %{y}</br>',
                            "<br><b>School score</b>: %{text}</br>",
                            '<extra></extra>')
    )
      
  })
}

shinyApp(ui = ui, server = server)
