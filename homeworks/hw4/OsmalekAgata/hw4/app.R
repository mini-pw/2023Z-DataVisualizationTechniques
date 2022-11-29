#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(PogromcyDanych)
df<-read.csv(file = "Properties_philly_Kraggle_v2.csv", sep = ',')

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Philadelphia Real Estate"),
  
  # Sidebar with a slider input for number of bins 
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
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    
    # draw the histogram with the specified number of bins
    
    # df<-read.csv(file = "Properties_philly_Kraggle_v2.csv", sep = ',')
    if(input$checked){
      df<-df[df$School.Score>30,]
    }
    
    ggplot(df[df$PropType == input$Prop,], aes(x = yearBuilt,y = Opening.Bid))+
      geom_point()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
