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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Philadelphia houses"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "property", "Property:",
                c("Year built"="yearBuilt",
                  "Opening bid"="Opening.Bid",
                  "Water"="Water",
                  "PGW"="PGW",
                  "School Score"="School.Score",
                  "Violent crime rate"="Violent.Crime.Rate")
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("boxPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    df <- read.csv("Properties_philly_Kraggle_v2.csv")

    output$boxPlot <- renderPlotly({
        df %>% 
            filter(yearBuilt!=0) -> df 
        df[,"prop"] <- df[,input$property]
        plot_ly(
            data = df,
            y = ~prop,
            x = ~PropType,
            type = "box"
            ) %>% layout(
                updatemenus = list(
                    list(
                        x = 1, y = 1,
                        buttons = list(
                            list(method = "restyle",
                                 args = list("type", "box"),
                                 label = "Boxplot"),
                            list(method = "restyle",
                                 args = list("type", "violin"),
                                 label = "Violinplot")
                        ))
                ),
                xaxis=list(fixedrange=TRUE),
                yaxis=list(fixedrange=TRUE))
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
