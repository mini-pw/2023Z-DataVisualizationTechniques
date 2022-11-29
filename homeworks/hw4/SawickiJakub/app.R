#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
#dla run
df <- read.csv("Philadelphia.csv")

df <- na.omit(df)
df$Sale.Price.bid.price <- as.numeric(gsub('[$,]', '', df$Sale.Price.bid.price))

price_min <- min(df$Sale.Price.bid.price)
price_max <- max(df$Sale.Price.bid.price)

df <- df %>% 
  select(Sale.Price.bid.price, Sheriff.Cost, finished...SqFt., Violent.Crime.Rate, School.Score, Avg.Walk.Transit.score)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Handel nieruchomościami w Filadelfii"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("price",
                        "Wybierz zakres ceny",
                        min = price_min,
                        max = price_max,
                        value = c(price_min, price_max)),
            #wybór
            #checkboxGroupInput("properies", "Choose values:",
            #                   choiceNames =
            #                     list(icon("calendar"), icon("bed"),
            #                          icon("cog"), icon("bug")),
            #                   choiceValues =
            #                     list("calendar", "bed", "cog", "bug")
            #),
            selectInput("variable", "Wybierz pole", c("Sheriff.Cost", "finished...SqFt.", "Violent.Crime.Rate", "School.Score", "Avg.Walk.Transit.score")),
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
      
        df_box <- df[, c("Sale.Price.bid.price", input$variable)]
        df_box %>% 
          filter(Sale.Price.bid.price >= input$price[1] & Sale.Price.bid.price <= input$price[2]) -> df_box
        
        ggplot(df_box, aes(y = df_box[, 2])) +
        geom_boxplot() +
        labs(
          title = paste("Wykres zmiennej ", input$variable),
          x = "",
          y = "wartość"
        ) -> p
    
        p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
