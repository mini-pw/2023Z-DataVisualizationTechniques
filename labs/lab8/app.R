#install.packages("palmerpenguins")
library(shiny)
library(palmerpenguins)
library(ggplot2)
library(plotly)
library(bslib)
library(dplyr)
data("penguins")

### Zadanie 5 ###
zmienne <- c("bill_length_mm",
             "bill_depth_mm",
             "flipper_length_mm",
             "body_mass_g")
### Koniec - Zadanie 5 ###

ui <- fluidPage(
    ### Zadanie 9 ###
    theme = bs_theme(bootswatch = "minty"),
    ### Koniec - Zadanie 9 ###

    titlePanel("Analiza danych o pingwinach"),
    ### Zadanie 1 ###
    textOutput("text"),
    ### Koniec - Zadanie 1 ###
    
       fluidRow(
            column(6, 
                   ### Zadanie 2 ###
                   checkboxGroupInput("species",
                                      "Który gatunek pingwinów wybierasz?",
                                      unique(penguins$species)),
                   ### Koniec - Zadanie 2 ###  
                   
                   ### Zadanie 3 ###
                   sliderInput("zakres",
                               "Rok",
                               value = c(min(penguins$year), max(penguins$year)),
                               min = min(penguins$year),
                               max = max(penguins$year),
                               step = 1)
                   ### Koniec - Zadanie 3 ###
                   ),
            column(6,
                   ### Zadanie 5 ### 
                   selectInput("zmienna",
                               "Dla jakiej zmiennej narysować rozkład?",
                               zmienne),
                   ### Koniec - Zadanie 5 ###
                   
                   ### Zadanie 8 ###
                   uiOutput("zmienna2")
                   ### Koniec - Zadanie 8 ###
                  ),
                ),
        fluidRow(
            column(6,
                   ### Zadanie 4 ###
                   plotlyOutput("pointPlot")
                   ### Koniec - Zadanie 4 ###
                   ),
            column(6,
                   ### Zadanie 6 ###
                   plotlyOutput("histPlot")
                   ### Koniec - Zadanie 6 ###
                   )
            ),
        fluidRow(
          column(6, 
                 ### Zadanie 7 ###
                 dataTableOutput("table")
                 ### Koniec - Zadanie 7 ###
                 ),
          column(6, 
                 ### Zadanie 8 ###
                 plotOutput("pointPlot2")
                 ### Koniec - Zadanie 8 ###
          ),
          
          )
    )
        



server <- function(input, output) {
    ### Zadanie 7 ###
    output$table <- renderDataTable({
      
        penguins %>% 
            select(-year, -sex) %>% 
            group_by(species, island) %>% 
            summarise_all(funs(mean(., na.rm=T)))
      
    })
    ### Koniec - Zadanie 7 ###
    
    ### Zadanie 1 ###
    output$text <- renderText(({
      
        paste("Aplikacja zawiera wstępną analizę zbioru danych o pingwinach.", 
              "W zbiorze danych jest", 
              nrow(penguins), 
              "pingwinów.",
              "Najmniejszy waży",
              min(penguins$body_mass_g, na.rm = TRUE), "gramów!")
        
    }))
    ### Koniec - Zadanie 1 ###
    
    ### Zadanie 4 ###
    output$pointPlot <- renderPlotly({
        
        plot_ly(penguins %>% 
                  filter(species %in% input$species,
                         year >= input$zakres[1],
                         year <= input$zakres[2]), 
                x = ~flipper_length_mm,
                y = ~body_mass_g,
                color = ~species,
                colors = "Set1") %>% 
            layout(title = "Zależność długości skrzydła a masą ciała", 
                   xaxis = list(title = 'Długość skrzydła [mm]', range=c(162,241)), 
                   yaxis = list(title = 'Masa [g]', range=c(2500,6500)))
      
    })
    ### Koniec - Zadanie 4 ###
    
    ### Zadanie 6 ###
    output$histPlot <- renderPlotly({

        ggplotly(
            ggplot(penguins, aes_string(x = input$zmienna, fill = "species")) +
            geom_histogram(alpha = 0.4) + 
            theme_minimal() + 
            labs(title = paste("Rozkład dla zmiennej", input$zmienna)) + 
            scale_fill_manual(values = c("#e41a1c", "#377eb8", "#4daf4a"))
            )
      
    })
    ### Koniec - Zadanie 6 ###
    
    ### Koniec - Zadanie 8 ###
    output$zmienna2 <- renderUI({
      
      selectInput("zmienna2", "Druga zmienna", zmienne[!(zmienne %in% input$zmienna)]) 
      
      })
    
    output$pointPlot2 <- renderPlot({
      
      ggplot(penguins, aes_string(x = input$zmienna, y = input$zmienna2, color = "species")) + 
        geom_point() + 
        theme_minimal() + 
        labs(title = paste("Rozkład dla zmiennej", input$zmienna, "oraz", input$zmienna2)) + 
        scale_fill_manual(values = c("#e41a1c", "#377eb8", "#4daf4a"))
      
    })
    ### Koniec - Zadanie 8 ###
}


shinyApp(ui = ui, server = server)

