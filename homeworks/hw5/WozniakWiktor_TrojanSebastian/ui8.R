library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(packrat)
library(rsconnect)
library(bslib)





ui2 <- fluidPage(
  
  
  titlePanel("Kontynuacja nauki w zależności od dziedziny studiów"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "wybor",
        label = "Rodzaj studiów po uzyskaniu dyplomu studiów pierwszego stopnia:" ,
        choices = c(
          "Studia magisterskie" = "P_PROC_KONT",
          "Studia doktoranckie" = "P_PROC_DOKTORAT",
          "Podjęcie innych studiów" = "P_PROC_DYPLOM"
          
        )
      ),
      width = 12
    ),
    mainPanel(

      shinycssloaders::withSpinner(plotOutput("plot1", height = 400),
                                   type = getOption("spinner.type", default = 4),
                                   color = getOption("spinner.color", default = "#2c3e50"),
                                   size = getOption("spinner.size", default = 1)),
                                   
      width = 12
    )
  )
  
  
)
ui3 <- fluidPage(
  
  
  titlePanel("Przyszłość po studiach w zależności od dziedziny"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("rokPoStudiach",
                  "Lata po studiach:",
                  min = 1,
                  max = 5,
                  value = 1),
      selectInput(
        inputId = "typ2",
        label = "Wskaźnik:",
        choices = c(
          "Średnie zarobki" = "zarobki",
          "Ryzyko bezrobocia" = "bezrobocie"
        )
      ),
      
      selectInput(
        inputId = "dosw",
        label = "Doświadczenie:",
        choices = c(
          "Dla wszystkich studentów" = "P",
          "Z doświadczeniem w pracy" = "DOSW_P",
          "Bez doświadczenia w pracy" = "NDOSW_P"
        )
      ),
      width = 12,
      
    ),
    
    
    mainPanel(
      shinycssloaders::withSpinner(plotOutput("plot3", height = 400),
                                   type = getOption("spinner.type", default = 4),
                                   color = getOption("spinner.color", default = "#2c3e50"),
                                   size = getOption("spinner.size", default = 1)),
      width = 12
    )
  )
)


