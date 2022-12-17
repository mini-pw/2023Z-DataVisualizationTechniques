library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shinycssloaders)

df <- read.csv("https://www.ela.nauka.gov.pl/dataExperts/graduates/graduates-major-data.csv", sep = ";") %>% 
  select(contains("P_WWZ_MIES"),contains("P_WWB_MIES"),
             "P_KIERUNEK_NAZWA","P_NAZWA_UCZELNI","P_DZIEDZINA","P_WOJ","P_ROKDYP")

dziedziny <- unique(df$P_DZIEDZINA)
dziedziny <- dziedziny[dziedziny != ""]

ui1 <- fluidPage(
  theme = bslib::bs_theme(
    bg = "#282828",
    primary = "#9D00FF",
    secondary ="#9D00FF",
    info = "#9D00FF",
    fg = "white",
    base_font = "Helvetica"
  ),
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #9D00FF}")),
  titlePanel("Filtruj"),
  fluidRow( 
    column(3,
           selectInput(
             inputId = "uczelnia",
             label = "Wybierz uczelnie",
             choices = unique(df$P_NAZWA_UCZELNI),
             multiple = TRUE
           )),
    column(4,
           offset = 1,
           checkboxGroupInput(
             inputId = "dziedzina",
             label = "Wybierz dziedzinę",
             choices = dziedziny)),
    column(3,
           sliderInput(
             inputId = "rok",
             label = "Wybierz lata ukończenia studiów",
             min = min(df$P_ROKDYP),
             max = max(df$P_ROKDYP),
             value = c(2014,2020),
             round = TRUE,
             step = 1,
             sep = "")
    ))
)


ui2 <- fluidPage(
  theme = bslib::bs_theme(
    bg = "#282828",
    fg = "white",
    base_font = "Helvetica"
  ),
  titlePanel("Średni Względny Wskaźnik Zarobków w kolejnych miesiącach po uzyskaniu dyplomu"),
  mainPanel(shinycssloaders::withSpinner(plotOutput("WWZPlot"),
                                         type = getOption("spinner.type", default = 4),
                                         color = getOption("spinner.color", default = "#9D00FF"),
                                         size = getOption("spinner.size", default = 1)),
            width = 12)
)
ui3 <- fluidPage(
  theme = bslib::bs_theme(
    bg = "#282828",
    fg = "white",
    base_font = "Helvetica"
  ),
  titlePanel("Średni Względny Wskaźnik Bezrobocia w kolejnych miesiącach po uzyskaniu dyplomu"),
  mainPanel(shinycssloaders::withSpinner(plotOutput("WWBPlot"),
                                         type = getOption("spinner.type", default = 4),
                                         color = getOption("spinner.color", default = "#9D00FF"),
                                         size = getOption("spinner.size", default = 1)),
            width = 12)
)


server <- function(input, output, session) {
  output$WWZPlot <- renderPlot({
    df %>% 
      filter(if(length(input$uczelnia)>0) P_NAZWA_UCZELNI %in% input$uczelnia else TRUE) %>% 
      filter(if(length(input$dziedzina)>0) P_DZIEDZINA %in% input$dziedzina else TRUE) %>%
      filter(P_ROKDYP >= input$rok[1] & P_ROKDYP <= input$rok[2]) %>%
      select(contains("P_WWZ_MIES")) %>%
      pivot_longer(everything(),names_to = "miesiac",values_to = "WWZ") %>%
      mutate(miesiac = as.numeric(gsub("[a-zA-Z_]+","",miesiac)),
             WWZ = as.numeric(gsub(",",".",WWZ))) %>% 
      group_by(miesiac) %>% 
      summarise(WWZ = mean(WWZ, na.rm = TRUE)) %>% 
      ggplot(aes(x = miesiac, y = WWZ)) +
      geom_line(size = 1.5, color = "#9D00FF") +
      scale_y_continuous(expand = c(0,0),limits = c(0,1)) +
      scale_x_continuous(expand = c(0,0)) +
      labs(y = "Średni Względny Wskaźnik Zarobków",
           x = "Liczba miesięcy") +
      theme(plot.background = element_rect(fill = "#282828",color = "#282828"),
            panel.background = element_rect(fill = "#282828"),
            text = element_text(colour = "#ffffff"),
            axis.line = element_line(colour = "#ffffff"),
            axis.text = element_text(colour = "#ffffff", size = 12),
            axis.title = element_text(size = 14, colour = "#ffffff", face = "bold"),
            panel.grid = element_line(colour = "#ffffff",size = 0.1,linetype="dashed"),
            panel.border = element_blank(),
            plot.margin = unit(c(1, 1, 1, 1), "cm"))
  }) %>% 
  bindCache(input$uczelnia,input$dziedzina, input$rok)
  output$WWBPlot <- renderPlot({
    df %>% 
      filter(if(length(input$uczelnia)>0) P_NAZWA_UCZELNI %in% input$uczelnia else TRUE) %>% 
      filter(if(length(input$dziedzina)>0) P_DZIEDZINA %in% input$dziedzina else TRUE) %>%
      filter(P_ROKDYP >= input$rok[1] & P_ROKDYP <= input$rok[2]) %>% 
      select(contains("P_WWB_MIES")) %>%
      pivot_longer(everything(),names_to = "miesiac",values_to = "WWB") %>%
      mutate(miesiac = as.numeric(gsub("[a-zA-Z_]+","",miesiac)),
             WWB = as.numeric(gsub(",",".",WWB))) %>% 
      group_by(miesiac) %>% 
      summarise(WWB = mean(WWB, na.rm = TRUE)) %>% 
      ggplot(aes(x = miesiac, y = WWB)) +
      geom_line(size = 1.5, color = "#9D00FF") +
      scale_y_continuous(expand = c(0,0),limits = c(0,1)) +
      scale_x_continuous(expand = c(0,0)) +
      labs(y = "Średni Względny Wskaźnik Bezrobocia",
           x = "Liczba miesięcy") +
      theme(plot.background = element_rect(fill = "#282828",color = "#282828"),
            panel.background = element_rect(fill = "#282828"),
            text = element_text(colour = "#ffffff"),
            axis.line = element_line(colour = "#ffffff"),
            axis.text = element_text(colour = "#ffffff", size = 12),
            axis.title = element_text(size = 14, colour = "#ffffff", face = "bold"),
            panel.grid = element_line(colour = "#ffffff",size = 0.1,linetype="dashed"),
            panel.border = element_blank(),
            plot.margin = unit(c(1, 1, 1, 1), "cm"))
  }) %>% 
    bindCache(input$uczelnia,input$dziedzina, input$rok)
}

app_ui <- navbarPage(
  title = "Analiza ekonomicznych losów absolwentów uczelni w Polsce",
  tabPanel("Filtruj",ui1,
           icon = icon("glyphicon glyphicon-glass",lib = "glyphicon")),
  tabPanel("Zarobki",ui2,
           icon = icon("glyphicon glyphicon-usd",lib = "glyphicon")),
  tabPanel("Bezrobocie",ui3,
           icon = icon("glyphicon glyphicon-search",lib = "glyphicon")),
  footer = shiny::HTML("
                <footer class='text-center text-sm-start'
                style='width:100%; background-color : #282828; color: #ffffff;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  Copyright © 2022 Marta Szuwarska
                </p>
                </footer>
                ")
)

shinyApp(app_ui, server)