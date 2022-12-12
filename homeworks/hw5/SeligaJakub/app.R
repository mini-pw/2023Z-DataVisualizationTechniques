library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)



uczelnia <- read.csv("graduates-institution-data.csv", sep=";", encoding = "UTF-8")
kierunek <- read.csv("graduates-major-data.csv", sep=";", encoding = "UTF-8") %>% 
  filter(P_DZIEDZINA!="")

server <- function(input, output, session) {
  
  output$barPlot <- renderPlot({
    
    
    years <- paste0("U_ME_ZAR_P", input$lataPoDyplomie)
    
    uczelnia %>% 
      select(years, U_NAZWA_UCZELNI, U_POZIOM, U_N, U_ROKDYP) %>% 
      rename(zarobki = 1) %>% 
      filter(U_POZIOM==input$rodzajStudiow, U_ROKDYP==input$rokDyplomu, U_N>=input$ileAbsolwentow[1], U_N<=input$ileAbsolwentow[2]) %>% 
      select(zarobki, U_NAZWA_UCZELNI, U_POZIOM, U_N, U_ROKDYP) %>% 
      mutate(zarobki = as.numeric(gsub(",", ".", zarobki))) %>% 
      group_by(U_NAZWA_UCZELNI) %>% 
      arrange(desc(zarobki)) %>% 
      head(as.integer(input$showMax))%>% 
      ggplot(aes(x=zarobki, y=reorder(U_NAZWA_UCZELNI,zarobki)))+
      geom_bar(stat='identity', fill="#375A7F") -> plot
    
    plot+
      theme_bw() +
      labs(
        x = "Mediana zarobków [PLN]",
        y = "Nazwa uczelni",
        title = "Uczelnie o najlepszych medianach zarobków absolwentów, wg podanych kryteriów:"
      ) +
      theme(text = element_text(size = 15))+
      scale_x_continuous(expand=c(0,0,0.1,0.1))

    
  }) %>% bindCache(input$lataPoDyplomie, input$rokDyplomu, input$rodzajStudiow, input$ileAbsolwentow, input$showMax)
  
  
  output$pointPlot <- renderPlot({
    
    monthsWWZ <- paste0("P_WWZ_MIES_", input$miesiac)
    monthsWWB <- paste0("P_WWB_MIES_", input$miesiac)
    
    kierunek %>% 
      select(monthsWWZ, monthsWWB, P_ROKDYP, P_DZIEDZINA, P_KIERUNEK_ID) %>% 
      rename(monthsWWZ = 1, monthsWWB = 2) %>%
      mutate(monthsWWZ = as.numeric(gsub(",", ".", monthsWWZ)),
             monthsWWB = as.numeric(gsub(",", ".", monthsWWB))) %>% 
      filter(P_ROKDYP==input$rokDyplomuWWZWWB, P_DZIEDZINA %in% input$dziedzina) %>% 
      ggplot(aes(x=monthsWWZ, y=monthsWWB, color=P_DZIEDZINA))+
      geom_point() -> plot
    
    
    plot +
      theme_bw() +
      labs(
        x = "Względny wskaźnik zarobków",
        y = "Względny wskaźnik bezrobocia",
        title = "Względne wskaźniki zarobków i bezrobocia, wg dziedziny ukończonego kierunku:"
      ) +
      guides(color=guide_legend(title="Wybrane dziedziny kierunków:"))+
      theme(text = element_text(size = 15))
    
  }) %>% bindCache(input$miesiac, input$dziedzina, input$rokDyplomuWWZWWB)
  
}


ui1 <- fluidPage(titlePanel("Najlepsze uczelnie pod względem mediany zarobków absolwentów"),
                 
                 sidebarLayout(
                   sidebarPanel(
                     
                     radioButtons(
                       "lataPoDyplomie",
                       "W którym roku po uzyskaniu dyplomu?",
                       choices = c("1",
                                   "2",
                                   "3",
                                   "4",
                                   "5")
                     ),
                     
                     selectInput(
                       inputId = "rodzajStudiow",
                       label = "Wybierz poziom studiów:",
                       choices = c(
                         "Pierwszego stopnia" = "1",
                         "Drugiego stopnia" = "2",
                         "Jednolite magisterskie" = "JM"
                       )
                     ),
                     
                     sliderInput(
                       inputId = "rokDyplomu",
                       label = "Wybierz rok uzyskania dyplomu:",
                       min = min(uczelnia$U_ROKDYP),
                       max = max(uczelnia$U_ROKDYP),
                       value = c(2016),
                       round = TRUE,
                       step = 1
                     ),
                     
                     sliderInput(
                       inputId = "ileAbsolwentow",
                       label = "Ilość absolwentów z danej uczelni w danym roku:",
                       min = min(uczelnia$U_N),
                       max = max(uczelnia$U_N),
                       value = c(500, max(uczelnia$U_N))
                     ),
                     
                     selectInput(
                       "showMax",
                       "Pokaż maksymalnie:",
                       choices = c("5", "10", "15", "20", "25"),
                       selected = "10"
                     ),
                     
                     width = 3
                   ),
                   mainPanel(
                     shinycssloaders::withSpinner(plotOutput("barPlot"),
                                                  type = getOption("spinner.type", default = 4),
                                                  color = getOption("spinner.color", default = "#0275D8"),
                                                  size = getOption("spinner.size", default = 1)
                     ),
                     width = 9)
                 ))



ui2 <- fluidPage(titlePanel("Względne wskaźniki zarobków i bezrobocia, wg dziedziny ukończonego kierunku"),
                 sidebarLayout(
                   sidebarPanel(
                     
                     sliderInput(
                       inputId = "rokDyplomuWWZWWB",
                       label = "Wybierz rok uzyskania dyplomu:",
                       min = min(kierunek$P_ROKDYP),
                       max = max(kierunek$P_ROKDYP),
                       value = c(2016),
                       round = TRUE,
                       step = 1
                     ),
                     
                     checkboxGroupInput(
                       "dziedzina",
                       "Absolwenci kierunków z dziedziny:",
                       choices = unique(kierunek$P_DZIEDZINA) -> t,
                       selected = c("Dziedzina nauk humanistycznych",
                                    "Dziedzina nauk inżynieryjno-technicznych")),
                     
                     numericInput(
                       "miesiac",
                       "Miesiąc po uzyskaniu dyplomu:",
                       min = 1,
                       max = 60,
                       value = 36
                     ),
                     
                     width = 3
                   ),
                   mainPanel(
                     shinycssloaders::withSpinner(plotOutput("pointPlot"),
                                                  type = getOption("spinner.type", default = 4),
                                                  color = getOption("spinner.color", default = "#0275D8"),
                                                  size = getOption("spinner.size", default = 1)
                     ),
                     width = 9)
                 ))



app_ui <- navbarPage(
  title = "Analiza ekonomicznych losów absolwentów uczelni w Polsce",
  tabPanel("Uczelnie o najlepszych zarobkach absolwentów", ui1,
           icon = icon("sack-dollar")),
  tabPanel("Porównanie dziedzin kierunków", ui2,
           icon = icon("scale-unbalanced-flip")
  ),

  theme = bslib::bs_theme(bootswatch = "darkly"),

  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  © 2022 Copyright:
                  <a class='text-dark' href='https://github.com/kseligga'>kseligga</a>
                </p>
                </footer>
                ")

)

shinyApp(app_ui, server)
