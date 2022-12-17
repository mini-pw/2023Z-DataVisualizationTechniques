
library(rsconnect)
library(packrat)
library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(dplyr)
library(ggrepel)
library(shinycssloaders)

dane <-
  read.csv("students-major-data.csv",
           sep = ";",
           fileEncoding = "UTF-8")
dane2 <-
  read.csv("graduates-major-data.csv",
           sep = ";",
           fileEncoding = "UTF-8")
# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bs_theme(bootswatch = "superhero"),

    # Application title
    titlePanel("Informacje odnośnie studentów"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
      column(12,
    
    sidebarLayout(
        sidebarPanel(
          sliderInput(
            inputId = "rok",
            label = "Lata:",
            min = min(dane$P_ROK_OD),
            max = max(dane$P_ROK_OD),
            step = 1,
            value = c(2015, 2019)
          )
          ,checkboxGroupInput(inputId = "dziedziny", "Dziedziny",
                              c("Dziedzina nauk społecznych",
                                "Dziedzina nauk humanistycznych",
                                "Dziedzina nauk medycznych i nauk o zdrowiu",
                                "Dziedzina sztuki",
                                "Dziedzina nauk ścisłych i przyrodniczych",
                                "Dziedzina nauk inżynieryjno-technicznych",
                                "Dziedzina nauk rolniczych",
                                "Dziedzina nauk teologicznych",
                                "inne" = ""
                                ),
                              selected = c("Dziedzina nauk społecznych",
                                           "Dziedzina nauk humanistycznych",
                                           "Dziedzina nauk medycznych i nauk o zdrowiu",
                                           "Dziedzina sztuki",
                                           "Dziedzina nauk ścisłych i przyrodniczych",
                                           "Dziedzina nauk inżynieryjno-technicznych",
                                           "Dziedzina nauk rolniczych",
                                           "Dziedzina nauk teologicznych",
                                           "inne" = "" ))
            
            ),

        # Show a plot of the generated distribution
        mainPanel(
          shinycssloaders::withSpinner(plotOutput("barPlot"),
                                       type = getOption("spinner.type", default = 6),
                                       color = getOption("spinner.color", default = "#DF623D"),
                                       size = getOption("spinner.size", default = 1)
          )
        )
    ))),
    
    fluidRow(
      
      column(12,
             sliderInput(
               inputId = "rok3",
               label = "Lata:",
               min = min(dane$P_ROK_OD),
               max = max(dane$P_ROK_OD),
               step = 1,
               value = c(2015, 2019),
               
             )
      ),
      column(12,
             
             
             plotOutput("Plot3")),),)

ui2 <- fluidPage(
  theme = bs_theme(bootswatch = "superhero"),
  titlePanel("Informacje odnośnie absolwentów"),
  sidebarPanel(
    sliderInput(
      inputId = "rok4",
      label = "Lata:",
      min = min(dane$P_ROK_OD),
      max = max(dane$P_ROK_OD),
      step = 1,
      value = c(2015, 2019),
      
    )
    
    # rok
  ),
  selectInput(inputId = "dziedziny", "Dziedziny",
                      c("Dziedzina nauk społecznych",
                        "Dziedzina nauk humanistycznych",
                        "Dziedzina nauk medycznych i nauk o zdrowiu",
                        "Dziedzina sztuki",
                        "Dziedzina nauk ścisłych i przyrodniczych",
                        "Dziedzina nauk inżynieryjno-technicznych",
                        "Dziedzina nauk rolniczych",
                        "Dziedzina nauk teologicznych",
                        "inne" = ""
                      ),
                      selected = c("Dziedzina nauk społecznych",
                                   "Dziedzina nauk humanistycznych",
                                   "Dziedzina nauk medycznych i nauk o zdrowiu",
                                   "Dziedzina sztuki",
                                   "Dziedzina nauk ścisłych i przyrodniczych",
                                   "Dziedzina nauk inżynieryjno-technicznych",
                                   "Dziedzina nauk rolniczych",
                                   "Dziedzina nauk teologicznych",
                                   "inne" = "" )),
  mainPanel(
    shinycssloaders::withSpinner(plotOutput("Plot4"),
                                 type = getOption("spinner.type", default = 6),
                                 color = getOption("spinner.color", default = "#DF623D"),
                                 size = getOption("spinner.size", default = 1)
                            
    ),
    dataTableOutput("table")
    
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$table <- renderDataTable({
    df <- dane2 %>% filter(P_ROKDYP >= input$rok4[1], P_ROKDYP <= input$rok4[2])
    df <- df %>% filter(P_DZIEDZINA %in% input$dziedziny)
    df <- aggregate(df$P_N, by=list(P_ROKDYP=df$P_ROKDYP), FUN=sum)
    df <- df %>% rename("Ilość absolwentów"="x") %>% rename("Rok ukończenia"= "P_ROKDYP")
   })
  # 
  output$Plot4 <- renderPlot({
    df <- dane2 %>% filter(P_ROKDYP >= input$rok4[1], P_ROKDYP <= input$rok4[2])
    df <- df %>% filter(P_DZIEDZINA %in% input$dziedziny)
    df <- aggregate(df$P_N, by=list(P_ROKDYP=df$P_ROKDYP), FUN=sum)
    ggplot(data=df, aes(x=P_ROKDYP, y=x), ) +
      geom_bar(stat="identity", width=0.5, fill = "#903e50") + theme_minimal() + labs(
        x = "Rok",
        y = "Liczba osób",
        title = "Liczba osób, który skończyli studia w danym roku",
      ) + theme(plot.title = element_text(size = 16))
  })
    

  
  
    output$barPlot <- renderPlot({
      d <- dane %>% filter(P_DZIEDZINA_NEW %in% input$dziedziny)
      d <- d %>% filter(P_ROK_OD >= input$rok[1], P_ROK_OD <= input$rok[2])
      d <- aggregate(d$P_N, by=list(P_ROK_OD=d$P_ROK_OD), FUN=sum)
      ggplot(data=d, aes(x=P_ROK_OD, y=x), ) +
        geom_bar(stat="identity", width=0.5, fill = "#903e50") + theme_minimal() + labs(
          x = "Rok",
          y = "Liczba studentów",
          title = "Liczba studentów zaczynających studia w danym roku",
          subtitle = "Z możliwością dopasowania roku oraz dziedzin branych pod uwagę"
        ) + theme(plot.title = element_text(size = 16))
    })
    
 
    output$Plot3 <- renderPlot({
      
      d <- dane %>% filter(P_ROK_OD >= input$rok3[1], P_ROK_OD <= input$rok3[2])
      d <- aggregate(d$P_N, by=list(a=d$P_NSEMESTR), FUN=sum)
      ggplot(data=d, aes(x=a, y=x) ) +
        geom_bar(stat="identity", width=0.5, fill = "#903e50") + theme_minimal() + labs(
          x = "Ilość lat",
          y = "Liczba studentów",
          title = "Liczba studentów studiujących ilość semestrów",
          subtitle = "Z możliwością dopasowania roku"
        ) + theme(plot.title = element_text(size = 16))
      
      
    })
}

app_ui <- navbarPage(
  title = "Ekonomiczne losy absolwentów uczelni w Polsce",
  tabPanel("Studenci", ui,
           icon = icon("fa-sharp fa-solid fa-book-open-reader")),
  tabPanel("Absolwenci", ui2,
           icon = icon("fa-sharp fa-solid fa-graduation-cap")),
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  © Źródło danych:
                  <a class='text' href='https://www.ela.nauka.gov.pl/pl/experts/source-data'>www.ela.nauka.gov.pl</a>
                </p>
                </footer>
                ")
  )


# Run the application 
shinyApp(ui = app_ui, server = server)
