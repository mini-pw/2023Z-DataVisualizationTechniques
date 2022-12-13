library(shiny)
library(dplyr)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(shinycssloaders)


#Pobieranie danych
studiaLata <- read.csv("StudentsData.csv", sep=";", encoding = "UTF-8") %>%
  filter(PL_POZIOM == 1) %>%
  select(PL_ROK_OD, PL_N_1L, PL_N_2L, PL_N_3L, PL_N_4L, PL_N_5L, PL_N_6L)

studiaDziedzina <- read.csv("StudentsData.csv", sep=";", encoding = "UTF-8") %>%
  filter(PL_POZIOM == 1) %>%
  select(PL_ROK_OD, PL_N_NDZIE1, PL_N_NDZIE2, PL_N_NDZIE3, PL_N_NDZIE4, PL_N_NDZIE5, PL_N_NDZIE6, PL_N_NDZIE7, PL_N_NDZIE8)

studiaRocznikowo <- studiaDziedzina %>%
  mutate(suma = PL_N_NDZIE1 + PL_N_NDZIE2 + PL_N_NDZIE3 + PL_N_NDZIE4 + PL_N_NDZIE5 + PL_N_NDZIE6 + PL_N_NDZIE7 + PL_N_NDZIE8) %>%
  select(PL_ROK_OD, suma)


#Serwer
serverA <- function(input, output, session) {
  
  #Plot1
  output$Plot1 <- renderPlot({
    ggplot(studiaLata, aes_string(x="PL_ROK_OD", y=input$jakie)) +
      geom_bar(stat="identity") +
      theme_bw() +
      labs(
        x = "Rok rozpoczęcia studiów",
        y = "Liczba studentów",
        title = "Liczba studentów w zależności od typu studiów w poszczególnych latach."
      )
  })
  
  #barPlot2
  output$barPlot2 <- renderPlot({
    ggplot(studiaDziedzina, aes_string(x="PL_ROK_OD", y=input$jaka)) +
      geom_bar(stat="identity") +
      theme_bw() +
      labs(
        x = "Rok rozpoczęcia studiów",
        y = "Liczba studentów",
        title = "Liczba studentów w zależności od typu studiów."
      )
  })
  
  #Plot3
  df <- reactive({
    studiaRocznikowo %>%
      filter(PL_ROK_OD %in% input$rocznik)
  })
  output$Plot3 <- renderPlot({
    req(input$rocznik)
    df() %>% 
      ggplot(aes_string(x="PL_ROK_OD", y="suma")) + 
      geom_bar(position="dodge", stat="identity")
  })
}

#UI
ui1A <- fluidPage(
  titlePanel("Liczba studentów w zależności od typu studiów w poszczególnych latach."),
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(
                       inputId = "jakie",
                       label = "Wybierz typ studiów: ",
                       choices = c(
                         "Studia roczne" = "PL_N_1L",
                         "Studia dwuletnie" = "PL_N_2L",
                         "Studia trzyletnie" = "PL_N_3L",
                         "Studia czteroletnie" = "PL_N_4L",
                         "Studia pięcioletnie" = "PL_N_5L",
                         "Studia sześcioletnie" = "PL_N_6L"
                       )
                     ),
                     width = 4
                   ),
                   mainPanel(
                     shinycssloaders::withSpinner(plotOutput("Plot1"),
                                                  type = getOption("spinner.type", default = 1),
                                                  size = getOption("spinner.size", default = 1)
                     ),
                     width = 8)
                 ))

ui2A <- fluidPage(
  titlePanel("Liczba studentów w zależności od typu studiów."),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "jaka",
        label = "Wybierz typ studiów: ",
        choices = c(
          "nauki humanistyczne" = "PL_N_NDZIE1",
          "nauki inżynieryjno-techniczne" = "PL_N_NDZIE2",
          "nauki medyczne" = "PL_N_NDZIE3",
          "nauki rolnicze" = "PL_N_NDZIE4",
          "nauki ścisłe" = "PL_N_NDZIE5",
          "nauki społeczne" = "PL_N_NDZIE6",
          "nauki teologiczne" = "PL_N_NDZIE7",
          "sztuka" = "PL_N_NDZIE8")
      ),
      width = 4
    ),
    mainPanel(
      shiny::markdown(
        "Na podanym wykresie widzimy liczność studentów danych nauk."
      ),
      plotOutput("barPlot2"),
      width = 9)
  ))

ui3A <- fluidPage(
  titlePanel("Rozkład liczności studentów w różnych latach."),
  selectInput("rocznik", "Wybierz lata: ", choices = studiaRocznikowo$PL_ROK_OD, selected=2015, multiple=TRUE),
  plotOutput("Plot3")
)

#app_ui
app_ui <- navbarPage(
  title = "Analiza danych: studenci",
  tabPanel("Wykres pierwszy", ui1A, icon = icon("list")),
  tabPanel("Wykres drugi", ui2A, icon = icon("bars")),
  tabPanel("Wykres trzeci", ui3A, icon = icon("calendar-days")),
  theme = bslib::bs_theme(bootswatch = "cosmo")
)

#finalna
shinyApp(app_ui, serverA)