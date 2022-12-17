library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(dplyr)
library(readr)

national_data <- read.csv("graduates-national-data.csv", sep = ";")
institution_data <- read.csv("graduates-institution-data.csv", sep = ";")

ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),

  titlePanel("Analiza danych o losach absolwetów polskich uczelni"),
  fluidRow(
    column(
      6,
      sliderInput(
        inputId = "rok",
        label = "Wybierz lata:",
        min = min(national_data$PL_ROKDYP),
        max = max(national_data$PL_ROKDYP),
        value = c(2014, 2020),
        sep = "",
        step = 1
      ),
      selectInput(
        inputId = "uczelnie",
        label = "Wybierz uczelnie:",
        choices = unique(institution_data$U_NAZWA_UCZELNI),
        selected = "Politechnika Warszawska",
        multiple = TRUE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      selectInput(
        inputId = "stopien",
        label = "Wybierz stopień:",
        choices = c(1, 2),
        selected = 1,
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      )
    ),
    column(6, ),
  ),
  fluidRow(
    column(
      6,
      plotOutput("barPlot")
    ),
    column(
      6,
      plotOutput("barPlot2")
    )
  ),
  fluidRow(
    column(
      6,
    ),
    column(
      6,
    ),
  )
)




server <- function(input, output) {
  output$barPlot <- renderPlot({
    if (input$stopien != 1) {
      stopien <- c("1")
    } else {
      stopien <- c("2", "JM")
    }

    institution_data %>%
      mutate(U_CZY_PRACA_NSTUD_P1 = parse_number(U_CZY_PRACA_NSTUD_P1, locale = locale(decimal_mark = ",", grouping_mark = "."))) %>%
      filter(U_NAZWA_UCZELNI %in% input$uczelnie) %>%
      filter(U_ROKDYP >= input$rok[1], U_ROKDYP <= input$rok[2]) %>%
      mutate(U_ROKDYP = as.factor(U_ROKDYP)) %>%
      filter(U_POZIOM %in% stopien) %>%
      select(U_ROKDYP, U_CZY_PRACA_NSTUD_P1, U_NAZWA_UCZELNI) %>%
      ggplot(aes(x = U_ROKDYP, y = U_CZY_PRACA_NSTUD_P1, fill = U_NAZWA_UCZELNI)) +
      geom_bar(position = "dodge", stat = "identity") +
      labs(title = "Procent aboselwentów, którzy mieli doświadczenie zawodowe rok rok po ukończeniu studiów") +
      xlab("Rok dyplomacji") +
      ylab("% aboselwentów") +
      guides(fill = guide_legend(title = "Nazwa uczelni")) -> p1
    p1
  })

  output$barPlot2 <- renderPlot({
    if (input$stopien != 1) {
      stopien <- c("1")
    } else {
      stopien <- c("2", "JM")
    }

    institution_data %>%
      mutate(U_E_ZAR_NSTUD_P1 = parse_number(U_E_ZAR_NSTUD_P1, locale = locale(decimal_mark = ",", grouping_mark = "."))) %>%
      filter(U_NAZWA_UCZELNI %in% input$uczelnie) %>%
      filter(U_ROKDYP >= input$rok[1], U_ROKDYP <= input$rok[2]) %>%
      mutate(U_ROKDYP = as.factor(U_ROKDYP)) %>%
      filter(U_POZIOM %in% stopien) %>%
      select(U_ROKDYP, U_E_ZAR_NSTUD_P1, U_NAZWA_UCZELNI) %>%
      ggplot(aes(x = U_ROKDYP, y = U_E_ZAR_NSTUD_P1, fill = U_NAZWA_UCZELNI)) +
      geom_bar(position = "dodge", stat = "identity") +
      labs(title = "Średnie miesięczne wynagrodzenie absolwentów rok po ukończeniu studiów") +
      xlab("Rok dyplomacji") +
      ylab("zł") +
      guides(fill = guide_legend(title = "Nazwa uczelni")) -> p1
    p1
  })
}


shinyApp(ui = ui, server = server)
