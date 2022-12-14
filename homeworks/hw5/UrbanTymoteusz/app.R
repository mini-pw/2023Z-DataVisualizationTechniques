library(ggplot2)
library(shiny)
library(dplyr)
library(shinycssloaders)

df_main <- read.csv('kierunek_dane.csv', sep = ';', encoding='UTF-8')
df <- df_main %>%
  select(P_ROKDYP, P_E_ZAR_ETAT_NDOSW_P1, P_DZIEDZINA, P_CZAS_ETAT_NDOSW, P_POZIOM) %>%
  mutate(P_CZAS = as.numeric(gsub(',','.',P_CZAS_ETAT_NDOSW))) %>%
  mutate(P_PLACA = as.numeric(gsub(',','.',P_E_ZAR_ETAT_NDOSW_P1))) %>%
  mutate(P_STOPIEN = case_when(P_POZIOM == '1' ~ 'Pierwszego stopnia',
                               P_POZIOM == '2' ~ 'Drugiego stopnia',
                               P_POZIOM == 'JM' ~ 'Jednolite magisterskie'))
df <- na.omit(df)
wszystkie_dziedziny <- unique(df$P_DZIEDZINA)[-c(7,9)]
wszystkie_stopnie <- unique(df$P_STOPIEN)
wszystkie_lata <- unique(df$P_ROKDYP)

ui1 <- fluidPage(
  
  titlePanel("Czas rozpoczęcia pracy"),
  "Średni czas w miesiącach od uzyskania dyplomu do podjęcia pierwszej pracy na
  umowę o pracę w zależności od dziedziny studiów. Absolwenci bez doświadczenia pracy przed uzyskaniem dyplomu.",

  sidebarLayout(
      sidebarPanel(
        sliderInput(
          inputId = 'rok2',
          label = 'Rok ukończenia studiów:',
          min = 2015,
          max = 2020,
          value = 2015,
          step = 1,
          sep = "",
          ticks = FALSE),
        checkboxGroupInput(
          inputId = "dziedziny",
          label = "Wybierz dziedziny:",
          choices = wszystkie_dziedziny,
          selected = wszystkie_dziedziny)),
      
      mainPanel(
        mainPanel(shinycssloaders::withSpinner(
          plotOutput("praca"))
      )
    )
))

ui2 <- fluidPage(
  
  titlePanel("Średnie zarobki"),
  "Średnie miesięczne wynagrodzenie z tytułu umów o pracę w pierwszym roku po
  uzyskaniu dyplomu w zależności od dziedziny studiów. Absolwenci bez 
                  doświadczenia pracy przed uzyskaniem dyplomu.",
  sidebarLayout(
    sidebarPanel(
      selectInput("poziom", "Wybierz poziom studiów: ", wszystkie_stopnie),
      checkboxGroupInput(
        inputId = "dziedziny2",
        label = "Wybierz dziedziny:",
        choices = wszystkie_dziedziny,
        selected = wszystkie_dziedziny)),
    
    mainPanel(
      mainPanel(shinycssloaders::withSpinner(
        plotOutput("ilosc"))
      )
    )
  )
)

server <- function(input, output) {

    output$praca <- renderPlot({
      df %>% 
        filter(P_STOPIEN == 'Pierwszego stopnia') %>%
        filter(P_ROKDYP == input$rok2) %>%
        filter(P_DZIEDZINA %in% input$dziedziny) %>%
        group_by(P_DZIEDZINA) %>%
        summarise(sredni_czas = mean(P_CZAS)) %>%
        ggplot(aes(y = P_DZIEDZINA, x = sredni_czas)) +
        geom_col(fill = 'steelblue') +
        labs(x = 'Liczba miesięcy', y = '') +
        theme_minimal()
    })
    
    output$ilosc <- renderPlot({
      df %>%
        filter(P_STOPIEN == input$poziom) %>%
        filter(P_DZIEDZINA %in% input$dziedziny2) %>%
        group_by(P_DZIEDZINA, P_ROKDYP) %>%
        summarise(placa = mean(P_PLACA)) %>%
        ggplot(aes(x=as.character(P_ROKDYP), placa, group=factor(P_DZIEDZINA))) +
        geom_line(aes(color = factor(P_DZIEDZINA)), size = 1) +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(x = 'Rok', y = 'Wynagrodzenie w PLN')
             })
}

app_ui <- navbarPage(
  title = 'Niedoświadczeni studenci i ich praca na etacie po zdobyciu dyplomu',
  tabPanel('Czas rozpoczęcia pracy', ui1, icon = icon(name="glyphicon glyphicon-time",lib="glyphicon")),
  tabPanel('Średnie zarobki', ui2, icon = icon('dollar-sign')),
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  Tymoteusz Urban, Techniki Wizualizacji Danych 2022
                </p>
                </footer>
                "),
  header = tags$head(tags$link(rel = "stylesheet",
                               href = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css"))
)

shinyApp(ui = app_ui, server = server)
