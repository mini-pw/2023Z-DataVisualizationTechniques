library(ggplot2)
library(dplyr)
library(shiny)
library(bslib)

HW5_data <- read.csv("D:\\Michalek\\szkola\\Studia\\Inzynieria i Analiza Danych\\Semestr 3\\TWD\\Prace domowe\\HW5-data.csv", sep = ';')
HW5_data_t <- select(HW5_data,
                     P_ROKDYP,
                     P_CZAS_PRACA,
                     P_DZIEDZINA,
                     P_E_ZAR_PODCZAS_STUDIOW_DOSW
                     ) %>%
  mutate(P_DZIEDZINA = gsub("Dziedzina nauk społecznych", "Społeczne", P_DZIEDZINA)) %>%
  mutate(P_DZIEDZINA = gsub("Dziedzina nauk humanistycznych", "Humanistyczne", P_DZIEDZINA)) %>%
  mutate(P_DZIEDZINA = gsub("Dziedzina nauk inżynieryjno-technicznych", "Inżynieryjno-techniczne", P_DZIEDZINA)) %>%
  mutate(P_DZIEDZINA = gsub("Dziedzina sztuki", "Artystyczne", P_DZIEDZINA)) %>%
  mutate(P_DZIEDZINA = gsub("Dziedzina nauk teologicznych", "Teologiczne", P_DZIEDZINA)) %>%
  mutate(P_DZIEDZINA = gsub("Dziedzina nauk ścisłych i przyrodniczych", "Ścisłe i przyrodnicze", P_DZIEDZINA)) %>%
  mutate(P_DZIEDZINA = gsub("Dziedzina nauk rolniczych", "Rolnicze", P_DZIEDZINA)) %>%
  mutate(P_DZIEDZINA = gsub("Dziedzina nauk medycznych i nauk o zdrowiu", "Medyczne", P_DZIEDZINA)) %>%
  filter(P_DZIEDZINA!="") %>% na.omit

ui1 <- fluidPage(
  titlePanel("TWD HW5 - Czas od skończenia studiów do znalezienia pierwszej pracy"),
  sliderInput("years_range",
              "Zakres lat",
              min = min(HW5_data_t$P_ROKDYP),
              max = max(HW5_data_t$P_ROKDYP),
              value = c(2014,2020),
              step = 1
              ),
  plotOutput("time_to_work")
)


ui2 <- fluidPage(
  titlePanel("TWD HW5 - Średnie zarobki studentów pracujących na studiach"),
  checkboxGroupInput("faculties",
                     "Wybierz interesujące dziedziny studiów",
                     unique(HW5_data_t$P_DZIEDZINA),
                     unique(HW5_data_t$P_DZIEDZINA)
  ),
  plotOutput("earnings")
)

server <- function(input, output) {
  output$time_to_work <- renderPlot({
    HW5_data_t %>% 
      filter(P_ROKDYP>=input$years_range[1],
             P_ROKDYP<=input$years_range[2]) %>% 
      mutate(P_CZAS_PRACA = as.numeric(gsub(",", ".", P_CZAS_PRACA))) %>%
      ggplot(aes(x=P_DZIEDZINA,y=P_CZAS_PRACA))+
      geom_boxplot(position = position_dodge(0.5))+
      labs(
        x = "Dziedzina kierunku studiów",
        y = "Średni czas od uzyskania dyplomu do podjęcia pierwszej pracy",
        title = "Średni czas od uzyskania dyplomu do podjęcia pracy w zależności od dziedziny studiów"
      )+
      theme_bw()+
      scale_y_continuous(expand = c(0,0))
  })
  output$earnings <- renderPlot({
    HW5_data_t %>% filter(
      P_DZIEDZINA %in% input$faculties
    ) %>% 
      mutate(P_E_ZAR_PODCZAS_STUDIOW_DOSW = as.numeric(gsub(",", ".", P_E_ZAR_PODCZAS_STUDIOW_DOSW))) %>%
      ggplot(aes(x=P_DZIEDZINA,y=P_E_ZAR_PODCZAS_STUDIOW_DOSW))+
      geom_boxplot()+
      labs(
        x = "Dziedzina kierunku studiów",
        y = "Średnie miesięczne wynagrodzenie w trakcie studiów\nabsolwentów mających doświadczenie przed rozpoczęciem studiów",
        title = "Zarobki studentów w czasie studiów w zależności od dziedziny studiów\n(dla studentów z doświadczeniem przed podjęciem studiów)"
      )+
      theme_bw()+
      scale_y_continuous(expand = c(0,0))
  })
}

app_ui <- navbarPage(
  title = "Ekonomiczne losy absolwentów uczelni w Polsce",
  tabPanel("Na studiach", ui2),
  tabPanel("Po studiach", ui1),
  theme = bs_theme(bootswatch = "darkly"),
  footer = HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  <a class='text-dark' href='https://www.ela.nauka.gov.pl/pl/experts/source-data'>Użyte dane</a>
                </p>
                </footer>
                ")
)


shinyApp(ui = app_ui, server = server)