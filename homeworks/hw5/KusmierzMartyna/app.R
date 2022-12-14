library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(dplyr)
library(tidyr)
library(shinythemes)


dane <-
  read.csv("kierunekstudiow.csv",
           sep = ";",
           fileEncoding = "UTF-8") %>%
  drop_na(P_DZIEDZINA_NEW) %>%
  filter(P_DZIEDZINA_NEW != "") %>%
  filter(P_PROC_DROPOUT != "", P_PROC_PRZERWA != "", P_PROC_KOREKTA != "")

dziedziny <- unique(dane$P_DZIEDZINA_NEW)
zmienne <- c(dane$P_PROC_DROPOUT,
             dane$P_PROC_PRZERWA,
             dane$P_PROC_KOREKTA)


dane %>% 
  select(P_DZIEDZINA_NEW, P_POZIOM_TEKST_PL, P_N) %>%
  group_by(P_DZIEDZINA_NEW, P_POZIOM_TEKST_PL) %>%
 summarize(c = sum(as.numeric(P_N), na.rm = TRUE))
  

ui1 <- fluidPage(
  titlePanel("Liczba studentów"),
                 sidebarLayout(
                   sidebarPanel(
                      checkboxGroupInput("dziedziny",
                                          "Typy dziedzin kierunków",
                                          dziedziny,
                                          selected = c("Dziedzina nauk humanistycznych",
                                                        "Dziedzina nauk rolniczych",
                                                        "Dziedzina nauk ścisłych i przyrodniczych")),
                      sliderInput("zakres",
                            "Zakres lat",
                            value = c(min(dane$P_ROK_OD), max(dane$P_ROK_OD)),
                            min = min(dane$P_ROK_OD),
                            max = max(dane$P_ROK_OD),
                            sep = "",
                            step = 1),
                      checkboxGroupInput("kolejnosc",
                                         label = NULL,
                                         choiceNames = "Sortuj",
                                         choiceValues = "total ascending"),),
    mainPanel(shinycssloaders::withSpinner(
      plotlyOutput("barPlot1"),
      type = 1,
      size = 1)
      
   
)))


ui2 <- fluidPage(
  titlePanel("Rezygnacje, przerwy i korekty"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("dziedziny2",
                         "Typy dziedzin kierunków",
                         dziedziny,
                         selected = c("Dziedzina nauk humanistycznych",
                                      "Dziedzina nauk rolniczych",
                                      "Dziedzina nauk ścisłych i przyrodniczych")),
      sliderInput("zakres2",
                  "Zakres lat",
                  value = c(min(dane$P_ROK_OD), max(dane$P_ROK_OD)),
                  min = min(dane$P_ROK_OD),
                  max = max(dane$P_ROK_OD),
                  sep = "",
                  step = 1),
      checkboxGroupInput("kolejnosc2",
                         label = NULL,
                         choiceNames = "Sortuj",
                         choiceValues = "total ascending"),),
    mainPanel(shinycssloaders::withSpinner(
      plotlyOutput("barPlot"),
      type = 1,
      size = 1)
      
      
    )))


server <- function(input, output) {
  
  output$barPlot <- renderPlotly({
    plot_ly(dane %>% 
              filter(P_DZIEDZINA_NEW %in% input$dziedziny2,
    P_ROK_OD >= input$zakres2[1],
    P_ROK_OD <= input$zakres2[2]) %>%
    select(P_DZIEDZINA_NEW, P_PROC_DROPOUT, P_PROC_PRZERWA, P_PROC_KOREKTA) %>%
    group_by(P_DZIEDZINA_NEW) %>%
    summarize(Dropout = mean(as.numeric(P_PROC_DROPOUT), na.rm = TRUE),
              Przerwa = mean(as.numeric(P_PROC_PRZERWA), na.rm = TRUE),
              Korekta = mean(as.numeric(P_PROC_KOREKTA), na.rm = TRUE)), 
        y = ~P_DZIEDZINA_NEW,
        x = ~Dropout, type = "bar", name = "Rezygnacja", color = "#003f5c") %>%
      add_trace(x = ~Korekta, name = "Korekta", color = "#bc5090") %>%
      add_trace(x = ~Przerwa, name = "Przerwa", color = "#ffa600") %>%
    layout(title = "Procent studentów, którzy porzucili studia, zrobili przerwę (maks. 12 miesięcy) lub dokonali korekty kierunku",
           barmode = "stack",
           xaxis = list(title = '', range=c(0,55),ticksuffix = "%"),
           yaxis = list(title="", categoryorder = input$kolejnosc2)
    )
  })
  
  output$barPlot1 <- renderPlotly({
    plot_ly(dane %>% 
              filter(P_DZIEDZINA_NEW %in% input$dziedziny,
                     P_ROK_OD >= input$zakres[1],
                     P_ROK_OD <= input$zakres[2]) %>%
              select(P_DZIEDZINA_NEW, P_POZIOM_TEKST_PL, P_N) %>%
              group_by(P_DZIEDZINA_NEW, P_POZIOM_TEKST_PL) %>%
              summarize(c = sum(as.numeric(P_N), na.rm = TRUE)), 
            y = ~P_DZIEDZINA_NEW,
            x = ~c, type = "bar",
            color = ~P_POZIOM_TEKST_PL) %>%
      layout(title = "Liczba studentów danej dziedziny",
             barmode = "stack",
             xaxis = list(title = ''),
             yaxis = list(title="",categoryorder = input$kolejnosc)
      )
  })

    
}

app_ui <- navbarPage(
  "Analiza losów studentów polskich uczelni wyższych",
  tabPanel("Liczba studentów", ui1),
  tabPanel("Rezygnacje, przerwy i korekty", ui2),
  theme = bs_theme(bootswatch = "sandstone")
  
 
)
shinyApp(ui = app_ui, server = server)


