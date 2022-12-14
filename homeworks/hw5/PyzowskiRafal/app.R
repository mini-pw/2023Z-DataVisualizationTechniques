library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(bslib)

# setwd("C:/Users/rafp2/OneDrive/Pulpit/RAFAŁ/studia/sem3/TWD/lab/hw5/PyzowskiRafalHW5")
# graduates_national <- read.csv("../graduates-national-data.csv", sep = ";")

# graduates_national <- read.csv("C:/Users/rafp2/OneDrive/Pulpit/RAFAŁ/studia/sem3/TWD/lab/hw5/graduates-national-data.csv", sep = ";")

graduates_national <- read.csv("graduates-national-data.csv", sep = ";")

graduates_national %>% 
  select(c(1, 2, 479, 480, 481, 488, 489, 490, 546:605)) %>% 
  filter(PL_POZIOM != "JM") -> df

df %>%
  select(-(3:8)) %>%
  pivot_longer(cols = starts_with("PL_WWZ")) %>%
  filter(value != "") %>%
  mutate(value = as.double(gsub(",", ".", value))) -> temp


ui1 <- fluidPage(
  titlePanel("Wskaźnik zarobków absolwentów uczelni wyższych w czasie"),
  
  fluidRow(
    column(6, 
           checkboxGroupInput(
             inputId = "rok_wskaznik",
             label = "Wybierz lata uzyskania dyplomu",
             inline = T,
             selected = c("2014", "2015", "2016", "2017", "2018", "2019", "2020"),
             choiceNames = c("2014", "2015", "2016", "2017", "2018", "2019", "2020"),
             choiceValues = c("2014", "2015", "2016", "2017", "2018", "2019", "2020")
           )),
    column(6,
           sliderInput(
             inputId = "miesiac",
             label = "Podaj zakres miesięcy od uzyskania dyplomu",
             value = c(1, 60),
             min = 1,
             max = 60,
             step = 1
           ))
  ),
  fluidRow(
    shinycssloaders::withSpinner(plotOutput("wskaznik1"),
                                 type = getOption("spinner.type", default = 6),
                                 color = getOption("spinner.color", default = "#0275D8"),
                                 size = getOption("spinner.size", default = 1))
  ),
  fluidRow(
    shinycssloaders::withSpinner(plotOutput("wskaznik2"),
                                 type = getOption("spinner.type", default = 6),
                                 color = getOption("spinner.color", default = "#0275D8"),
                                 size = getOption("spinner.size", default = 1))
  )
)


ui2 <- fluidPage(
  titlePanel("Porównanie absolwentów ze względu na miejsce zamieszkania"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "zakres",
        label = "Miejsce zamieszkania absolwentów",
        choiceNames = c("Największe miasta Polski", 
                        "Pozostałe miasta na prawach powiatu",
                        "Cała reszta"),
        choiceValues = c("1", "2", "3")
      ),
      radioButtons(
        inputId = "kategoria",
        label = "Kategoria",
        choiceNames = c("Średnie miesięczne wynagrodzenie po uzyskaniu dyplomu", 
                        "Średni czas od uzyskania dyplomu do podjęcia pierwszej pracy"),
        choiceValues = c("PL_E_ZAR_KMZ", "PL_CZAS_PRACA_ODLJEDN")
      )
    ),
    mainPanel(
      shinycssloaders::withSpinner(plotOutput("slupki"),
                                   type = getOption("spinner.type", default = 6),
                                   color = getOption("spinner.color", default = "#0275D8"),
                                   size = getOption("spinner.size", default = 1))
      
    )
  )
)


app_ui <- navbarPage(
  title = "Ekonomiczne losy absolwentów uczelni wyższych - Polska",
  tabPanel("Wskaźnik zarobków w czasie", ui1),
  tabPanel("Wpływ miejsca zamieszkania na karierę", ui2),
  theme = bs_theme(bootswatch = "lumen")
)


server <- function(input, output) {
  
  output$slupki <- renderPlot({
    
    col <- paste0(input$kategoria, input$zakres)
    
    colnames(df)[3:8] -> columns
    
    df %>% mutate(PL_E_ZAR_KMZ1 = as.double(gsub(",", ".", PL_E_ZAR_KMZ1))) %>% 
      mutate(PL_E_ZAR_KMZ2 = as.double(gsub(",", ".", PL_E_ZAR_KMZ2))) %>% 
      mutate(PL_E_ZAR_KMZ3 = as.double(gsub(",", ".", PL_E_ZAR_KMZ3))) %>% 
      mutate(PL_CZAS_PRACA_ODLJEDN1 = as.double(gsub(",", ".", PL_CZAS_PRACA_ODLJEDN1))) %>% 
      mutate(PL_CZAS_PRACA_ODLJEDN2 = as.double(gsub(",", ".", PL_CZAS_PRACA_ODLJEDN2))) %>% 
      mutate(PL_CZAS_PRACA_ODLJEDN3 = as.double(gsub(",", ".", PL_CZAS_PRACA_ODLJEDN3))) -> df2

    
    if(input$kategoria == "PL_E_ZAR_KMZ") {
      myTitle = "Średnie miesięczne wynagrodzenie po uzyskaniu dyplomu"
      y_axis = "Wynagrodzenie w PLN"
    } else {
      myTitle = "Średni czas od uzyskania dyplomu do podjęcia pierwszej pracy"
      y_axis = "Czas w miesiącach"
    }
    
    
    ggplot(df2) +
      aes_string(x = "PL_ROKDYP", 
          y = col,
          fill = "PL_POZIOM") +
      geom_bar(stat = "identity", position = "dodge") +
      scale_x_continuous(breaks = 2014:2020) +
      theme_minimal() +
      labs(x = "Rok otrzymania dyplomu",
           y = y_axis,
           title = myTitle) +
      guides(fill = guide_legend(title = "Rodzaj studiów")) +
      scale_fill_discrete(labels = c("Inżynierka/licencjat", "Magisterka"))-> myPlot
    
    if(input$kategoria == "PL_E_ZAR_KMZ") {
      myPlot + ylim(0, 6000)
    } else {
      myPlot + ylim(0, 15)
    }
    
  })
  
  output$wskaznik1 <- renderPlot({
    
    temp %>% 
      mutate(name = as.numeric(substr(temp$name, 13, 14))) %>% 
      filter(PL_ROKDYP %in% input$rok_wskaznik) %>% 
      filter(name >= input$miesiac[1] & name <= input$miesiac[2]) -> df_long
    
    ggplot(df_long %>% filter(PL_POZIOM == 1)) +
      aes(x = factor(name, level=unique(name)), 
          y = value,
          
          color = as.factor(PL_ROKDYP),
          group = PL_ROKDYP
      ) +
      ylim(0.65, 1.02) +
      theme_minimal() -> p2
    
    p2 + geom_point(stat = "identity",
                    size = 1.8) +
      geom_path(linewidth = 1.2) +
      guides(color = guide_legend(title = "Rok uzyskania dyplomu")) +
      theme(legend.position = c(0.85, 0.3)) +
      labs(x = "Miesiąc od uzyskania dyplomu",
           y = "Względny wskaźnik zarobków",
           title = "Absolwenci studiów 1-go stopnia")
  })
  
  output$wskaznik2 <- renderPlot({
    
    temp %>% 
      mutate(name = as.numeric(substr(temp$name, 13, 14))) %>% 
      filter(PL_ROKDYP %in% input$rok_wskaznik) %>% 
      filter(name >= input$miesiac[1] & name <= input$miesiac[2]) -> df_long
    
    ggplot(df_long %>% filter(PL_POZIOM == 2)) +
      aes(x = factor(name, level=unique(name)), 
          y = value,
          
          color = as.factor(PL_ROKDYP),
          group = PL_ROKDYP
      ) +
      ylim(0.65, 1.02) +
      theme_minimal() -> p2
    
    p2 + geom_point(stat = "identity",
                    size = 1.8) +
      geom_path(linewidth = 1.2) +
      guides(color = guide_legend(title = "Rok uzyskania dyplomu")) +
      theme(legend.position = c(0.85, 0.3)) +
      labs(x = "Miesiąc od uzyskania dyplomu",
           y = "Względny wskaźnik zarobków",
           title = "Absolwenci studiów 2-go stopnia")
    })
  
}

shinyApp(ui = app_ui, server = server)
