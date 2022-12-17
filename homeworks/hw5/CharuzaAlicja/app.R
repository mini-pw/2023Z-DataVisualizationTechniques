library(shiny)
library(dplyr)
library(plotly)
library(tidyr)
library(stringr)

institution <-
  read.csv("graduates-institution-data.csv",
           sep = ";",
           fileEncoding = "UTF-8")


ui1 <- fluidPage(
  titlePanel("Analiza WWB i WWZ pod względem dziedzin kierunków ukończonych przez absolwentów"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "wskaznik",
        "Wykres przedstawiający:",
        choices = c("Względny Wskaźnik Bezrobocia",
                    "Względny Wskaźnik Zarobków")
      ),
      checkboxGroupInput(
        "dziedzina",
        "Absolwenci kierunków z dziedziny:",
        choices = c(
          "nauk humanistycznych",
          "nauk inżynieryjno-technicznych",
          "nauk medycznych i nauk o zdrowiu",
          "nauk rolniczych",
          "nauk ścisłych i przyrodniczych",
          "nauk społecznych",
          "nauk teologicznych",
          "sztuki"
        ) -> t,
        selected = t
      ),
      numericInput(
        "rok",
        "Rok po uzyskaniu dyplomu:",
        min = 1,
        max = 5,
        value = 1
      ),
      helpText("Analizowane były dane od pierwszego do piątego roku po uzyskaniu dyplomu.")
    ),
    
    mainPanel(
      plotlyOutput("boxplot") %>% 
        shinycssloaders::withSpinner(type = 6,
                                     color = "#0000FF")
    )
  )
)

ui2 <- fluidPage(
  titlePanel("Analiza średnich zarobków absolwentów poszczególnych uczelni"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("uczelnia",
                  "Wybrana uczelnia:",
                  choices = unique(institution$U_NAZWA_UCZELNI)),
      radioButtons("doswiad",
                         "Absolwenci, którzy przed uzyskaniem dyplomu",
                         choices = c("nie mieli doświadczenia pracy",
                                     "mieli doświadczenie pracy"))
    ),
    mainPanel(
      plotlyOutput("colPlot") %>% 
        shinycssloaders::withSpinner(type = 6,
                                     color = "#0000FF")
    )
  )
  
  
)


server <- function(input, output) {
  output$boxplot <- renderPlotly({
    wskaznik <- ifelse(input$wskaznik == "Względny Wskaźnik Bezrobocia",
                    "U_WWB_GRUP",
                    "U_WWZ_GRUP")
    
    kodydziedzin <- data.frame(kod = c("D1",
                                       "D2",
                                       "D3",
                                       "D4",
                                       "D5",
                                       "D6",
                                       "D7",
                                       "D8"),
                               nazwa = c(
                                 "nauk humanistycznych",
                                 "nauk inżynieryjno-technicznych",
                                 "nauk medycznych i nauk o zdrowiu",
                                 "nauk rolniczych",
                                 "nauk ścisłych i przyrodniczych",
                                 "nauk społecznych",
                                 "nauk teologicznych",
                                 "sztuki"
                               ))
    
    institution %>% 
      select(contains(wskaznik)) %>% 
      select(contains(paste("P", input$rok, sep = ""))) %>% 
      select(contains(kodydziedzin[kodydziedzin$nazwa %in% input$dziedzina, 1])) -> df1
    
    
    pivot_longer(df1, cols = colnames(df1), names_to = "typ", values_to = "wskaznik") %>% 
      mutate(rok = substr(typ, 13, 13)) %>% 
      mutate(dziedzina = substr(typ, 15, 16)) %>% 
      drop_na() %>% 
      mutate(wskaznik = as.numeric(str_replace(wskaznik, ",", "."))) %>% 
      mutate(nazwa = kodydziedzin[as.numeric(substr(dziedzina, 2, 2)), 2]) -> df2
    
    
    ggplot(df2, aes(x = nazwa, y = wskaznik)) + 
      geom_boxplot() +
      theme_minimal() +
      labs(title = "",
           y = input$wskaznik,
           x = "") +
      coord_flip() -> p
    
    ggplotly(p)
    
  })
  
  output$colPlot <- renderPlotly({
    
    institution %>%
      select(contains(
        c(
          "U_E_ZAR_ETAT_NDOSW_P",
          "U_E_ZAR_ETAT_DOSW_P",
          "U_NAZWA_UCZELNI"
        )
      )) %>%
      filter(U_NAZWA_UCZELNI == input$uczelnia) %>%
      pivot_longer(cols = starts_with("U_E"),
                   names_to = "typ",
                   values_to = "srednia") %>%
      mutate(rok = substr(typ, nchar(typ), nchar(typ))) %>%
      mutate(typ = substr(typ, 14, 14)) %>% 
      filter(!is.na(srednia)) %>% 
      mutate(srednia = as.numeric(str_replace(srednia, ",", "."))) %>% 
      group_by(rok, typ) %>% 
      summarise(srednia = mean(srednia)) -> df
    
    if (input$doswiad == "mieli doświadczenie pracy") {
      df %>% 
        filter(typ == "D") -> df
    } else {
      df %>% 
        filter(typ == "N") -> df
    }
    
    ggplot(df, aes(x = factor(rok), y = srednia)) + 
      geom_col() +
      labs(title = "",
           x = "Rok po uzyskaniu dyplomu",
           y = "Średnia zarobków [PLN]") +
      ylim(0, 10000) +
      theme_minimal() -> p
    
    ggplotly(p)
    
  })
  
}

app_ui <- navbarPage(
  title = "Ekonomiczne losy absolwentów uczelni w Polsce",
  tabPanel("Dziedziny", ui1),
  tabPanel("Uczelnie", ui2),
  theme = bslib::bs_theme(bootswatch = "sandstone"),
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  Użyte 
                  <a class='text-dark' href='https://www.ela.nauka.gov.pl/pl/experts/source-data'>dane</a>
                </p>
                </footer>
                ")
)


shinyApp(ui = app_ui, server = server)
