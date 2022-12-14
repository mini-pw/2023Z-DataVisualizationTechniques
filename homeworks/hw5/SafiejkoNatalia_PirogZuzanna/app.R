

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggthemes)

df_major <- read.csv("graduates.csv", sep = ";", encoding = "UTF-8")

institution <-
  read.csv("graduates.csv",
           sep = ";",
           fileEncoding = "UTF-8")

zamien_na_liczbe <- function(x){
  as.numeric(gsub(",", ".", x))
}


uczelnie <- c("PW", "AGHSSK", "PW1", "SGHW")

df_major <- df_major %>% 
  select(P_CZAS_PRACA_DOSW, P_CZAS_PRACA_NDOSW,  P_CZAS_PRACA, P_NAZWA_UCZELNI, P_UCZELNIA_SKROT, P_ROKDYP) %>% 
  mutate(P_NAZWA_UCZELNI=(sub("Akademia Górniczo-Hutnicza im. Stanisława Staszica w Krakowie", "AGH w Krakowie", P_NAZWA_UCZELNI))) %>% 
  filter(P_UCZELNIA_SKROT %in% uczelnie )

df_major[c("P_CZAS_PRACA","P_CZAS_PRACA_NDOSW","P_CZAS_PRACA_DOSW")]<-sapply(df_major[c("P_CZAS_PRACA","P_CZAS_PRACA_NDOSW","P_CZAS_PRACA_DOSW")],zamien_na_liczbe)


df_major <- df_major %>%
  filter(!is.na(P_CZAS_PRACA), !is.na(P_CZAS_PRACA_NDOSW), !is.na(P_CZAS_PRACA_NDOSW)) %>% 
  rename(razem = P_CZAS_PRACA,
         brak_dos = P_CZAS_PRACA_NDOSW,
         z_dosw = P_CZAS_PRACA_DOSW)


ui <- fluidPage(
  
  
  titlePanel("Ile potrzeba czasu na znalezeienie pracy przez abslolwentów poniższych uczelni?"),
  
  br(),
  br(),
  
  
  sidebarLayout(
    sidebarPanel(
      textOutput("text"),
      br(),
      selectInput( inputId = "experience",
                   "Wybierz doświadczenie studenta ",
                   choices = c("razem","brak doświadczenia", "z doświadczeniem"),
                   selected = "razem",
                   multiple = FALSE),
      
      checkboxGroupInput("uczelnia",
                         "Wybierz uczelnie",
                         unique(df_major$P_NAZWA_UCZELNI),
                         selected=unique(df_major$P_NAZWA_UCZELNI),
      )
      
    ),
    mainPanel(
      shinycssloaders::withSpinner(
        plotOutput("violinPlot",
                   height = "600px"), 
        type = 1, 
        color = "#00ff00", 
        size = 1 
      )
    )
    
    
  )
)

ui1 <- fluidPage(
  titlePanel("Analiza zarobków studentów wybranych kierunków na wybranych uczelniach"),
  textOutput("text1"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "kierunki",
        "Wykres przedstawiający kierunki:",
        choices = c("Informatyka",
                    "Budownictwo",
                    "Energetyka",
                    "Architektura",
                    "Zarządzanie"
        ),
        selected=c("Informatyka",
                   "Budownictwo",
                   "Energetyka",
                   "Architektura",
                   "Zarządzanie"
        )
      ),
      checkboxGroupInput(
        "uczelnia1",
        "Absolwenci kierunków z uczelni:",
        choices = c(
          "Politechnika Lubelska",
          "Politechnika Warszawska",
          "Politechnika Gdańska",
          "AGH w Krakowie",
          "Politechnika Poznańska"
        ),
        selected= c(
          "Politechnika Lubelska",
          "Politechnika Warszawska",
          "Politechnika Gdańska",
          "AGH w Krakowie",
          "Politechnika Poznańska"
        )
      )
    ),
    
    
    mainPanel(
      plotOutput("barPlot") %>% 
        shinycssloaders::withSpinner(type = 6,
                                     color = "#0000FF")
      
    )
  ))



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$text <- renderText({
    paste("Wykres przedstawia średni czas po jakim absolwenci najlepszych uczelni technicznnych średnio znajdują pracę.
               Proszę wybrać czy dane mają przedstawiać absolwentów posiadających wcześniejsze doświadczenie zawodowowe, czy mają to być
               osoby podejmujące się pracy po raz pierwszy po ukończeniu studiów.")
  })
  
  output$barPlot <- renderPlot({
    df <- institution %>%
      mutate(P_NAZWA_UCZELNI=(sub("Akademia Górniczo-Hutnicza im. Stanisława Staszica w Krakowie", "AGH w Krakowie", P_NAZWA_UCZELNI))) %>% 
      filter(P_KIERUNEK_NAZWA %in% input$kierunki) %>% 
      filter(P_NAZWA_UCZELNI %in% input$uczelnia1) %>% 
      mutate( P_E_ZAR= as.integer(sub(",", ".", P_E_ZAR))) %>% 
      select(P_KIERUNEK_NAZWA, P_NAZWA_UCZELNI, P_E_ZAR) %>% 
      group_by(P_NAZWA_UCZELNI, P_KIERUNEK_NAZWA) %>% 
      summarise(P_E_ZAR=mean(P_E_ZAR))
    
    
    
    ggplot(df,aes(y = P_E_ZAR,x = P_KIERUNEK_NAZWA, fill= P_NAZWA_UCZELNI)) +
      coord_flip()+
      ylab("Średnie wynagrodzenie")+
      xlab("Kierunek")+
      labs("Zależność średniego wynagrodzenia od kierunku na danej uczelni", fill="Uczelnia")+
      theme_gdocs()+
      scale_y_continuous(expand = c(0,0))+ 
      
      geom_col(position = position_dodge2(width = 0.9, preserve = "single")) 
    
  })
  
  output$text1 <- renderText({
    paste("Wykres przedstawia średni czas po jakim absolwenci najlepszych uczelni technicznnych średnio znajdują pracę.
               Proszę wybrać czy dane mają przedstawiać absolwentów posiadających wcześniejsze doświadczenie zawodowowe, czy mają to być
               osoby podejmujące się pracy po raz pierwszy po ukończeniu studiów.")
  })
  
  
  jaki_czas_D <- reactive({
    switch(input$experience,
           "razem" = "razem",
           "brak doświadczenia" = "brak_dos",
           "z doświadczeniem" = "z_dosw",)
  })
  
  observe({
    
    if(length(input$uczelnia) < 1){
      updateCheckboxGroupInput(session,"uczelnia", selected= "Politechnika Warszawska")
    }
  })
  
  output$violinPlot <- renderPlot({
    
    
    
    df_major <- df_major %>% 
      filter(P_NAZWA_UCZELNI %in% input$uczelnia) %>% 
      group_by(P_NAZWA_UCZELNI)
    
    ggplot(df_major, aes(na.rm = TRUE))+
      geom_boxplot(aes_string(x =jaki_czas_D(), y= "P_NAZWA_UCZELNI", fill = "P_NAZWA_UCZELNI"))+
      scale_x_continuous(expand = c(0,0), limits = c(0, 10))+
      scale_y_discrete(expand = c(0,0))+
      theme_gdocs()+
      labs(title = "Średni czas poszukiwań pracy po zakończeniu studiów",
           x = "Liczba miesięcy",
           y = "Wybrane uczelnie")+
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
            axis.title = element_text(face = "bold", size = 16),
            axis.text = element_text( size = 14))
    
    
    
    
    
  })
  
  
}

app_ui <- navbarPage(
  title = "Analiza danych absolwentów Polskich uczelni",
  tabPanel("Ile średnio zarabiają absolwenci?", ui1,
           icon = icon("money-bill")),
  tabPanel("Jak szybko absolwenci znajdują pracę?", ui,
           
           icon = icon("briefcase")
           
  ),
  
  theme = bslib::bs_theme(bootswatch = "lux"),
  
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  Natalia Safiejko i Zuzanna Piróg
                 
                </p>
                </hr>
                </footer>
                
                "),
  header = tags$head(tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css")))

# Run the application 
shinyApp(ui = app_ui, server = server)

