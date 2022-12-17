library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(forcats)
library(tidyr)

data <- read.csv("graduates-major-data.csv", sep = ";")
data <- data %>% mutate(P_E_ZAR = as.numeric(gsub(",", ".", P_E_ZAR)))

ui <- fluidPage(

    titlePanel("Ekonomiczne losy absolwentów szkół wyższych"),
    
    mainPanel(
      shiny::markdown("Aplikacja zawiera wizualizacje dotyczące ekonomicznych losów absolwentów uczelni wyższych w Polsce,
      na podstawie danych ze strony https://www.ela.nauka.gov.pl. Poniżej należy wybrać interesujący poziom,
                      kierunek studiów oraz rok uzyskania dyplomu."),
      fluidRow(
        column(4,
               radioButtons("poziom",
                            "Poziom studiów",
                            choices = c("Studia pierwszego stopnia", 
                                        "Studia drugiego stopnia", "Jednolite studia magisterskie"))
        ),
        column(4,
               uiOutput("kierunek")
        ),
        column(4,
               uiOutput("dostepne_lata") 
        )),
      textOutput("podpis_wykresu_1"),
      
      shinycssloaders::withSpinner(
        plotlyOutput("wykres1"),
        type = getOption("spinner.type", default = 6),
        color = getOption("spinner.color", default = "#536CF8")
      ),
      uiOutput("dostepne_uczelnie"),
      textOutput("podpis_wykresu_2"),
      shinycssloaders::withSpinner(
        plotlyOutput("wykres2"),
        type = getOption("spinner.type", default = 6),
        color = getOption("spinner.color", default = "#536CF8")
      )
      )
)

server <- function(input, output) {

  output$kierunek <- renderUI({
    
    kierunki <- unique(data %>% 
      filter(P_POZIOM_TEKST_PL == input$poziom, !is.na(P_E_ZAR)) %>% 
      select(P_KIERUNEK_NAZWA) %>% 
      arrange(P_KIERUNEK_NAZWA) %>% 
      transmute(Kierunek = P_KIERUNEK_NAZWA))
    selectInput("kierunek_studiow", "Kierunek", kierunki) 
    
  })
  
  output$dostepne_lata <- renderUI({
    
    lata <- unique((data %>% 
              filter(P_KIERUNEK_NAZWA == input$kierunek_studiow, P_POZIOM_TEKST_PL == input$poziom) %>% 
              mutate(Rok = P_ROKDYP) %>% 
              select(Rok) %>% 
              arrange(as.numeric(Rok))))
    selectInput("rok", "Rok uzyskania dyplomu", lata)
  })
  
  output$dostepne_uczelnie <- renderUI({
    
    uczelnie <- unique(data %>% 
                         filter(P_KIERUNEK_NAZWA == input$kierunek_studiow, P_POZIOM_TEKST_PL == input$poziom,
                                P_ROKDYP == input$rok) %>% 
                         mutate(Uczelnia = P_NAZWA_UCZELNI) %>% 
                         select(Uczelnia) %>% 
                         arrange(Uczelnia))
    selectInput("uczelnia", "Uczelnia", uczelnie)
  })
  
  output$wykres1 <- renderPlotly({
    
    ggplotly(data %>% 
               filter(P_ROKDYP == input$rok, P_KIERUNEK_NAZWA == input$kierunek_studiow, 
                      P_POZIOM_TEKST_PL == input$poziom) %>% 
               group_by(P_NAZWA_UCZELNI) %>% 
               summarise(Wynagrodzenie = round(mean(P_E_ZAR, na.rm = TRUE), 2)) %>% 
               arrange(desc(Wynagrodzenie)) %>% 
               top_n(30) %>% 
               mutate(Uczelnia = fct_rev(factor(P_NAZWA_UCZELNI, levels = P_NAZWA_UCZELNI))) %>% 
               ggplot(aes(x = Uczelnia, y = Wynagrodzenie)) +
               geom_col(fill = "#536CF8") +
               coord_flip() +
               theme_minimal() +
               labs(
                 x = "Uczelnia",
                 y = "Wynagrodzenie w złotych"
               ))
  })
  
  output$podpis_wykresu_1 <- renderText({
    
    x <- paste("Poniższy wykres prezentuje zależność średniego miesięcznego wynagrodzenia absolwentów 
               bezpośrednio po uzyskaniu dyplomu od uczelni dla kierunku", input$kierunek_studiow,
               "w", input$rok, "roku.", "Prezentowane jest co najwyżej 30 uczelni z najwyższym wynagrodzeniem.") 
  })
  
  output$podpis_wykresu_2 <- renderText({
    
    x <- paste("Poniższy wykres prezentuje średnie miesięczne wynagrodzenie absolwentów w zależności od
               czasu po ukończeniu studiów dla kierunku ", input$kierunek_studiow, 
               ". Dypolom uzyskano w ", input$rok, " roku na uczelni ", input$uczelnia, ".", sep="") 
  })
  
  output$wykres2 <- renderPlotly({
    
    ggplotly(data %>%
               mutate(ZAR_1 = as.numeric(gsub(",", ".", P_E_ZAR_P1)),
                      ZAR_2 = as.numeric(gsub(",", ".", P_E_ZAR_P2)),
                      ZAR_3 = as.numeric(gsub(",", ".", P_E_ZAR_P3)),
                      ZAR_4 = as.numeric(gsub(",", ".", P_E_ZAR_P4)),
                      ZAR_5 = as.numeric(gsub(",", ".", P_E_ZAR_P5))) %>% 
               filter(P_ROKDYP == input$rok, 
                      P_POZIOM_TEKST_PL == input$poziom,
                      P_NAZWA_UCZELNI == input$uczelnia) %>% 
               group_by(P_KIERUNEK_NAZWA) %>% 
               summarise("Rok" = round(mean(ZAR_1, na.rm = TRUE),2),
                         "Dwa lata" = round(mean(ZAR_2, na.rm = TRUE),2),
                         "Trzy lata" = round(mean(ZAR_3, na.rm = TRUE),2),
                         "Cztery lata" = round(mean(ZAR_4, na.rm = TRUE),2),
                         "Pięć lat" = round(mean(ZAR_5, na.rm = TRUE),2)) %>% 
               pivot_longer(!P_KIERUNEK_NAZWA, names_to = "Okres", values_to = "Wynagrodzenie") %>% 
               filter(P_KIERUNEK_NAZWA == input$kierunek_studiow, !is.na(Wynagrodzenie)) %>%
               mutate(Okres = factor(Okres, levels = Okres)) %>% 
               ggplot(aes(x = Okres, y = Wynagrodzenie)) +
               geom_col(fill = "#536CF8") +
               theme_minimal() +
               labs(
                 y = "Wynagrodzenie w złotych",
                 x = "Czas od zakończenia studiów") +
               scale_y_continuous(expand = c(0,0)))
  })
  
}

shinyApp(ui = ui, server = server)
