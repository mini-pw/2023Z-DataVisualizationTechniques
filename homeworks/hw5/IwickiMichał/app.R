library(dplyr)
library(ggplot2)
library(shiny)
library(bslib)
library(readr)
kierunek <- read_delim("graduates-major-data.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

kierunek[,c(1,3,5,383,384)]%>%
  mutate(P_POZIOM = case_when(
    P_POZIOM == 1 ~ "Pierwszego stopnia",
    P_POZIOM == 2 ~ "Drugiego stopnia",
    P_POZIOM == "JM" ~ "Jednolite magisterskie"))->wykres1

kierunek[,c(1,3,5,23)]%>%
  mutate(P_POZIOM = case_when(
    P_POZIOM == 1 ~ "Pierwszego stopnia",
    P_POZIOM == 2 ~ "Drugiego stopnia",
    P_POZIOM == "JM" ~ "Jednolite magisterskie"))->wykres2

ui <- fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    titlePanel("Analiza danych o absolwentach polskich uczelni"),
    
    fluidRow(
      column(6,
            textOutput("text1"),
             checkboxGroupInput(
               "stopien",
               "Wybierz stopnie kierunków, które chcesz wziąć pod uwage:",
               c("Pierwszego stopnia", "Drugiego stopnia", "Jednolite magisterskie"),
               selected = c("Pierwszego stopnia", "Drugiego stopnia", "Jednolite magisterskie") 
             ),
             sliderInput(
               "zakres",
               "Wybierz zakres liczby absolwentów kierunku",
               value = c(min(wykres1$P_N), max(wykres1$P_N)),
               min = min(wykres1$P_N),
               max = max(wykres1$P_N),
               step = 1,
               width = "600px"
             ),
            plotOutput("Plot1")),  
      
      column(6,
             textOutput("text2"),
             checkboxGroupInput(
               "stopien2",
               "Wybierz stopnie kierunków, które chcesz wziąć pod uwage:",
               c("Pierwszego stopnia", "Drugiego stopnia", "Jednolite magisterskie"),
               selected = c("Pierwszego stopnia", "Drugiego stopnia", "Jednolite magisterskie")
             ),
             selectInput(
               "rok2",
               "Wybierz rok dla którego chcesz sprawdzić dane",
               2014:2020),
             plotOutput("Plot2")
             )
      )
)
server <- function(input, output) {

    output$Plot1 <- renderPlot({
      wykres1%>%
        filter(P_POZIOM %in% input$stopien,
               P_N >= input$zakres[1],
               P_N<= input$zakres[2])%>%
        mutate(rok = P_ROKDYP,dosw = P_E_ZAR_ETAT_DOSW/100, ndosw = P_E_ZAR_ETAT_NDOSW/100)%>%
        na.omit()%>%
        group_by(rok)%>%
        summarise(dosw = weighted.mean(dosw, P_N), ndosw = weighted.mean(ndosw, P_N))%>%
        ggplot(mapping = aes(x = rok,y = ndosw, color = "Bez doświadczenia"))+
        geom_point()+
        geom_line()+
        geom_point(mapping = aes(x = rok,y = dosw, color = "Z doświadczeniem"))+
        geom_line(mapping = aes(x = rok,y = dosw, color = "Z doświadczeniem"))+
        theme_minimal()+
        theme(text = element_text(size = 15))+
        labs(title = "Różnica w zarobkach absolwentów z i bez doświadczenia w zawodzie" ,x = "Rok", y = "Średnie miesięczne wynagrodzenie [PLN]", color = "Doświadczenie w zawodzie 
przed otrzymaniem dyplomu") 
          })
    
    output$text1 <- renderText({
                "Wykres przedstawia średnie zarobki absolwentów w zależności od doświadczenia. Można wybrać które stopnie studiów bierzemy pod uwage oraz 
                przedział liczby absolwentów do którego będą należały kierunki biorące udział w średniej. "
    })
    output$text2 <-renderText({
      "Wykres przedstawia zależność między liczba absolwentów kierunku a czasem potrzebnym na znalezienie pracy.
      Każda kropka to oddzielny kierunek. Można wybrać rok dla którego dane zostaną przedstawione oraz stopnień kierunków.
      "
    })
    
    output$Plot2 <- renderPlot({
      wykres2%>%
        filter(P_ROKDYP == input$rok2, P_POZIOM %in% input$stopien2)%>%
        
        mutate(czas = as.numeric(gsub(",",".", P_CZAS_PRACA)))%>%
        group_by(P_POZIOM,P_N)%>%
        summarise(czas = mean(czas))%>%
        na.omit()%>%
        ggplot(mapping = aes(x = P_N, y = czas, color = P_POZIOM))+
        geom_point()+
        theme_minimal()+
        xlim(0,600)+
        theme(text = element_text(size = 15))+
        labs(title = "Zależność między liczbą absolwentów kierunku a czasem szukania pracy",
             x = "Liczba absolwentów danego kierunku", 
             y = "Średni czas szukania pracy [miesiące]",
             color = "Stopień studiów")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
