library(shiny)
library(bslib)
library(stringr)
library(ggplot2)
library(forcats)
library(shinythemes)
library(shinycssloaders)
library(plotly)

typy <- c("Medyczne", "Muzyczne", "Sportowe", "Wojskowe", "Ekonomiczne")
data <- read.csv('graduates-major-data.csv', sep=';', fileEncoding = "UTF-8")
data %>% 
  select(P_KIERUNEK_NAZWA, P_NAZWA_UCZELNI, P_CZAS_PRACA_NDOSW,P_FORMA,P_E_ZAR_NDOSW) %>% 
  filter(P_KIERUNEK_NAZWA %in%c("Informatyka", "Pedagogika",
                                "Elektrotechnika","Administracja",
                                "Mechatronika","Teologia",
                                "Inżynieria i Analiza Danych")) %>% 
  filter(P_CZAS_PRACA_NDOSW!="") %>% 
  mutate(P_CZAS_PRACA_NDOSW = as.numeric(gsub(",", ".",P_CZAS_PRACA_NDOSW ))) %>% 
  mutate(P_E_ZAR_NDOSW = as.numeric(gsub(",", ".",P_E_ZAR_NDOSW )))->data
data<-na.omit(data)
forms <- unique(data$P_FORMA)
dft <- read.csv("international_rankings-major-data.csv", sep=";", fileEncoding = "UTF-8")
df <- read.csv("graduates-institution-data.csv", sep=";", fileEncoding = "UTF-8")
df %>% 
  mutate(U_PROC_MIES_BEZR = as.numeric(gsub(",", ".", gsub("\\.", "", U_PROC_MIES_BEZR)))) %>%
  group_by(U_UCZELNIA_ID) %>%
  summarise(srednia = mean(U_PROC_MIES_BEZR)) %>%
  rename(Id = U_UCZELNIA_ID)-> df
library(dplyr)

generujTyp <- function(string){
  
  res <- switch(string,
                "Medyczne" = dft %>%
                  group_by(P_UCZELNIA, P_NAZWA_UCZELNI)%>%
                  summarise() %>%
                  filter(grepl("Medy", P_NAZWA_UCZELNI) || grepl("Zdr", P_NAZWA_UCZELNI))%>% rename(Id = P_UCZELNIA) -> k,
                "Muzyczne" = dft %>%
                  group_by(P_UCZELNIA, P_NAZWA_UCZELNI)%>%
                  summarise() %>%
                  filter(grepl("Muz", P_NAZWA_UCZELNI))%>% rename(Id = P_UCZELNIA) -> k,
                "Sportowe" = dft %>%
                  group_by(P_UCZELNIA, P_NAZWA_UCZELNI)%>%
                  summarise() %>%
                  filter(grepl("Sport", P_NAZWA_UCZELNI) || grepl("Fiz", P_NAZWA_UCZELNI))%>% rename(Id = P_UCZELNIA) -> k,
                "Wojskowe" = dft %>%
                  group_by(P_UCZELNIA, P_NAZWA_UCZELNI)%>%
                  summarise() %>%
                  filter(grepl("Woje", P_NAZWA_UCZELNI)|| grepl("Wojsk", P_NAZWA_UCZELNI) || grepl("ficer", P_NAZWA_UCZELNI))%>% rename(Id = P_UCZELNIA) -> k,
                "Ekonomiczne" = dft %>%
                  group_by(P_UCZELNIA, P_NAZWA_UCZELNI)%>%
                  summarise() %>%
                  filter(grepl("Ekon", P_NAZWA_UCZELNI) || grepl("Ban", P_NAZWA_UCZELNI)|| grepl("Biz", P_NAZWA_UCZELNI)
                         || grepl("Hand", P_NAZWA_UCZELNI) || grepl("Przeds", P_NAZWA_UCZELNI)) %>% rename(Id = P_UCZELNIA) -> k)
  k
}

ui1 <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  titlePanel("Ryzyko bezrobocia po uzyskaniu dyplomu dla uczelni o wybranym ukierunkowaniu"),
  sidebarPanel(sliderInput(inputId = "Liczba",
                           label = "Liczba przedstawionych wyników",
                           min = 1,
                           max = 10,
                           value = 5),
               selectInput("Typ",
                           "Dla jakiego typu uczelni przedstawić dane?",
                           typy),
               selectInput("Rosnaco",
                           "W jakim porządku przedstawić dane?",
                           c("Rosnąco", "Malejąco")), width = 3, height = 2),
  mainPanel(plotlyOutput("firstPlot", height = 500, width = 1700)%>% 
              shinycssloaders::withSpinner(type = 5,
                                           color = "#1fa682"))
)

ui2 <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  titlePanel("Sytuacja absolwentów na rynku pracy po uzyskaniu dyplomu bez do świadczenia w branży"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("kierunek", "Wybierz kierunek",choices=c("Informatyka", "Pedagogika",
                                                           "Elektrotechnika","Administracja",
                                                           "Mechatronika","Teologia",
                                                           "Inżynieria i Analiza Danych")
      ),
      checkboxGroupInput("form", "Wybierz form?", choices = c("Stacjonarne" = "Stacjonarne", "Niestacjonarne" = "Niestacjonarne"),selected="Niestacjonarne"),
      sliderInput("npoints", "Liczba punktów",
                  min = 1, max = nrow(data) , value = 3500),
      sliderInput("x","Wybierz zakres na osi X", min=0, max=max(data$P_CZAS_PRACA_NDOSW)+2, value=c(0,max(data$P_CZAS_PRACA_NDOSW)+2)),
      sliderInput("y","Wybierz zakres dla osi y", min=0, max=max(data$P_E_ZAR_NDOSW)+500, value=c(0,max(data$P_E_ZAR_NDOSW)+500))
    ),
    mainPanel(plotlyOutput("pointPlot", height = 800) %>% 
                shinycssloaders::withSpinner(type = 5,
                                             color = "#1fa682"))
  ))
server = function(input, output, session){
  
  observeEvent(input$Typ,  {
    n <- nrow(generujTyp(input$Typ))
    updateSliderInput(session = session, inputId = "Liczba", max = min(n, 26),  value = 5)
  })
  
  output$firstPlot <- renderPlotly({
    string <- paste("Wykres średniego ryzyka bezrobocia\nze względu na ukończoną uczelnię dla placówek ",
                    substr(input$Typ, 1, nchar(input$Typ)-1) ,"ych", sep="")
    df %>% inner_join(generujTyp(input$Typ)) -> df1
    if (input$Rosnaco == "Rosnąco") {
      cat("a")
      df1 %>%
        mutate(P_NAZWA_UCZELNI = fct_reorder(P_NAZWA_UCZELNI, srednia, .desc = TRUE)) %>%
        top_n(input$Liczba) -> df1
    }
    else {
      df1 %>%
        mutate(P_NAZWA_UCZELNI = fct_reorder(P_NAZWA_UCZELNI, srednia, .desc = FALSE)) %>%
        top_n(input$Liczba) -> df1
    }
    
    ggplot(df1, aes(x = P_NAZWA_UCZELNI, y = srednia)) +
      geom_col(fill = "#1fa682", color = "Black") +
      coord_flip() +
      scale_y_continuous(expand = c(0, 0)) +
      labs(y = "Średnie ryzyko bezrobocia po uzyskaniu dyplomu (%)",
           x = "Nazwa uczelni",
           title = string) +
      theme_bw() +
      theme(text = element_text(size = 15), plot.title = element_text(size = rel(0.8)))
  })
  
  output$pointPlot<-renderPlotly({
    df<-data %>% 
      filter(P_KIERUNEK_NAZWA %in% input$kierunek) %>% 
      mutate(P_FORMA = ifelse(P_FORMA == "N", "Niestacjonarne", "Stacjonarne")) %>%
      filter(P_FORMA %in% input$form) %>% 
      arrange(P_CZAS_PRACA_NDOSW) %>% 
      head(input$npoints)
    
    
    
    
    ggplot(df, aes(P_CZAS_PRACA_NDOSW, P_E_ZAR_NDOSW,fill = P_FORMA,text = P_NAZWA_UCZELNI))+
      geom_point()+
      scale_fill_manual(values = c("Stacjonarne" = "red", "Niestacjonarne" = "green"), name="Forma")+
      xlab("Średni czas (w miesiącach) od uzyskania\ndyplomu do podjęcia pierwszej pracy")+
      ylab("Średnie miesięczne wynagrodzenie ze wszystkich źródeł")+
      labs(title="Wykres zależności między średnim czasem od uzyskania dyplomu do podjęcia \npierwszej pracy i średnim miesięcznym wynagrodzeniem") +
      scale_x_continuous(limits=(c(input$x[[1]],input$x[[2]])))+
      scale_y_continuous(limits=(c(input$y[[1]],input$y[[2]])))+
      theme_bw() +
      theme(text = element_text(size = 15), plot.title = element_text(size = rel(0.8)))
    
    
    
  })
  
  observeEvent(input$form, {
    df <- data %>% filter(P_FORMA %in% input$form)
    plotlyProxy("barplot", df)
  })
}

app_ui <- navbarPage(
  title = "HW5 Kacper Wnęk, Wojciech Grabias",
  tabPanel("Bezrobocie", ui1, icon = icon("briefcase", verify_fa = FALSE)),
  tabPanel("Czas do uzyskania pierwszej pracy", ui2,
           icon = icon("clock", verify_fa = FALSE)
  ),
  theme = bs_theme(bootswatch = "minty"))

shinyApp(ui = app_ui, server = server)
