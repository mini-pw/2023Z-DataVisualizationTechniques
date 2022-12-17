library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(plotly)

df <- data.frame(read.csv('graduates-major-data.csv', sep = ";",
               fileEncoding = "UTF-8")) %>% 
  filter(P_NAZWA_UCZELNI %in% c("Uniwersytet Warszawski",
                                "Uniwersytet Jagielloński w Krakowie",
                                "Politechnika Warszawska",
                                "Akademia Górniczo-Hutnicza im. Stanisława Staszica w Krakowie",
                                "Politechnika Gdańska",
                                "Politechnika Wrocławska",
                                "Gdański Uniwersytet Medyczny",
                                "Uniwersytet im. Adama Mickiewicza w Poznaniu",
                                "Uniwersytet Medyczny w Łodzi",
                                "Politechnika Łódzka")) %>% 
  select(contains(paste("P_WWB_MIES_", as.character(1:12), sep = "")), P_NAZWA_UCZELNI, P_KIERUNEK_NAZWA)

df2 <- data.frame(read.csv('graduates-major-data.csv', sep = ";",
                          fileEncoding = "UTF-8")) %>% 
  filter(P_NAZWA_UCZELNI %in% c("Uniwersytet Warszawski",
                                "Uniwersytet Jagielloński w Krakowie",
                                "Politechnika Warszawska",
                                "Akademia Górniczo-Hutnicza im. Stanisława Staszica w Krakowie",
                                "Politechnika Gdańska",
                                "Politechnika Wrocławska",
                                "Gdański Uniwersytet Medyczny",
                                "Uniwersytet im. Adama Mickiewicza w Poznaniu",
                                "Uniwersytet Medyczny w Łodzi",
                                "Politechnika Łódzka")) %>% 
  select(contains(paste("P_WWZ_MIES_", as.character(1:12), sep = "")), P_NAZWA_UCZELNI, P_KIERUNEK_NAZWA)

# Define UI for application that draws a histogram
ui <- fluidPage(#theme = shinytheme("yeti"),

    # Application title
    titlePanel("Students data"),
    navbarPage(
      "Interaktywne Wykresy",
      
      # Sidebar with a slider input for number of bins 
      tabPanel(
          "WWB absolwentów",
          sidebarPanel(
              selectInput("major_choices",
                          "Kierunek studiów",
                          unique(df$P_KIERUNEK_NAZWA),
                          multiple = TRUE,
                          selected = "Administracja"),
              selectInput("univ_choices",
                          "Uczelnia",
                          unique(df$P_NAZWA_UCZELNI),
                          multiple = TRUE,
                          selected = "Uniwersytet Jagielloński w Krakowie")
          ),
  
          # Show a plot of the generated distribution
          mainPanel(
             plotlyOutput('wwb_Plot')
          )
      ),
      tabPanel(
        "WWZ absolwentów",
        sidebarPanel(
          selectInput("major_choices2",
                      "Kierunek studiów",
                      unique(df$P_KIERUNEK_NAZWA),
                      multiple = TRUE,
                      selected = "Administracja"),
          selectInput("univ_choices2",
                      "Uczelnia",
                      unique(df$P_NAZWA_UCZELNI),
                      multiple = TRUE,
                      selected = "Uniwersytet Jagielloński w Krakowie")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput('wwz_Plot')
        )
      )
    )
    
)
df %>% distinct(P_KIERUNEK_NAZWA) %>%  count()
# Define server logic required to draw a histogram
server <- function(input, output) {

    df_wwb <- reactive(({
      df[c("P_KIERUNEK_NAZWA", "P_NAZWA_UCZELNI",
           paste("P_WWB_MIES_", as.character(1:12), sep = ""))] %>% 
        filter(P_KIERUNEK_NAZWA %in% input$major_choices) %>% 
        filter(P_NAZWA_UCZELNI %in% input$univ_choices) %>% 
        pivot_longer(cols = 3:14) %>% 
        mutate(mies = as.integer(substr(name, 12, 100))) %>%
        mutate(wwb_value = as.double(gsub(",", ".", value))) %>% 
        mutate(nazwa = paste(P_NAZWA_UCZELNI, P_KIERUNEK_NAZWA, sep = ": ")) %>% 
        select(nazwa, mies, wwb_value) %>% 
        group_by(nazwa, mies) %>% 
        summarise(.groups = "keep", avg_value = mean(wwb_value)) %>% 
        ungroup()
    }))
  
    output$wwb_Plot <- renderPlotly(
        # generate bins based on input$bins from ui.R 
        wwb_plot <- plot_ly(data = df_wwb(),
                x = ~mies,
                y = ~avg_value,
                color = ~nazwa,
                type = 'scatter',
                mode = 'lines+markers')
                %>% 
          layout(
            title = "Uśredniony względny wskaznik bezrobocia dla pierwszych 12 
                     miesięcy od uzyskania dyplomu", 
            xaxis = list(title = 'Miesiąc po uzyskaniu dyplomu'), 
            yaxis = list(title = 'Względny wskaźnik bezrobocia'), 
            legend = list(orientation = 'v', x = 0.5)
          )
    )
    
    df_zar <- reactive(({
      df2[c("P_KIERUNEK_NAZWA", "P_NAZWA_UCZELNI",
           paste("P_WWZ_MIES_", as.character(1:12), sep = ""))] %>% 
        filter(P_KIERUNEK_NAZWA %in% input$major_choices2) %>% 
        filter(P_NAZWA_UCZELNI %in% input$univ_choices2) %>% 
        pivot_longer(cols = 3:14) %>% 
        mutate(mies = as.integer(substr(name, 12, 100))) %>%
        mutate(zar_value = as.double(gsub(",", ".", value))) %>% 
        mutate(nazwa = paste(P_NAZWA_UCZELNI, P_KIERUNEK_NAZWA, sep = ": ")) %>% 
        select(nazwa, mies, zar_value) %>% 
        group_by(nazwa, mies) %>% 
        summarise(.groups = "keep", avg_value = mean(zar_value)) %>% 
        ungroup()
    }))
    
    output$wwz_Plot <- renderPlotly(
      # generate bins based on input$bins from ui.R 
      wwb_plot <- plot_ly(data = df_zar(),
                          x = ~mies,
                          y = ~avg_value,
                          color = ~nazwa,
                          type = 'scatter',
                          mode = 'lines+markers')
      %>% 
        layout(
          title = "Względny Wskaźnik Zarobków w 1. miesiącu po uzyskaniu dyplomu
                  ", 
          xaxis = list(title = 'Miesiąc po uzyskaniu dyplomu'), 
          yaxis = list(title = 'Względny wskaźnik zarobków'), 
          legend = list(orientation = 'v', x = 0.5)
        )
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
