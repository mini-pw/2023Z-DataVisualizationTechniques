#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(bslib)

load("wynik.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bs_theme(bootswatch = "united"),

    # Application title
    titlePanel("Prezentacja danych o zarobkach wśród absolwentów szkół wyższych"),
    fluidRow(
      selectInput("selectWoj",
                  "Wybierz województwo (14 - Mazowsze)",
                  choices = unique(wynik$P_WOJ),
                  selected = 14
                  ),
    ),
    
    fluidRow(
      shinycssloaders::withSpinner(plotOutput("plotTopTen"),
                                  type = 5)
    ),
    
    fluidRow(
      splitLayout(
        radioButtons("selectForm",
                     "Forma studiów (Stacjonarne/Niestacjonarne)",
                     choices = unique(wynik$P_FORMA)),
        radioButtons("selectExp",
                     "Z doświadczeniem/Bez doświadczenia w trakcie studiów:",
                     choices = list(
                       "Z doświadczeniem" = TRUE,
                       "Bez doświadczenia" = FALSE
                     )),
        checkboxGroupInput("selectLvl",
                     "Poziom studiów:",
                     choices = unique(wynik$P_POZIOM),
                     selected = unique(wynik$P_POZIOM)
                     ),
        sliderInput(
          inputId = "selectYear",
          label = "Lata:",
          min = min(wynik$P_ROKDYP),
          max = max(wynik$P_ROKDYP),
          value = c(min(wynik$P_ROKDYP),max(wynik$P_ROKDYP)),
          step = 1
        ),
        
      )
    ),
    
    fluidRow(
      shinycssloaders::withSpinner(plotOutput("plotKierunki"),
                                   type = 5)
    )
    
)
?checkboxGroupInput
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plotTopTen <- renderPlot({
    wynik %>%
      filter(P_WOJ %in% input$selectWoj) %>% 
      group_by(Jednostka, P_UCZELNIA_SKROT) %>%
      summarise(Srednia = mean(P_E_ZAR_ETAT)) %>% 
      arrange(-Srednia) %>% 
      head(10) %>% 
      inner_join(y = wynik, by = "Jednostka") %>%  
      filter(P_WOJ %in% input$selectWoj) %>% 
    ggplot(aes(x = reorder(as.factor(Jednostka), -Srednia), y = P_E_ZAR_ETAT, fill = P_UCZELNIA_SKROT.x))+
      geom_violin() +
      geom_boxplot(width=0.1, color="black", alpha=0.2,show.legend = FALSE) +
      scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
      theme_bw() +
      labs(title = "Porównanie rozkładu zarobków wśród jednostek uczelni o absolwentach z największą średnią zarobków (max. 10)",
           x = "Jednostka Uczelni",
           y = "Zarobki po uzyskaniu dyplomu [zł]",
           fill = "Skrót Uczelni"
           )
  })
  
  output$plotKierunki <- renderPlot({
    temp <- wynik %>% 
      filter(P_WOJ %in% input$selectWoj)
    if(input$selectExp){
      temp <- temp %>%
        mutate(Zarobki = P_E_ZAR_ETAT_DOSW)
    }
    else{
      temp <- temp %>% 
        mutate(Zarobki = P_E_ZAR_ETAT_NDOSW)
    }
    
    if(input$selectForm == "S")
      temp <- temp %>% 
        filter(P_FORMA == "S")
    else
      temp <- temp %>% 
        filter(P_FORMA == "N")
    
    temp <- filter(temp, P_ROKDYP %in% input$selectYear)
    
    temp <- filter(temp, P_POZIOM %in% input$selectLvl)
    
    temp %>% 
      filter(!is.na(Zarobki)) %>%
      arrange(-Zarobki) %>% 
      head(10) -> temp
    
    ggplot(temp, aes(x = reorder(Kierunek.Rok,-Zarobki), y = Zarobki, fill = P_NAZWA_UCZELNI))+
      geom_col(position = position_dodge2()) +
      geom_text(aes(label = Zarobki), vjust = 1.5, position = position_dodge2(0.9))+
      scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
      scale_y_continuous(expand = c(0,0))+
      theme_bw() +
      labs(title = "Ranking kierunków studiów, których absolwenci zarabiają najwięcej (max. 10)",
           x = "Kierunek Studiów, Jednostka Uczelni, Poziom, Rok ",
           y = "Zarobki po uzyskaniu dyplomu [zł]",
           fill = "Nazwa Uczelni"
      )+
      theme(panel.grid = element_line(colour = 'black'))
  })
  }

# Run the application 
shinyApp(ui = ui, server = server)
