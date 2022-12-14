#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(shinycssloaders)
library(shinythemes)


df <- read.csv("graduates-major-data.csv", sep=";")

data <- df %>% 
  select(P_ROKDYP, P_KIERUNEK_NAZWA, P_KIERUNEK_ID, P_ME_ZAR, P_NAZWA_UCZELNI, P_NAZWA_JEDN) %>% 
  filter(P_ME_ZAR != "", P_ME_ZAR != "0") %>% 
  mutate(P_ME_ZAR = as.numeric(gsub(",", ".", P_ME_ZAR))) %>% 
  mutate(P_NAZWA_JEDN = ifelse(P_NAZWA_JEDN!="",P_NAZWA_JEDN,"Nieokreślony"))

additional <- data %>% 
  group_by(P_NAZWA_UCZELNI,P_KIERUNEK_NAZWA, P_NAZWA_JEDN) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  group_by(P_NAZWA_UCZELNI) %>% 
  summarise(s = n()) %>% 
  filter(s > 3)

data2 <- data %>% filter(P_NAZWA_UCZELNI %in% additional$P_NAZWA_UCZELNI)


ui1 <- fluidPage(


    titlePanel("Porównanie średnich miesięcznych zarobków absolwentów wybranych kierunków w danym roku"),
    fluidRow(
      
      column(3,
             selectInput("kierunek1",
                         "Kierunek1:",
                         unique(df$P_KIERUNEK_NAZWA),
             ),          
      ),
      
      column(3,
             selectInput("kierunek2",
                         "Kierunek2:",
                         c("-----------------",unique(df$P_KIERUNEK_NAZWA)),
                         "-----------------"
             ),    
      ),
      column(8,
        shinycssloaders::withSpinner(plotOutput("distPlot"))
      )
    )

)

ui2 <- fluidPage(
  titlePanel(h1("Porównanie średnich miesięcznych zarobków absolwentów wybranej uczelni")),
  titlePanel(h5("Lata 2014-2020")),
  titlePanel(h5(br())),
  
  
  selectInput("uczelnia",
              "Uczelnia:",
              unique(df$P_NAZWA_UCZELNI),
              "Politechnika Warszawska",
              width=620
  ),
  
  fluidRow(
    column(3,
           radioButtons("najlepsze",
                        "Jakie kierunki chesz zobaczyć?",
                        c("Najlepsze" = TRUE, "Najsłabsze" = FALSE))
           ),
    column(3,
           sliderInput("ile",
                       "Ile kierunków chcesz zobaczyć?",
                       5, 
                       min = 3, 
                       max = 10,
                       step = 1
                       
           )  
    )
  ),
  
  
  shinycssloaders::withSpinner(plotOutput("distPlot2"))
)


server <- function(input, output, session) {
  
    output$distPlot <- renderPlot({
      data %>% filter(P_KIERUNEK_NAZWA==input$kierunek1 | P_KIERUNEK_NAZWA==input$kierunek2) %>% 
        group_by(P_ROKDYP, P_KIERUNEK_NAZWA) %>%
        summarise(mean = mean(P_ME_ZAR)) %>% 
        ggplot(aes(x=as.factor(P_ROKDYP), y=mean, fill=P_KIERUNEK_NAZWA))+
        geom_col(position = "dodge") +
        labs(title = "Średnie miesięczne zarobki studentów w pierwszym roku po uzyskaniu dyplomu",
             y = "Średnie zarobki",
             x = "Rok",
             fill='Kierunek')+
        theme(plot.title = element_text(size=22),
              text = element_text(size = 16))
    })
    
    observe({
      updateSelectInput(session, "kierunek2",
                        choices = c("-----------------",unique(df$P_KIERUNEK_NAZWA[df$P_KIERUNEK_NAZWA!=input$kierunek1])),
      )
    })
    
    output$distPlot2 <- renderPlot({
      dt <- data %>% 
        filter(P_NAZWA_UCZELNI == input$uczelnia) %>% 
        group_by(P_KIERUNEK_NAZWA, P_NAZWA_JEDN) %>%
        summarise(mean = mean(P_ME_ZAR)) %>% 
        arrange(mean)
      if (input$najlepsze) {
        p <- ggplot(tail(dt,input$ile),aes(x=mean, y=reorder(interaction(P_KIERUNEK_NAZWA,P_NAZWA_JEDN),mean), fill=P_NAZWA_JEDN))+
          geom_col(position = "dodge") +
          scale_y_discrete(labels = tail(dt,input$ile)$P_KIERUNEK_NAZWA)
      } else {
        p <- ggplot(head(dt,input$ile),aes(x=mean, y=reorder(interaction(P_KIERUNEK_NAZWA,P_NAZWA_JEDN),-mean), fill=P_NAZWA_JEDN))+
          geom_col(position = "dodge") +
          scale_y_discrete(labels = (head(dt,input$ile) %>% arrange(-mean))$P_KIERUNEK_NAZWA)
      }
        p + labs(title = "Średnie miesięczne zarobki studentów w pierwszym roku po uzyskaniu dyplomu",
                 y = "Kierunek",
                 x = "Średnie zarobki",
                 fill='Wydział') +
          theme(text = element_text(size = 16),
                plot.title = element_text(size=25,hjust = 0.5))
    })
    
    observe({
      val <- additional %>% 
        filter(P_NAZWA_UCZELNI == input$uczelnia) %>% 
        select(s) %>% 
        pull()
      if (val<10) {
        updateSliderInput(session, "ile",
                          "Ile kierunków chcesz zobaczyć?", 
                          min = 3, 
                          max = val,
                          step = 1
        )
      } else {
        updateSliderInput(session, "ile",
                          "Ile kierunków chcesz zobaczyć?",
                          5,
                          min = 3, 
                          max = 10,
                          step = 1
        )
      }
      
    })
    
}


app_ui <- navbarPage(
  title = "Analiza losów studentów",
  tabPanel("Porównanie dwóch wybranych kierunków", ui1),
  tabPanel("Porównanie kierunków na wybranej uczelni", ui2),
  theme = shinytheme("united")
)

shinyApp(ui = app_ui, server = server)
