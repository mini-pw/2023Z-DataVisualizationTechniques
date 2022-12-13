library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(packrat)
library(rsconnect)
library(bslib)

dane <- read.csv("https://www.ela.nauka.gov.pl/dataExperts/graduates/graduates-major-data.csv", header=TRUE,sep=";", fileEncoding ="UTF-8",stringsAsFactors=FALSE)

source('ui8.R')

server <- function(input, output, session) {
  output$plot3 <- renderPlot({
    if(input$typ2 == "zarobki"){
      p <- dane %>%
        filter(P_DZIEDZINA != "") %>%
        select(P_DZIEDZINA, k = paste("P_E_ZAR_", input$dosw, input$rokPoStudiach, sep = "")) %>%
        ggplot(aes(x = P_DZIEDZINA,
                   y = as.numeric(gsub("(,)", ".", k)))) +
        geom_boxplot(fill = "#25b09c") +
        scale_y_continuous(limits = c(0,20000)) +
        labs(y = "Zarobki",
             x = "Dziedzina studiów")
    }
    
    if(input$typ2 == "bezrobocie"){
      p <- dane %>%
        filter(P_DZIEDZINA != "") %>%
        select(P_DZIEDZINA, k = paste("P_PROC_MIES_BEZR_", input$dosw , input$rokPoStudiach, sep = "")) %>%
        ggplot(aes(x = P_DZIEDZINA,
                   y = as.numeric(gsub("(,)", ".", k)))) +
        geom_boxplot(fill = "#25b09c") +
        scale_y_continuous(limits = c(0,50)) +
        labs(y = "Ryzyko bezrobocia",
             x = "Dziedzina studiów")
    }
    p + theme_minimal()+
      theme( axis.text.x = element_text(size = 15),
             axis.text.y = element_text(size = 15),
             axis.title = element_text(size = 20),
             strip.text.x = element_text(size = 20),
             strip.text.y = element_text(size = 20)
      ) +
      scale_x_discrete(labels = c("humanistyczne", "inżynieryjno techniczne", "medyczne", "rolnicze", "społeczne", "ścisłe i przyrodznicze", "teologiczne", "sztuka") %>%
                         str_wrap(width = 10))
    
  }
  )
  
  
  output$plot1 <- renderPlot({
    if(input$wybor == "P_PROC_KONT")
    {p <- dane %>% 
      filter(P_DZIEDZINA != "") %>% 
      group_by(P_DZIEDZINA) %>% 
      summarise(srednia1 = mean(as.numeric(gsub("(,)", ".", P_PROC_KONT)), na.rm = TRUE),
                srednia2 = mean(as.numeric(gsub("(,)", ".", P_PROC_UKON)), na.rm = TRUE)) %>%
      pivot_longer(!P_DZIEDZINA,names_to = "srednia", values_to = "wartosci") %>%
      ggplot(aes(fill = srednia, x = P_DZIEDZINA, y = wartosci)) +
      geom_bar(position = "dodge", stat = "identity")+
      theme_minimal()+
      theme( axis.text.x = element_text(size = 15),
             axis.text.y = element_text(size = 15),
             axis.title = element_text(size = 20),
             strip.text.x = element_text(size = 20),
             strip.text.y = element_text(size = 20),
             legend.text = element_text(size = 15),
             legend.title = element_blank(),
             legend.position = "top"
      )+
      labs(y = "Procent studentów",
           x = "Dziedzina studiów")+
      scale_x_discrete(labels = c("humanistyczne", "inżynieryjno techniczne", "medyczne", "rolnicze", "społeczne", "ścisłe i przyrodznicze", "teologiczne", "sztuka") %>%
                         str_wrap(width = 10))+
      scale_fill_manual(values = c("#25b09c", "#2c3e50"),
                        labels = c("Rozpoczęcie studiów", "Ukończenie studiów"))
    }
    
    if(input$wybor == "P_PROC_DOKTORAT")
    {p <- dane %>% 
      filter(P_DZIEDZINA != "") %>% 
      group_by(P_DZIEDZINA) %>% 
      summarise(srednia1 = mean(as.numeric(gsub("(,)", ".", P_PROC_DOKTORANCKIE)), na.rm = TRUE),
                srednia2 = mean(as.numeric(gsub("(,)", ".", P_PROC_DOKTORAT)), na.rm = TRUE)) %>%
      pivot_longer(!P_DZIEDZINA,names_to = "srednia", values_to = "wartosci") %>%
      ggplot(aes(fill = srednia, x = P_DZIEDZINA, y = wartosci)) +
      geom_bar(position = "dodge", stat = "identity")+
      theme_minimal()+
      theme( axis.text.x = element_text(size = 15),
             axis.text.y = element_text(size = 15),
             axis.title = element_text(size = 20),
             strip.text.x = element_text(size = 20),
             strip.text.y = element_text(size = 20),
             legend.text = element_text(size = 15),
             legend.title = element_blank(),
             legend.position = "top"
      )+
      labs(y = "Procent studentów",
           x = "Dziedzina studiów")+
      scale_x_discrete(labels = c("humanistyczne", "inżynieryjno techniczne", "medyczne", "rolnicze", "społeczne", "ścisłe i przyrodznicze", "teologiczne", "sztuka") %>%
                         str_wrap(width = 10))+
      scale_fill_manual(values = c("#25b09c", "#2c3e50"), 
                        labels = c("Rozpoczęcie studiów", "Ukończenie studiów"))
    }
    
    if(input$wybor == "P_PROC_DYPLOM")
    {p <- dane %>% 
      filter(P_DZIEDZINA != "") %>% 
      group_by(P_DZIEDZINA) %>% 
      summarise(srednia1 = mean(as.numeric(gsub("(,)", ".", P_PROC_DYPLOM)), na.rm = TRUE)) %>%
      pivot_longer(!P_DZIEDZINA,names_to = "srednia", values_to = "wartosci") %>%
      ggplot(aes(fill = srednia, x = P_DZIEDZINA, y = wartosci)) +
      geom_bar(position = "dodge", stat = "identity")+
      theme_minimal()+
      theme( axis.text.x = element_text(size = 15),
             axis.text.y = element_text(size = 15),
             axis.title = element_text(size = 20),
             strip.text.x = element_text(size = 20),
             strip.text.y = element_text(size = 20),
             legend.position = "none"
      )+
      labs(y = "Procent studentów",
           x = "Dziedzina studiów")+
      scale_x_discrete(labels = c("humanistyczne", "inżynieryjno techniczne", "medyczne", "rolnicze", "społeczne", "ścisłe i przyrodznicze", "teologiczne", "sztuka") %>%
                         str_wrap(width = 10))+
      scale_fill_manual(values = c("#25b09c"))
    }
    p}
  )
}