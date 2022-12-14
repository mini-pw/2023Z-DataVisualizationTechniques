
library(shiny)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(shinycssloaders)
library(plotly)
library(tidyr)
library(rsconnect)


source("./data_preparation.R")


server <- function(input, output, session) {
  
  
  output$zar_plot <- renderPlot({
    
    
    df_zar <-
      df_uni %>% 
      select(U_UCZELNIA_SKROT, U_E_ZAR_P5, U_ROKDYP) %>% 
      filter(!is.na(U_E_ZAR_P5)) %>%
      filter(U_ROKDYP == input$rok) %>% 
      group_by(U_UCZELNIA_SKROT) %>% 
      summarise(sredaaa = mean(U_E_ZAR_P5)) %>% 
      arrange(-sredaaa) %>% 
      head(50)
    
    df_zar <- df_zar[input$zakres_uczelni[1]:input$zakres_uczelni[2],]
    
    
    ggplot(df_zar, aes(x=reorder(factor(U_UCZELNIA_SKROT),-sredaaa), y= sredaaa ))+
      geom_col() +
      theme_bw() +
      labs(title = "Universities with the highest average salaries of graduates",
           x = "University",
           y = "Avareage salaries of graduates [zł]")  +
      theme(text = element_text(size = 20))
  }, bg = "transparent")
  
  output$zar_kie_plot <- renderPlot({
   
    df_to_show <-
      df_stu %>% 
        select(P_KIERUNEK_NAZWA,P_UCZELNIA_SKROT, P_ROKDYP,P_E_ZAR_P3, P_E_ZAR_P5, P_E_ZAR)  %>% 
        filter(!(is.na(P_KIERUNEK_NAZWA) | P_KIERUNEK_NAZWA=="")) %>% 
        group_by(P_KIERUNEK_NAZWA, P_ROKDYP) %>% 
        summarise(srednia_p3 = mean(P_E_ZAR_P3, na.rm = TRUE),
                  srednia_p5 = mean(P_E_ZAR_P5, na.rm = TRUE),
                  srednia_p = mean(P_E_ZAR, na.rm = TRUE)) %>% 
        arrange(-srednia_p3) %>% 
        filter(P_KIERUNEK_NAZWA %in% input$kierunek)
  
      
      ggplot(df_to_show, aes_string(x= "P_ROKDYP", y= input$kiedy_zarobki, color = "P_KIERUNEK_NAZWA")) + 
      geom_smooth(method = "loess",size =3, se=FALSE) +
      labs(title = paste("Avarage salaries of graduates according to the field of 
      study and the number of year since receiving the degree"),
           x = "Year of receving the degree", y= "Average salaries of graduates [zł]",
           color = "Field of study") +
      theme(text = element_text(size = 20),
            plot.title = element_text(hjust= 0.5))
    
    
    
    
  }, bg = "transparent")
  
  
}


ui1 <- fluidPage(titlePanel("Top universities and fields of studies according to earnings of graduates"),
            
                 splitLayout(
                   
                   verticalLayout(
                     splitLayout(
                       checkboxGroupInput(
                         inputId = "kierunek",
                         label = "Field of study:",
                         c("Informatics" = "Informatyka",
                           "Law" = "Prawo",
                           "Speech therapy" = "Logopedia",
                           "Psychology" = "Psychologia",
                           "Economics" = "Ekonomia", 
                           "Philology" = "Filologia", 
                           "Pharmacy" = "Farmacja", 
                           "Physics" ="Fizyka", 
                           "History" = "Historia", 
                           "Sociology" = "Socjologia"),
                         selected = c("Informatyka")),
                       
                       radioButtons(
                         inputId = "kiedy_zarobki",
                         label = "Number of years since graduation",
                         c("Immediately after graduation" = "srednia_p",
                           "3 years" = "srednia_p3",
                           "5 years" = "srednia_p5"),
                         selected = c("srednia_p5"))
                     )
                     ,
       
                     plotOutput("zar_kie_plot"))
                   ,
                   
                   verticalLayout(
                     selectInput(
                       inputId = "rok",
                       label = "Year:",
                       choices = c(
                         "2014" = "2014",
                         "2015" = "2015",
                         "2016" = "2016")),

                     
                     plotOutput("zar_plot"),
                     sliderInput("zakres_uczelni", 
                                 "Range:",
                                 min = 1,
                                 max = 50,
                                 value = range(1,10))
                   )
                   
                   
                   )
     
                   )

app_ui <- navbarPage(
  title = "Shiny app",
  theme = bslib::bs_theme(bootswatch = "slate"),
  tabPanel("Top uni and majors", ui1)
)


shinyApp(app_ui, server)

#rsconnect::deployApp("C:/Users/Uzytkownik/Desktop/IiAD/sem3/Twd/hw5/app")
#rsconnect::setAccountInfo(name='kubitam', token='35BEF4AE42A227A9E4297424D427540B', secret='+drS3DRzaJ6DGJ0AIBlVOG4ESHjRGmJoFX1UjFZk')
