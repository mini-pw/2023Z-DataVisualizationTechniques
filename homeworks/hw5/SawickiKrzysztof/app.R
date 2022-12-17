#HW5

#Biblioteki
library(shiny)
library(dplyr)
library(ggplot2)
library(bslib)
library(shinycssloaders)
library(stringi)
library(packrat)
library(rsconnect)

#Wczytywanie danych
uczelnie_nazwy <- read.csv("uczelnienazwyUTF8.csv", sep = ";", fileEncoding = "UTF-8")   
uczelnie_Dane <- read.csv("uczelnieUTF8.csv", sep = ";", fileEncoding = "UTF-8")         
kierunki_dane <- read.csv("kierunkiUTF8.csv", sep = ";", fileEncoding = "UTF-8")         

#Przygotowywanie danych do wykresu

uczelnie_Dane %>% filter(U_UCZELNIA_ID < 3861, U_UCZELNIA_ID > 3844) -> uczelnie_Dane

uczelnie_Dane %>% 
  right_join(y = uczelnie_nazwy, by = c("U_UCZELNIA_ID")) %>% 
  arrange(U_UCZELNIA_ID) -> df

df <- df[,c(3,4,764,seq(from = 406, to = 418, by = 3))]
df <- na.omit(df)

df %>%
  group_by(U_NAZWA_UCZELNI.y,U_POZIOM) %>% 
  summarize_all(funs(stri_sub(.,1,4))) %>% 
  summarize_all(funs(as.integer(.)))%>% 
  summarize_all(funs(mean(., na.rm = T))) %>% 
  rename(PierwszyRok = U_ME_ZAR_ETAT_P1,
         DrugiRok = U_ME_ZAR_ETAT_P2, 
         TrzeciRok = U_ME_ZAR_ETAT_P3, 
         CzwartyRok = U_ME_ZAR_ETAT_P4,
         PiatyRok = U_ME_ZAR_ETAT_P5) ->dfcol

kierunki_dane %>% filter(P_UCZELNIA < 3861, 
                         P_UCZELNIA > 3844, 
                         P_ZAR_ETAT_MAX != "",
                         P_DZIEDZINA != "")  %>% 
  right_join(y = uczelnie_nazwy, by = c("P_UCZELNIA" = "U_UCZELNIA_ID")) %>% 
  mutate(P_ZAR_ETAT_MAX = as.integer(stri_sub(P_ZAR_ETAT_MAX,1,4))) -> dfdens

# ui
ui <- fluidPage(
  
  # Application title
  titlePanel("Porownanie srednich zarobkow dla absolwentow polskich Politechnik"),
  fluidRow(
    column(12, 
           plotOutput("colPlot")  %>% 
             shinycssloaders::withSpinner(type = 8,
                                          color = "#003cff")
    )
  ),
  br(),
  br(),
  br(),
  fluidRow(
    column(6,
           checkboxGroupInput("poziomStudiow",
                              "Wybierz poziom studiów",
                              selected = unique(dfcol$U_POZIOM)[1],
                              choiceNames = list("Inzynierskie i Licencjackie", "Magisterskie"),
                              choiceValues = unique(dfcol$U_POZIOM)[1:2]
           )
    ),
    column(6,
           sliderInput("rok",
                       "Wybierz rok po uzyskaniu dyplomu",
                       min = 1,
                       max = 5,
                       value = 1)
    )
  )
)

ui2 <- fluidPage(
  titlePanel("Porównanie zarobkow dla absolwentów różnych kierunków na polskich Politechnikach"),
  
  fluidRow(
    column(12, 
           plotOutput("densPlot")  %>% 
             shinycssloaders::withSpinner(type = 8,
                                          color = "#003cff")
    )
  ),
  br(),
  br(),
  fluidRow(
    column(4,
           radioButtons("poziomStudiow2",
                        "Wybierz poziom studiów",
                        selected = unique(dfcol$U_POZIOM)[1],
                        choiceNames = list("Inżynierskie i Licencjackie", "Magisterskie"),
                        choiceValues = unique(dfcol$U_POZIOM)[1:2])
    ),
    column(4,
           selectInput("politechnika",
                       "Wybierz Politechnike",
                       unique(dfdens$U_NAZWA_UCZELNI)),
           uiOutput("politechnikaWyb")
    ),
    column(3,
           actionButton("pudzian", "Kliknij, aby poznać zdanie pudziana na temat studiow", icon("book"), 
                        style="color: #2FA4E7; background-color: #2FA4E7; border-color: #2FA4E7"),
           imageOutput("myImage")
    ),
  )
)

# server
server <- function(input, output) {
  
  output$colPlot <- renderPlot({
    dfcol <- dfcol %>% filter(U_POZIOM %in% as.character(input$poziomStudiow))
    ggplot(dfcol, aes_string(x = colnames(dfcol)[3+input$rok], y = "U_NAZWA_UCZELNI.y",fill = "U_POZIOM")) +
      geom_col(position = position_dodge(width=0.5), color = "black")+
      labs(x = "Mediana średnich miesięcznych wynagrodzeń absolwentów z tytułu umów o prace", 
           y = "Nazwa uczelni",fill = "Poziom studiow")+ 
      scale_x_continuous(expand = c(0,0), limits = c(0, 8000))+
      theme_bw()+
      theme(plot.title = element_text(size=18))+
      scale_fill_manual(values = c("#2FA4E7", "#05659c"))+
      theme(axis.title=element_text(size=14), axis.text=element_text(size=12, colour = "black"))+
      theme(legend.title = element_text(face = "bold", size = 14), legend.text = element_text(size = 14))+
      theme(legend.background = element_rect(fill="white",
                                             size=0.5, linetype="solid", 
                                             colour ="black"))
    
  })
  
  output$densPlot <- renderPlot({
    
    dfdens <- dfdens %>% 
      filter(P_POZIOM %in% as.character(input$poziomStudiow2)) %>% 
      filter(U_NAZWA_UCZELNI == as.character(input$politechnika)) %>% 
      filter(P_DZIEDZINA %in% as.character(input$politechnikaWyb))
    
    ggplot(dfdens, aes(x = P_ZAR_ETAT_MAX, fill = P_DZIEDZINA)) + 
      geom_density(alpha = 0.7) +
      scale_x_continuous(expand = c(0,0), breaks = c(1000,1250, 2500, 5000, 7500, 9000)) +
      scale_y_continuous(expand = c(0,0))+
      labs(x = "Maksimum średnich miesięcznych wynagrodzeń absolwentów z tytułu umów o prace po uzyskaniu dyplomu", 
           y = "Gęstość",fill = "Dziedzina studiów")+
      theme_bw()+
      theme(plot.title = element_text(size=18))+
      theme(axis.title=element_text(size=14), axis.text=element_text(size=12, colour = "black"))+
      theme(legend.title = element_text(face = "bold", size = 14), legend.text = element_text(size = 14))+
      theme(legend.background = element_rect(fill="white",
                                             size=0.5, linetype="solid", 
                                             colour ="black"))
  })
  
  output$politechnikaWyb <- renderUI({
    
    dfdens %>% filter(U_NAZWA_UCZELNI == input$politechnika) %>% filter(P_POZIOM == input$poziomStudiow2) -> df3
    
    checkboxGroupInput("politechnikaWyb", 
                       "Wybierz dziedzine studiów", 
                       unique(df3$P_DZIEDZINA),
                       selected = unique(df3$P_DZIEDZINA)[1]) 
    
  })
  
  output$myImage <- renderImage({
    
    if (input$pudzian == 0){
      filename <- normalizePath(file.path(
        paste('pudzian', input$n, '.png', sep='')))
      
      # Return a list containing the filename and alt text
      list(src = filename,
           alt = paste("Image number", input$n))
      
    } else{
      filename <- normalizePath(file.path(
        paste('pudziancytat', input$n, '.png', sep='')))
      
      # Return a list containing the filename and alt text
      list(src = filename,
           alt = paste("Image number", input$n))
      
    }
    
  }, deleteFile = FALSE)
}

app_ui <- navbarPage(
  title = "Ekonomiczne losy absolwentów politechnik w Polsce",
  tabPanel("Zarobki studentów", ui, icon = icon("sack-dollar")),
  tabPanel("Porównanie uczelni", ui2, icon = icon("building-columns")),
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-left' style='font-size:12px;'>
                  <a class='text-dark' href='https://www.ela.nauka.gov.pl/pl/experts/source-data'>Źródło danych</a>
                </p>
                </footer>
                "),
)

# App
shinyApp(app_ui, server = server)


