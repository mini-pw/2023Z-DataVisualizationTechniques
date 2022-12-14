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

institution_data <- read.csv("graduates-institution-data.csv", sep = ";")
major_data <- read.csv("graduates-major-data.csv", sep = ";" )
national_data <- read.csv("graduates-national-data.csv", sep = ";" )

pw <- major_data %>% 
  filter(P_NAZWA_UCZELNI == "Politechnika Warszawska" | P_NAZWA_UCZELNI == "PW") %>% 
  select(P_ROKDYP, P_E_ZAR_NDOSW,P_NAZWA_UCZELNI)

uw <- major_data %>% 
  filter(P_NAZWA_UCZELNI == "Uniwersytet Warszawski" | P_NAZWA_UCZELNI == "UW") %>% 
  select(P_ROKDYP, P_E_ZAR_NDOSW,P_NAZWA_UCZELNI)

df <- bind_rows(pw, uw)
names(df) <- c('Rok', "Zarobki", "Uczelnia")
names(pw) <- c('Rok', "Zarobki", "Uczelnia")
names(uw) <- c('Rok', "Zarobki", "Uczelnia")


df$Zarobki <- as.numeric(gsub(",", ".", df$Zarobki))
pw$Zarobki <- as.numeric(gsub(",", ".", pw$Zarobki))
uw$Zarobki <- as.numeric(gsub(",", ".", uw$Zarobki))

df <- df %>% 
  mutate("Zarobki" = Zarobki/100)

df2 <- major_data %>% 
  select(P_DZIEDZINA, P_E_ZAR, P_ROKDYP)

df2$P_E_ZAR <- as.numeric(gsub(",", ".", df2$P_E_ZAR))


df2 <- na.omit(df2)

names(df2) <- c("Dziedzina", "Zarobki", "Rok")


df2$Dziedzina[df2$Dziedzina == "Dziedzina nauk spoÅ‚ecznych"] <- "Nauki spoleczne"
df2$Dziedzina[df2$Dziedzina ==	"Dziedzina sztuki"] <- "Sztuka"
df2$Dziedzina[df2$Dziedzina ==	"Dziedzina nauk humanistycznych"] <- "Nauki humanistyczne"
df2$Dziedzina[df2$Dziedzina ==	"Dziedzina nauk inÅ¼ynieryjno-technicznych"] <- "Nauki inzynieryjno-techniczne"
df2$Dziedzina[df2$Dziedzina ==	"Dziedzina nauk medycznych i nauk o zdrowiu"] <- "Nauki medyczne i o zdrowiu"
df2$Dziedzina[df2$Dziedzina ==	"Dziedzina nauk Å›cisÅ‚ych i przyrodniczych"] <- "Nauki scisle i przyrodnicze"

df2 <- df2 %>% 
  filter(Dziedzina != "" & Dziedzina != "Dziedzina nauk rolniczych"   & Dziedzina != "Dziedzina nauk teologicznych" )

pw <- pw %>% 
  mutate("Zarobki" = Zarobki/100)
uw <- uw %>% 
  mutate("Zarobki" = Zarobki/100)

server <- shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    p1 <- pw %>% 
      ggplot(aes(x = Rok,  y = Zarobki)) + 
      geom_col(fill = '#f8766d') + 
      theme_minimal() + 
      labs(title = "Zarobki absolwentów danej uczelni względem lat",
           x = "Rok ukończenia studiów",
           y = "Zarobki (*100)") 
    
    p2 <- ggplot(
      uw,
      aes(x = Rok, y = Zarobki)
    ) +
      geom_col(fill = '#00bfc4') + 
      theme_minimal() +
      labs(title = "Zarobki absolwentów danej uczelni względem lat",
           x = "Rok ukończenia studiów",
           y = "Zarobki (*100)")
    
    p3 <- ggplot(
      df,
      aes(x = Rok, y = Zarobki, fill = Uczelnia)
    ) + 
      geom_col(position = "dodge") + 
      theme_minimal() + 
      labs(title = "Zarobki absolwentów danej uczelni względem lat",
           x = "Rok ukończenia studiów",
           y = "Zarobki (*100)")
    
    switch (input$zmienna,
      "PW" = p1,
      "UW" = p2,
      "PW i UW" = p3
    )
   
  })
  output$densityPlot <- renderPlot({
    pl2 <- df2  %>% 
      filter(Rok >= input$zakres[1],
             Rok <= input$zakres[2]) %>% 
      ggplot(
      aes(x = Zarobki, color = Dziedzina)
    ) + geom_density() +
      theme_minimal() + 
      labs(
        title = "Rozkład zarobków absolwentów danej dziedziny dla wybranego zakresu lat",
        x = "Zarobki",
        y = "Gęstość"
      )
    pl2
  })
  
})

ui <- fluidPage(
  ## 2.
  titlePanel("PW vs UW oraz zarobki zależnie od ukończonej dziedziny studiów"),
  fluidRow(
    column(6, 
           selectInput("zmienna",
                       "Wybierz uczelnię",
                       c("PW","UW", "PW i UW")
           ), plotOutput("distPlot"))
  ,
  column(6,
         sliderInput("zakres",
                     'Wybierz zakres lat',
                     value = c(min(df2$Rok), max(df2$Rok)),
                     min = min(df2$Rok),
                     max = max(df2$Rok),
                     step = 1), plotOutput('densityPlot')
      
  ))
)
  


app <- shinyApp(ui, server)

app
