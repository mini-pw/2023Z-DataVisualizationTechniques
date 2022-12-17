library(forcats)
library(tidyr)
library(dplyr)
library(shiny)
library(ggplot2)
library(bslib)




df <- as.data.frame(read.csv("graduates-national-data.csv", sep = ";"))
df_f <- df %>% 
  filter(PL_POZIOM == 1 & PL_ROKDYP != 2014) %>% 
  select(PL_ROKDYP,PL_PROC_MIES_BEZR_P1, PL_PROC_MIES_BEZR_P2,PL_PROC_MIES_BEZR_P3,PL_PROC_MIES_BEZR_P4,PL_PROC_MIES_BEZR_P5) %>% 
  mutate(Rok = as.numeric(gsub(",", ".",PL_PROC_MIES_BEZR_P1))) %>% 
  mutate(Dwa_lata = as.numeric(gsub(",", ".",PL_PROC_MIES_BEZR_P2))) %>% 
  mutate(Trzy_lata = as.numeric(gsub(",", ".",PL_PROC_MIES_BEZR_P3))) %>% 
  mutate(Cztery_lata = as.numeric(gsub(",", ".",PL_PROC_MIES_BEZR_P4))) %>% 
  mutate(Pięć_lat = as.numeric(gsub(",", ".",PL_PROC_MIES_BEZR_P5))) %>% 
  select(PL_ROKDYP,Rok,Dwa_lata,Trzy_lata,Cztery_lata, Pięć_lat)
data_checkbox <- df_f %>% filter(PL_ROKDYP >= 2015 & PL_ROKDYP <= 2020) %>% 
  select(-PL_ROKDYP)


data_checkbox <- t(data_checkbox)
means <- as.vector(rowMeans(data_checkbox,na.rm = TRUE))
names <- row.names(data_checkbox)
data_checkbox <- data.frame(means,names)
data_checkbox


df1 <- df %>% 
  filter(PL_POZIOM == 1 & PL_ROKDYP != 2014) %>% 
  select(PL_ROKDYP, PL_PROC_KONT, PL_PROC_UKON) %>% 
  mutate(PL_PROC_KONT = as.numeric(gsub(",", ".",PL_PROC_KONT))) %>% 
  mutate(PL_PROC_UKON = as.numeric(gsub(",", ".",PL_PROC_UKON)))
df1

df2 <- df %>% 
  filter(PL_POZIOM == 2) %>% 
  select(PL_ROKDYP, PL_CZAS_ETAT) %>% 
  mutate(PL_CZAS_ETAT = as.numeric(gsub(",", ".",PL_CZAS_ETAT)))


server <- function(input, output, session) {
  
  data <- reactive({
    if("PL_PROC_KONT" %in% input$fate) {
      df_KONT <- df1 %>% filter(PL_ROKDYP >= input$slider[1] & PL_ROKDYP <= input$slider[2])
      return(df_KONT$PL_PROC_KONT)
    }
    if("PL_PROC_UKON" %in% input$fate) {
      df_UKON <- df1 %>% filter(PL_ROKDYP >= input$slider[1] & PL_ROKDYP <= input$slider[2])
      return(df_UKON$PL_PROC_UKON)
    }
  })
  
  
  dataSlider <- reactive({
    
    
    data_f <- df_f %>% filter(PL_ROKDYP >= input$slider2[1] & PL_ROKDYP <= input$slider2[2]) %>% 
      select(-PL_ROKDYP)
    
    
    data_f <- t(data_f)
    means <- as.vector(rowMeans(data_f,na.rm = TRUE))
    names <- row.names(data_f)
    data_f <- data.frame(means,names) %>% filter(names %in% input$lata)
    return(data_f)
    
  })
  
  
  output$plotf <- renderPlot({
    ggplot(dataSlider(),aes(x = fct_inorder(names),means))+
      geom_bar(stat ="identity")+
      labs(title = "Ryzyko bezrobocia absolwentów w zależności od ilości lat po uzyskaniu dyplomu",
           y = "Średnia wartość ryzyka bezrobocia wyrażona w procentach", 
           x = "Dla jakiego czasu po ukończeniu studiów"  )+
      theme(text = element_text(size = 15, family = "serif"))
    
    
  })
  
  output$plot1 <- renderPlot({
    ggplot(df1 %>%
             filter(PL_ROKDYP >= input$slider[1] & PL_ROKDYP <= input$slider[2]) %>%
             rename(y = input$fate),
           aes(as.factor(PL_ROKDYP), data())) +
      geom_point() +
      scale_y_continuous(limits = c(0,100)) +
      labs(title = "Procent studentów, którzy podjęli studia II stopnia w zależności od roku",
           y = "Procent absolwentów, którzy podjęli studia II stopnia",
           x = "Lata") +
      theme_grey() +
      theme(text = element_text(size = 15, family = "serif"))
    
  })
  
  output$plot2 <- renderPlot({
    ggplot(df2 %>%
             filter(PL_ROKDYP >= input$slider3[1] & PL_ROKDYP <= input$slider3[2]),
           aes(as.factor(PL_ROKDYP), PL_CZAS_ETAT)) +
      geom_point() +
      labs(y = "Przerwa pomiędzy studiami a pracą(w miesiącach)",
           x = "Lata") +
      scale_y_continuous(limits = c(0,7)) +
      theme_grey() +
      theme(text = element_text(size = 15, family = "serif"))
    
  })
}
ui1 <- fluidPage(
  titlePanel("Studenci I stopnia"),
  
  fluidRow(
    column(3,
           sliderInput(
             inputId = "slider",
             label = "Wybierz lata danych dla pierwszego wykresu:",
             min = 2015,
             max = 2020,
             value = c(2015, 2020)
           )
           
           
    ),
    column(3,
           
           selectInput(
             inputId = "fate",
             label = "Wybierz czy studenci ukończyli studia II stopnia:",
             choices = c(
               "Ukończyli" = "PL_PROC_UKON",
               "Nie ukończyli" = "PL_PROC_KONT"
             )
           )
           
           
    ),
    column(3,
           sliderInput(
             inputId = "slider2",
             label = "Wybierz lata danych dla drugiego wykresu:",
             min = 2015,
             max = 2020,
             value = c(2015, 2020)
           )
           
           
    ),
    column(3,
           checkboxGroupInput("lata",
                              "Wybierz lata po ukończeniu studiów, dla których interesują cię wyniki drugiego wykresu",
                              unique(data_checkbox$names),
                              selected = c("Rok","Pięć_lat")
           )
    )
    
  ),
  fluidRow(
    column(6,
           plotOutput("plot1"),
           width = 5
    ),
    column(6,
           plotOutput(("plotf"))
    )
  )
)



ui2 <- fluidPage(titlePanel("Studenci II stopnia"),
                 
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput(
                       inputId = "slider3",
                       label = "Wybierz lata danych które cię interesują:",
                       min = 2015,
                       max = 2020,
                       value = c(2015, 2020)
                     ),
                     width = 2
                   ),
                   mainPanel(
                     plotOutput("plot2"),
                     width = 5
                     
                   )
                 )
                 
)

app_ui <- navbarPage(
  title = "Badanie losów studentów",
  tabPanel("I stopnia", ui1),
  tabPanel("II stopnia", ui2),
  theme = bs_theme(bootswatch = "readable")
)

shinyApp(app_ui, server)

