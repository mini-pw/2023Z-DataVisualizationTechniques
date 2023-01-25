library(forcats)
library(tidyr)
library(dplyr)
library(shiny)
library(ggplot2)
library(bslib)
library(leaflet)
library(plotly)
library(lubridate)

###

dane_polaczone<- read.csv("dane_wspolrzedne.csv")

photos_df <- read.csv("metadane_mateusz.csv",  encoding = "UTF-8")

photos_df_with_h_column <- photos_df %>% 
  mutate(FormattedTime = sub(":","/", Image.DateTime)) %>% 
  mutate(FormattedTime = sub(":","/", FormattedTime)) %>%
  filter(Image.DateTime != "") %>% 
  mutate(WeekDayDat = format(as.POSIXct(FormattedTime), format = "20%y-%m-%d"))%>%
  mutate(WeekDay = wday(WeekDayDat, label=TRUE)) %>% 
  mutate(WeekDay = gsub("\\\\","", WeekDay)) %>%
  mutate(WeekDay = gsub("\\.","", WeekDay)) %>%
  mutate(FormattedTime = format(as.POSIXct(FormattedTime), format = "%H:%M"))%>% 
  mutate(h = as.numeric(substr(FormattedTime, 1,2))) %>% 
  mutate(m = as.numeric(substr(FormattedTime, 4,5))) %>% 
  mutate(time_in_h = h + m/60) %>% 
  mutate(month = as.numeric(substr(Image.DateTime, 6, 7)))

photos_df <- read.csv("metadane_mateusz.csv",  encoding = "UTF-8")
photos_df$flashed = ifelse(photos_df$EXIF.Flash == "Flash fired, compulsory flash mode", 1, 0)
photos_df$hour = substr(photos_df$Image.DateTime, 12, 13)
photos_df$month = as.numeric(substr(photos_df$Image.DateTime, 6, 7))
photos_df$seconds = paste0(substr(photos_df$Image.DateTime,1,4),'-',substr(photos_df$Image.DateTime,6,7),'-',
                           substr(photos_df$Image.DateTime,9,19))
seconds = strptime(photos_df$seconds, format = "%Y-%m-%d %H:%M:%S")
photos_df$seconds = seconds
photos_df$seconds = as.numeric(photos_df$seconds)
photos2 <- photos_df[order(photos_df$seconds),]
seconds = photos2$seconds
seconds = seconds[2:2109]
seconds = c(seconds, 0)
photos2$seconds2 = seconds
x = photos2$seconds2 - photos2$seconds
photos2$roznica = x
photos2$duplikat = ifelse(photos2$roznica <= 1, 1, 0)


###


server <- function(input, output, session) {
  
  output$plotH <- renderPlot({
    ggplot(photos_df_with_h_column %>% 
             filter(month >= input$sliderH[1] & 
                      month <= input$sliderH[2]),
           aes(time_in_h)) +
      geom_histogram(binwidth = 0.25) +
      scale_x_continuous(expand = c(0,0), limits = c(0, 24), breaks = scales::breaks_width(6)) +
      labs(x = "Time",
           y = "Number of photos") +
      theme_minimal() +
      theme(text = element_text(size = 12))
  })
  
  days_in_the_week <- c("pon","wt","śr","czw", "pt", "sob", "niedz")
  
  output$plotW <- renderPlot({
    ggplot(photos_df_with_h_column %>% 
             filter(month >= input$sliderH[1] & 
                      month <= input$sliderH[2]) %>% 
             count(WeekDay)) +
      geom_bar(aes(factor(WeekDay, c("pon","wt","śr","czw","pt","sob","niedz")), n), stat = "identity") +
      labs(x = "Day of the week",
           y = "Number of photos") +
      theme_minimal() +
      theme(text = element_text(size = 12))#, family = "serif"))
  })
  
  output$mapa <- renderLeaflet({
    
    data <- dane_polaczone[dane_polaczone$kraje == input$kraje & dane_polaczone$imiona == input$osoby,]
    leaflet(data) %>%
      addTiles() %>%  
      addCircleMarkers(~ long,~ lat) %>% 
      addProviderTiles("Stamen.TonerHybrid")
  })
  output$hist2Plot <- renderPlotly({
    
    df = photos2%>%  
      filter(photos2$duplikat == 1 & photos2$month %in% input$slider) %>% 
      select(hour, duplikat)
    plot = plot_ly(
      data = df,
      x = ~hour,
      type = "histogram")%>%
      layout(title = "Number of duplicated pictures",  xaxis = list(title='Time'))
    
    
  })
  output$hist1Plot <- renderPlotly({
    
    df = photos2%>%  
      filter(photos2$flashed == 1 & photos2$month %in% input$slider) %>% 
      select(hour, flashed)
    plot = plot_ly(
      data = df,
      x = ~hour,
      type = "histogram")%>%
      layout(title = "Number of pictures with flash",  xaxis = list(title='Time'))
    
    
  })
  
}

ui1 <- fluidPage(titlePanel("Distribution of your photos during the day"),
                 
                 fluidRow(
                   column(6,
                          plotOutput("plotH")
                   ),
                   column(6,
                          plotOutput("plotW")
                   )
                 ),
                 
                 
                 
                 fluidRow(
                   column(6,sliderInput(
                     inputId = "sliderH",
                     label = "Pick months that interest you:",
                     min = 1,
                     max = 12,
                     value = c(1,12))
                   )))
                 

ui3 <- fluidPage(
  fluidRow(
    column(6,selectInput("kraje","Choose country:",
                         c("Poland","Germany","France")),
           
    ),
    column(6,selectInput("osoby","Choose person:",
                         c("Franek","Felicja","Mateusz"))
    )
  ),
  fluidRow(leafletOutput("mapa"))
)
                 
                 
                 
                 
                 

ui4 <- fluidPage(titlePanel("Distribution of duplicated and flash pictures during the day"),
                   
                 fluidRow(
                   column(6,
                          plotlyOutput("hist2Plot")
                   ),
                   column(6,
                          plotlyOutput("hist1Plot")
                   )
                 ),
                 
                 
                 fluidRow(
                   column(6,sliderInput(
                     inputId = "slider",
                     label = "Pick months that interest you:",
                     min = 1,
                     max = 12,
                     value = c(6,8))
                          )
                 
                 
                 
                 
                 ))

app_ui <- navbarPage(
  title = "Statistics of the photos you've made",
  tabPanel("Distribution", ui1),
  tabPanel("Location", ui3),
  tabPanel("Other", ui4),
  theme = bs_theme(bootswatch = "flatly"),
  footer = shiny::HTML("
                 <footer class='text-center text-sm-start' style='width:100%;'>
                 <hr>
                 <p class='text-center' style='font-size:12px;'>
                   Franciszek Szczepaniak, Felicja Warno, Mateusz Wiktorzak
                 </p>
                 </footer>
                 ")
)

shinyApp(app_ui, server)
