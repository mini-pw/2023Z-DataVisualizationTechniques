library(ggplot2)
library(shiny)
library(dplyr)
library(shinycssloaders)
library(leaflet)
library(Cairo)
library(shinyWidgets)
library(htmltools)
library(shinyBS)
library(shinyjs)
library(jsonlite)
library(rjson)
library(reticulate)
library(purrr)
library(jsontools)
options(shiny.usecairo=T)
useShinyjs()

map_df <- read.csv("map_data/map_df.csv", encoding = 'UTF-8')
filterData <- readRDS(file = "time_data/data.rds")
baseFrame <- readRDS(file = "time_data/baseFrame.rds")
trans_df <- read.csv("trans_data/dataC", encoding = 'UTF-8')


info_ui <- fluidPage(
  
  h1(
    "About the project",
    align = "center",
    style = "color: #2fa4e7;"
  ),
  
  fluidRow(
    column(width = 3),
    column(width = 6, 
           "Welcome to our webpage. This shiny app was created for Data 
           Visualisation Techniques course by 3 WUT students. For almost two months we have
           been collecting data using Google Maps, which had access to our
           location 24 hours a day. Thanks to this, we could acquire information
           such as time spent in a certain place or mean of transport used.
           In our visualizations each person has a unique, assigned color: 
           ",
           style = "text-align: center;"),
    column(width = 3)),
  br(),
  fluidRow(
    column(width = 1),
    column(width = 4,
           img(src = 'czarek.png', height = '220px',
               style = "display: block; margin: 0 auto; cursor: pointer;",
               onclick ="window.open('https://github.com/CCzarek', '_blank')"),
           br(),
           tags$figcaption("Czarek", align = 'center', style = "font-size: 30px;")

    ),
    column(width = 2,
           img(src = 'tymek.png', height = '220px', style = "display: block; margin: 0 auto; cursor: pointer;",
               onclick ="window.open('https://github.com/tymsoncyferki', '_blank')"),
           br(),
           tags$figcaption("Tymek", align = 'center', style = "font-size: 30px;")
           
    ),
    column(width = 4,
           img(src = 'wojtek.png', height = '220px', style = "display: block; margin: 0 auto; cursor: pointer;",
               onclick ="window.open('https://github.com/wojo501', '_blank')"),
           br(),
           tags$figcaption("Wojtek", align = 'center', style = "font-size: 30px;")
    ),
    column(width = 1),
  ),
  br(),
  fluidRow(
    column(width = 3),
    column(width = 6, 
           "You can check out our github profiles simply by clicking on colored icons. 
           ",
           style = "text-align: center;"),
    column(width = 3)),
  br(),
  br(),
  fluidRow(
    column(width = 2),
    column(width = 4,
           img(src = 'maps_logo.png', height = '180px',
               style = "display: block; margin: 0 auto; cursor: pointer;",
               onclick ="window.open('https://takeout.google.com/takeout/custom/local_actions,location_history,maps,mymaps?dnm=false&continue=https://myaccount.google.com/yourdata/maps&hl=en&utm_source=privacy-advisor-maps',
               '_blank')"),
           br(),
           tags$figcaption("Download your Google Maps data", align = 'center', style = "font-size: 25px;")
    ),
    column(width = 4,
           img(src = 'github_logo.png', height = '180px',
               style = "display: block; margin: 0 auto; cursor: pointer;",
               onclick ="window.open('https://github.com/tymsoncyferki/TWD_Projekt2',
               '_blank')"),
           br(),
           tags$figcaption("View our Github repository", align = 'center', style = "font-size: 25px;")
    ),
  ),
  
    
)


map_ui <- fluidPage(
  
  fluidRow(
    column(width = 1),
    column(width = 11, 
           titlePanel("Map of visited places"))),
  
  
  fluidRow(
    column(width = 1),
    column(width = 11, 
           "Map of places we have visited in a given date range. The bigger the circle, 
    the more visited the place is.")),
  
  br(),
  
  fluidRow(
    column(width = 1),
    column(width = 3,
           wellPanel(checkboxGroupButtons(
             inputId = "persons",
             label = "Select persons:",
             choices = c("Czarek", "Tymek", "Wojtek"),
             selected = c("Czarek", "Tymek", "Wojtek"),
             individual = TRUE,
             status = c("primary", "secondary" , "success")
             
           ),
           
           dateRangeInput("date_range", "Select date range:",
                          start = "2022-12-07", end = "2023-01-24",
                          min = "2022-12-07", max = "2023-01-24",
                          format = "yyyy-mm-dd", startview = "month",
                          autoclose = TRUE)),
           br(),
           "Thanks to the interactivity of the map, you can easily check where
           each person spent new year's eve or which place has he visited most often.
           Of course, everybody's most visited places are home (or a dorm) and the university.
           As you can see, the person who travelled the most is Wojtek. He has 
           visited such places as Berlin, Hamburg or Tri-City. Unlike him, Tymek
           has never left Masovia and the furthest place he has been to is Piaseczno
           county. Because of it, the center of Warsaw is mostly covered with yellow markers. 
           Coming to Czarek, it can be seen that he lives outside the city and he 
           uses Warsaw East Station a lot. On winter holidays he went to Sokołów and Toruń.
           Are you interested in which places we visit and how often? Use our map 
           and try to find out yourself!",
           
           style = "text-align: justify;"
           
    ),
    column(width = 7,
           shinycssloaders::withSpinner(
             leafletOutput("placeMap", height = "550px"),
             color = "#2fa4e7"))
    
  ),
)


time_ui <- fluidPage(
  
  fluidRow(
    column(width = 1),
    column(width = 11, 
           titlePanel("Time spent"))),
  fluidRow(
    column(width = 1),
    column(width = 11, 
           "Hours spent in places from certain category.")),
  br(),
  
  fluidRow(
    column(width = 1),
    column(width = 3,
           wellPanel(
             fluidRow(
               checkboxGroupButtons(
                 inputId = "osoby",
                 label = "Select persons:",
                 choices = c("Czarek", "Tymek", "Wojtek"),
                 selected = c("Czarek", "Tymek", "Wojtek"),
                 individual = TRUE,
                 status = c("primary", "secondary" , "success")
               )
             ),
             fluidRow(
               column(width = 6,
                      selectInput(
                        "typ", 
                        "Select category:",
                        choices = c("University",
                                    "Home",
                                    "Entertainment",
                                    "Other")
                      )
               ),
               column(width = 6,
                      dateInput(
                        "tydzien",
                        "Choose week:",
                        min = "2022-12-12",
                        max = "2023-01-24",
                        value = "2022-12-13",
                        format = "yyyy-mm-dd",
                        startview = "month",
                        weekstart = 1,
                        language = "en"
                      )
               ),
               
             )
           ),
           br(),
           "The presented graph shows the duration of activities at home, university and entertainment.
           Thanks to the colored lines user can match each line to a particular person.
           After choosing a date from the calendar, the graph always presents the week with the chosen day.
           Each week presented by the graph starts on Monday.
           While checking the data it turned out that Wojtek and Tymek usually 
           spend more time at university than Czarek. 
           It turned out that only Czarek and Wojtek have spent New Year’s Eve partying.
           However, on a daily basis no one can compete with Tymek when it comes to entertainment time.",
           style = "text-align: justify;"
    ),
    column(width = 7,
           shinycssloaders::withSpinner(
             plotOutput("linePlot", height = "500px"),
             color = "#2fa4e7")),
    column(width = 1)
  )
)

trans_ui <- fluidPage(
  
  
  fluidRow(
    column(width = 1),
    column(width = 11, 
           titlePanel("Transport"))),
  fluidRow(
    column(width = 1),
    column(width = 11, 
           "Sum of kilometers we have travelled during the weekday.")),
  br(),
  
  fluidRow(
    column(width = 1),
    column(width = 5,
           wellPanel(
             fluidRow(
               column(width = 12,
                      "Select modes of transport:")),
             fluidRow(
               column(width = 3,
                      checkboxGroupInput("transport1",
                                         "",
                                         choices = c("walk", "bicycle", "car", "tram"),
                                         selected = "walk"),),
               column(width = 3,
                      checkboxGroupInput("transport2",
                                         "",
                                         choices = c("train", "bus", "subway", "plane"),
                                         selected = ""),)
             ),
             dateRangeInput("date_range_trans", "Select date range:",
                            start = "2022-12-07", end = "2023-01-24",
                            min = "2022-12-07", max = "2023-01-24",
                            format = "yyyy-mm-dd", startview = "month",
                            autoclose = TRUE),
           )),
    column(width = 5,
           "In this interactive plot, we can easily compare how we move around
           on a daily basis or during X-mas holidays.
           There are some obvious categories related to public transport,
           walking and driving a car, but thanks to Wojtek we have also
           plane and bicycle which might not be the most popular modes of transport during winter.
           Analysing time before Christmas break we can see that Wojtek and Tymek
           usually travel using public transport like trams, buses or subway, while
           Czarek, in contrast to them, travels mostly by car and train.
           Everybody takes a walk sometimes, especially Wojtek, but these aren't
           long distances anyway. During holidays Wojtek made more than 2000 
           kilometers in total, mostly by train and plane. No one was even close to him, as
           Czarek made no more than 700km, and Tymek significantly less. 
           It might be noted that nobody was travelling by subway at that time.
           Why Czarek haven't travelled once in january during weekend? That's because he 
           wasn't fully healthy. For more insides try it yourself!",
           style = "text-align: justify;"),
    column(width = 1)
  ),
  fluidRow(
    column(width = 1),
    column(width = 10,
           shinycssloaders::withSpinner(
             plotOutput("barPlot"),
             color = "#2fa4e7")),
    column(width = 1)
  ),
)

user_ui <- fluidPage(
  
  fluidRow(
    column(width = 1),
    column(width = 11, 
           titlePanel("Create your own map!"))),
  
  
  fluidRow(
    column(width = 1),
    column(width = 11, 
           "Upload JSON file with your own data from google maps and create map of places you have visited.")),
  br(),
  fluidRow(
    column(width = 1),
    column(width = 3,
           wellPanel(
             "How to download data:",
             br(),
             "1. Go to this website:",
             
             
             a('Google Takeout', href='https://takeout.google.com/takeout/custom/local_actions,location_history,maps,mymaps?dnm=false&continue=https://myaccount.google.com/yourdata/maps&hl=en&utm_source=privacy-advisor-maps')
             ,
             br(),
             "2. Select 'Location History' and click next.",
             br(),
             "3. Choose 'Send download link via email' and create export.",
             br(),
             "6. Go to your mailbox and download zip file.",
             br(),
             "7. Extract files and find 'Semantic Location History'.",
             br(),
             "8. Choose which month you want to visualize. File should look like this: '2023_JANUARY.json'",
             br(),
             "9. Upload the file here by clicking 'Browse'.",
             br(),
             "10. You are all set!",
             br(),
             br(),
           fileInput("file", "Upload your file here:", buttonLabel = "Browse", accept = ".json"))),
    column(width = 7,
           shinycssloaders::withSpinner(
             leafletOutput("userMap", height = "550px"),
             color = "#2fa4e7"))
  )
  
  
)


server <- function(input, output) {
  
  # pliki
  process_file <- reactive({
    print("try")
    req(input$file)
    user_data_raw <- fromJSON(file=input$file$datapath)
    user_data <- user_data_raw$timelineObjects
    print(user_data[[1]])
    user_df <- data.frame(name = character(0), address = character(0), latitude = numeric(0), longitude = numeric(0))
    for (i in user_data) {
      row <- c(0, 0, 0, 0)
      row[1] <- ifelse(length(i$placeVisit$location$name) > 0,i$placeVisit$location$name, "")
      row[2] <- ifelse(length(i$placeVisit$location$address) > 0, i$placeVisit$location$address, "")
      row[3] <- ifelse(length(i$placeVisit$location$latitudeE7) > 0, i$placeVisit$location$latitudeE7, "")
      row[4] <- ifelse(length(i$placeVisit$location$longitudeE7) > 0, i$placeVisit$location$longitudeE7, "")
      user_df <- rbind(user_df, row)
    }
    colnames(user_df) <- c("name", "address", "latitude", "longitude")
    
    file_df <- user_df %>%
      filter(address != "") %>%
      mutate(lat = as.numeric(latitude) / 10000000) %>%
      mutate(lng = as.numeric(longitude) / 10000000)
    print(file_df)
    return(file_df)
  })
  
  output$userMap <- renderLeaflet({
    req(input$file)
    file_df <- process_file()
    file_df %>%
      group_by(name, address, lat, lng) %>%
      summarise(number = n()) %>%
      mutate(number = ifelse(number > 18, 20, number + 2)) %>%
      leaflet() %>%
      addTiles() %>% 
      addCircleMarkers(lng = ~lng,
                       lat = ~lat,
                       radius = ~number,
                       popup = ~name,
                       color = ~"#4285F4",
                       opacity = 0.7,
                       fillOpacity = 0.3
      )
  })
  
  #Strona główna
  shinyjs::onclick("image_tymek", function(){
    window.open("https://google.com", "_blank")
  })
  
  #Czesc Czarek
  output$barPlot <- renderPlot({
    trans_df$weekDay <- factor(trans_df$weekDay, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    trans_df %>% 
      filter(date >= input$date_range_trans[1] & date <= input$date_range_trans[2]) %>%
      filter(type %in% cbind(input$transport1, input$transport2)) %>%
      mutate(distance = distance/1000) %>%
      group_by(weekDay, name) %>% 
      summarise(sumKilo = sum(distance)) %>% 
      ggplot(aes(x = weekDay, y = sumKilo, fill = name)) +
      geom_bar(position="dodge", stat = "identity") +
      scale_fill_manual(
        values = c(Czarek = "#4285F4", Wojtek = "#0F9D58", Tymek = "#F4B400")
      ) +
      labs(title = "", y = "Kilometers", x = "", fill = "person", ) +
      theme_minimal() +
      theme(text = element_text(color = "#4f4e4d"),
        axis.text=element_text(size=13,face="bold"), legend.title = element_blank(), axis.title=element_text(size=13,face="bold"),
            legend.text = element_text(size=11,face="bold"))
  })
  
  #Część Tymek
  output$placeMap <- renderLeaflet({
    map_df %>% 
      filter(date >= input$date_range[1] & date <= input$date_range[2]) %>%
      filter(person %in% input$persons) %>%
      group_by(placeVisit_location_name, placeVisit_location_address,
               lat, lng, color) %>%
      summarise(number = n()) %>%
      mutate(number = ifelse(number > 18, 20, number + 2)) %>%
      leaflet() %>%
      addTiles() %>% 
      addCircleMarkers(lng = ~lng,
                       lat = ~lat,
                       radius = ~number,
                       popup = ~placeVisit_location_name,
                       color = ~color,
                       opacity = 0.7,
                       fillOpacity = 0.3
                       )
  })
  
  #Część Wojtek z
  output$linePlot <- renderPlot({
    filtr <- case_when(
      input$typ == "University" ~"uni",
      input$typ == "Home" ~"home",
      input$typ == "Entertainment" ~"fun",
      input$typ == "Other" ~"other"
    )
    
    osoby <- substr(input$osoby, 1, 1)
    
    tydzien <- format(strptime(input$tydzien, '%Y-%m-%d'), format="%Y-%U")
    
    graphData <- filterData %>% 
      filter(week == tydzien & type == filtr & person %in% osoby) %>% 
      select(week, weekday, minutes, person) %>% 
      group_by(weekday, person) %>% 
      summarise(hours = sum(minutes)/60) %>% 
      mutate(hours = if_else(hours>24, 24, hours)) %>% 
      data.frame()
    
    graphData <- graphData %>% 
      full_join(baseFrame, by = c("weekday", "person")) %>% 
      mutate(hours = coalesce(hours.x, hours.y)) %>% 
      select(-c(hours.x, hours.y)) %>% 
      filter(person %in% osoby) %>% 
      mutate(weekdayN=case_when(
        weekday==1~"Monday",
        weekday==2~"Tuesday",
        weekday==3~"Wednesday",
        weekday==4~"Thursday",
        weekday==5~"Friday",
        weekday==6~"Saturday",
        weekday==7~"Sunday"
      ))
    
    
    
    
    plot <- ggplot(data = graphData, aes(x=weekday, y=hours, group = person, color = person)) +
      geom_line() + 
      scale_color_manual(
        values = c("C" = "#4285F4", "W" = "#0F9D58", "T" = "#F4B400")
      ) +
      geom_point() +
      theme_minimal()+
      scale_x_continuous("", labels = graphData$weekdayN, breaks = graphData$weekday) +
      labs(y = "Hours") +
      theme(text = element_text(color = "#4f4e4d"),
        legend.title = element_blank(),
            legend.position = "none", 
            axis.text=element_text(size=13,face="bold"), 
            axis.title=element_text(size=13,face="bold"))
    plot
    
  })
  
}

app_ui <- navbarPage(
  title = '',
  tabPanel('About', info_ui, icon = icon(name="glyphicon glyphicon-home",lib="glyphicon")),
  tabPanel('Map', map_ui, icon = icon(name="glyphicon glyphicon-map-marker",lib="glyphicon")),
  tabPanel('Time', time_ui, icon = icon(name="glyphicon glyphicon-time",lib="glyphicon")),
  tabPanel('Transport', trans_ui, icon = icon(name="glyphicon glyphicon-road",lib="glyphicon")),
  tabPanel('User', user_ui, icon = icon(name="glyphicon glyphicon-user",lib='glyphicon')),
  theme = bslib::bs_theme(bootswatch = "cerulean",
                          primary = '#4285F4',
                          secondary = '#F4B400',
                          success = '#0F9D58',
                          ),
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  <a class='text-dark' href='https://github.com/MI2-Education/2023Z-DataVisualizationTechniques/tree/main/projects/project2'  style='text-decoration: none'>Data Visualization Techniques 2023</a> 
                </p>
                </footer>
                "),
  header = tags$head(tags$link(rel = "stylesheet",
                               href = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css"))
)

shinyApp(ui = app_ui, server = server)
