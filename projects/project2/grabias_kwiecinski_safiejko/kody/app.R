library(dplyr)
library(ggplot2)
library(shinydashboard)
library(shiny)
library(shinycssloaders)
library(plotly)
library(lubridate)
library(spotifyr)
library(tidyr)
library(shinyWidgets)
library(shinyjs)

# setwd("../")
# jak coś to ten kod będzie moze kiedyś przepisany lepiej - teraz niestety nie jest najpiekniejszy

source("scripts/topki.R")
source("scripts/styles.R")
source("scripts/plots.R")
source("scripts/scripts_tymek.R")
source("scripts/MergeTimestamps.R")
source("scripts/descriptions.R")

options(scipen = 10000)


START_DATE <- as.Date("2022-12-01")
END_DATE <- as.Date("2023-01-21")

PERSON_CHOICES <- c("Natalia", "Wojtek", "Tymek")
GIT_LINKS <- list("Natalia" = "https://github.com/ssafiejko", "Wojtek"="https://github.com/WojtekGrbs", "Tymek"="https://github.com/Fersoil")
PERSON <- "Natalia"

# czytamy dane do spotify oraz komputera
df_computer <- read_kompik_data(PERSON, sufix = "_computer")
df_spotify <- read_spotify_data(PERSON, sufix = "_spotify")
df_merged <- read_merged_data(PERSON)

#zmien nazwe pliku byczku


Sys.setenv(SPOTIFY_CLIENT_ID = 'b4a4fa3315e44ebda8701c684cc5f43e')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '605e78b16bee4a499dbce461cdf57e6d')

access_token <- get_spotify_access_token()



#topka artystĂłw- fotki




ui <- dashboardPage(
  title = "Our analysis",
  skin = "green",
  dashboardHeader(title = span("Spotify + computer" #style="font-family: Gotham;"
  )),
  
  dashboardSidebar(
    width = "280px",
    sidebarMenu(
      id = "tabs",
      h3("Choose analysis", style = "margin-left: 3px;"),
      
      menuItem("  Spotify", tabName = "spotify", icon = icon('music')),
      menuItem("  Computer", tabName = "kompik", icon = icon('laptop')),
      menuItem(
        "  Merged data",
        tabName = "merge",
        icon = icon('chart-line')
      ),
      menuItem(
        "  Summary & About",
        tabName = "my",
        icon = icon('info')
      ),
      
      h3("Choose person", style = "margin-left: 3px;"),
      prettyRadioButtons(
        inputId = "user",
        label = NULL,
        thick = TRUE,
        fill = T,
        shape = "square",
        
        choices = PERSON_CHOICES,
        selected = PERSON,
        animation = "pulse",
        status = "default"
      ),
      uiOutput("our_image"),
      
      div(p(""), style="width:50px; height:1000px;
        margin:5px; 
    background:transparent;"),
      menuItem("", tabName = "pudzian")
      
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML((
      HTML_styles
    )))),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tabItems(
      tabItem(
        tabName = "spotify",
        fluidPage(
          uiOutput("spotify_title"),
          useShinyjs(),
          fluidRow(uiOutput("artist_boxes_title")),
          
          fluidRow(uiOutput("artist_boxes")),
          
          fluidRow(uiOutput("genre_boxes_title")),
          
          
          fluidRow(uiOutput("genres_boxes")),
          fluidRow(#h3("Listening to music during the day", class = "text-fav"),
            plotOutput("spotify_bumps_1"),
            p(" ")),
          fluidRow(#h3("Music energy and time of the day ", class = "text-fav"),
            plotOutput("spotify_bumps_energy"),
            p(" ")),
          fluidRow(#h3("Number of listened songs per day", class = "text-fav"),
            plotOutput("spotify_heatmap"),
            p(" ")),
          fluidRow(h3(""))
          
          #fluidRow(plotOutput("test_szerokosci_okna")),
          
          #fluidRow(uiOutput("test"))
        )
      ),
      
      tabItem(
        tabName = "kompik",
        fluidPage(
          uiOutput("kompik_title"),
          fluidRow(uiOutput("app_boxes_title")),
          fluidRow(uiOutput("apps_boxes"), ),
          fluidRow(#h3("Activity during the day", class = "text-fav"),
            plotOutput("computer_bumps"),
            p(" ")),
          fluidRow(#h3("Frequency of changing pages", class = "text-fav"),
            plotOutput("computer_heatmap")),
          fluidRow(h3(""))
        )
      ),
      
      tabItem(
        tabName = "merge",
        fluidPage(
          uiOutput("merged_title"),
          fluidRow(#h3("Activity on Spotify and on the computer during the day", class = "text-fav"),
            plotOutput("merge_bumps"),
            p(" ")),
          fluidRow(#h3("Type of activity and kind of music", class = "text-fav"),
            plotOutput("alluvial"),
            p(" ")),
          fluidRow(#h3("Frequency of changing pages and music energy",class = "text-fav"),
            plotOutput("merge_energy")),
          fluidRow(h3(""))
        )
      ),
      
      tabItem(
        tabName="my",
        fluidPage(
          
          fluidRow(h3("This is us :)", class = "text-fav")),
          fluidRow(uiOutput("us")),
          fluidRow(h3("About the project", class = "text-fav")),
          fluidRow(a(href = "https://github.com/MI2-Education/2023Z-DataVisualizationTechniques/tree/main/projects/project2", target="_blank", 
                     box(div(p("The aim of this project was to show relation between kind of music that we listened during December and activity on the computer that we measured from mid-December to mid-January."),
                             p("This simple dashboard was created as a second 
                            project of the subject Data Visualization Techniques at second year of Data Science course
                            at the Faculty of Mathematics and Information Science at the Warsaw University of Technology.
                       
                       "), style= "margin: 30px;"), img(src="mini_eng.png", height=200), class="description-box", width=12))),
          fluidRow(h3(""))
        )
      ),
      
      tabItem(
        tabName="pudzian",
        fluidPage(
          fluidRow(h3("")),
          fluidRow(a(href = "https://www.youtube.com/watch?v=BP1UTQXoAsk", target="_blank",
                     div(style="display: flex;
                              flex-direction: start;
                              height: 100%;
                              align-items: center;
                         padding:50px;",
                         img(src = "pudzian.jpg", 
                             style = "width: 280px; height: 280px; vertical-align: middle;
                                  border-radius: 50%;"),
                         h1("Pudzian", class = "text-fav")))),
          fluidRow(h3("")),
          fluidRow(box(class="descritption-box",width=12, "Mariusz Zbigniew Pudzianowski ps. Pudzian, Dominator, Pyton, Pudzilla (ur. 7 lutego 1977 w Białej Rawskiej) – polski zawodnik mieszanych sztuk walki (MMA), wcześniej utytułowany strongman i rugbysta. Obecny nr 5 rankingu KSW w wadze ciężkiej.

Ośmiokrotny Mistrz Polski Strongman. Sześciokrotny Mistrz Europy Strongman w latach 2002–2004 i 2007–2009. Pięciokrotny Mistrz Świata Strongman w latach 2002, 2003, 2005, 2007 i 2008. Trzykrotny drużynowy Mistrz Świata Par Strongman w latach 2003–2005. Mistrz Super Serii w sezonach 2003/2004, 2005–2007. Zwycięzca i finalista wielu innych zawodów siłaczy.")),
          fluidRow(h3(""))
        ))
      
      
      
    ),
    
    div(
      div(
        class = "left-caption",
        #uiOutput("our_image"),
        uiOutput("left_caption"),
        uiOutput("heart"),
      ),
      div(
        class = "center-lower-panel",
        div(
          icon(id = "backward", "backward", style = "z-index:100; cursor:pointer; margin: auto;"),
          icon("pause", style = "margin: auto;"),
          icon("forward",  id = "forward", style = "z-index:100; cursor:pointer; margin: auto;"),
          class = "icons-box"
        ),
        div(
          class = "suwak",
          sliderInput(
            "Dates",
            "",
            min = START_DATE,
            max = END_DATE,
            value = c(START_DATE, END_DATE),
            timeFormat = "%d.%m"
          )
        )
      ),
      div(
        class = "right-caption",
        a(href = "https://github.com/Fersoil/SomethingAboutUs", target="_blank",
          img(src="github.png", style="width: 40px;"),
          style = "z-index:100; cursor:pointer; margin: auto;"),
        #uiOutput("right_caption")
      ),
      class = "suwak-container"
    ),
    a(href="#shiny-tab-pudzian", 
      div(p("      "), id="pudzian-info", style="cursor:pointer; width:50px; height:50px;
        margin:5px; z-index:10000; position:fixed; bottom:20px; left:200px;
    background:transparent;"))
    
    # div(id = "loading",
    #     uiOutput("loading_output"),
    #     style = " position: fixed; left:50%; bottom:50%; z-index:300;")
  )
)

server = function(input, output, session) {
  
  ######
  # data manipulation
  
  width <- reactive({
    session$clientData[['output_test_szerokosci_okna_width']]
    
  })
  
  
  # read data
  df_computer_person <- reactive({
    read_kompik_data(input$user, sufix = "_computer")
  })
  
  df_spotify_person <- reactive({
    read_spotify_data(input$user, sufix = "_spotify")
  })
  
  
  df_merged_person <- reactive({
    read_merged_data(input$user)
  })
  
  # date filtering
  df_computer_date_filtered <- reactive({
    df_computer_person() %>% filter(date %within% interval(input$Dates[1], input$Dates[2]))
  })
  
  
  df_spotify_date_filtered <- reactive({
    df_spotify_person() %>% filter(date %within% interval(input$Dates[1], input$Dates[2]))
  })
  
  
  df_merged_date_filtered <- reactive({
    df_merged_person() %>% filter(date %within% interval(input$Dates[1], input$Dates[2]))
  })
  
  
  
  #####
  # animations
  
  forward_date <- function() {
    val = input$Dates
    val[2] <- val[2] + 1
    updateSliderInput(
      session,
      "Dates",
      value = val,
      min = START_DATE,
      max = END_DATE,
      timeFormat = "%d.%m"
    )
    
  }
  backward_date <- function() {
    val = input$Dates
    val[1] <- val[1] - 1
    print(input$Dates)
    updateSliderInput(
      session,
      "Dates",
      value = val,
      min = START_DATE,
      max = END_DATE,
      timeFormat = "%d.%m"
    )
    print(input$Dates)
  }
  
  
  change_tab <- function() {
    
    updateTabItems(session, "tabs", "pudzian")
  }
  
  change_person <- function(person) {
    updateCheckboxInput(session, inputId = "user", value=person)
  }
  
  
  observe({
    onclick("forward", forward_date)
    onclick("backward", backward_date)
    onclick("pudzian-info", change_tab)
    onclick("Tymek-box", change_person(PERSON_CHOICES[3]))
    onclick("Natalia-box", change_person(PERSON_CHOICES[1]))
    onclick("Wojtek-box", change_person(PERSON_CHOICES[2]))
  })
  
  
  
  ######
  
  output$spotify_title <- renderUI({
    div(h1(paste0("Analysis of listening to music by ", input$user, " on Spotify"), class="text-fav"),
        h3(paste0("from ", input$Dates[1], " to ", input$Dates[2]), class="text-fav", style="margin-top:-5px;"),
        
        div(HTML(SPOTIFY_DESCRIPTIONS[[input$user]]), style="margin-left:30px;"), p(""), style="margin-left: -15px; margin-bottom:10px; min-height: 180px;")
  })
  
  
  output$kompik_title <- renderUI({
    div(h1(paste0("Analysis of ", input$user, "'s computer usage"), class="text-fav"),
        h3(paste0("from ", input$Dates[1], " to ", input$Dates[2],"\n"), class="text-fav", style="margin-top:-5px;"),
        
        div(HTML(COMPUTER_DESCRIPTIONS[[input$user]]), style="margin-left:30px;"),
        p(""), style="margin-left: -15px; margin-bottom:5px; min-height: 180px;")
  })
  
  
  output$merged_title <- renderUI({
    div(h1(paste0("Analysis of the relationship between computer use and music listening for ", input$user), class="text-fav"),
        h3(paste0("from ", input$Dates[1], " to ", input$Dates[2],"\n"), class="text-fav", style="margin-top:-5px;"),
        
        div(HTML(MERGED_DESCRIPTIONS[[input$user]]), style="margin-left:30px;"),
        p(""), style="margin-left: -15px margin-bottom:5px; min-height: 180px;")
  })
  
  
  #####
  
  
  # output$loading_output <- renderUI({
  #   p()
  # })
  
  
  #####
  # dolny pasek
  
  output$left_caption <- renderUI({
    h4(paste0(input$user), style = "height: 100%; margin: 5px;")
    
  })
  
  
  output$heart <- renderUI({
    tags$a(style = "z-index:100; cursor:pointer; margin-top: 5px; margin-left: 5px;",
           href = GIT_LINKS[[input$user]],
           target="_blank",
           icon(id = "heart", "heart")
    ) 
  })
  
  output$right_caption <- renderUI({
    h3(paste0(input$tabs), style = "")
  })
  
  
  output$our_image <- renderUI({
    img(
      src = paste0(input$user, ".png"),
      class = "our-image",
      style = "width: 280px; height: 280px; vertical-align: middle;
    position: fixed; bottom: 90px;"
    )
  })
  
  
  
  
  
  #####
  # boxes - top artists, etc.
  artist_number <- 10
  
  id_artist_list <- paste("artist", 1:artist_number, sep = "")
  
  
  artist_number <- reactive({
    #min(floor(width() / 200), 5)
    5
  })
  
  
  v <- reactive({
    du <- get_top_artists_names(df_spotify_date_filtered())
    d <- get_top_artists_img(df_spotify_date_filtered())
    v <- list()
    for (i in 1:artist_number()) {
      image_uri <-  d[[i]]
      artist_name <- du[[1]][i]
      artist_link <- du[[2]][i]
      
      v[[i]] <- 
        a(href = artist_link, target="_blank",
          box(
            class = "box1",
            div(
              class = "image-container",
              img(src = image_uri, class = "top-image artist-image")
            ),
            h3(class = "top-text", artist_name, style = "font-weight:bold"),
            class = "top-box artist-box"
          ))
    }
    v
  })
  
  output$artist_boxes <- renderUI(v())
  
  
  
  
  # tutaj genres
  g <- reactive({
    topgenresnames <- get_top_genres(df_spotify_date_filtered())
    topgenresimages <- get_genres_img(df_spotify_date_filtered())
    g <- list()
    for (i in 1:artist_number()) {
      image_uri <-  topgenresimages[[i]]
      genre_name <-  topgenresnames[[1]][i]
      
      g[[i]] <-
        a(href = paste0("https://open.spotify.com/search/", genre_name), target="_blank",
          box(
            class = "box1",
            div(
              class = "image-container",
              img(src = image_uri, class = "top-image genre-image")
            ),
            h3(class = "top-text", genre_name, style = "font-weight:bold"),
            class = "top-box genre-box"
          )
        )
    }
    g
  })
  
  output$genres_boxes <- renderUI(g())
  
  #topapps
  topapps <- reactive({
    top_apps <- get_top_apps(df_computer_date_filtered())
    
    topapps <- list()
    for (i in 1:artist_number()) {
      topapps[[i]] = a( target="_blank",
                        href = paste0("https://google.com/search?q=", top_apps$app[[i]]),
                        box(
                          class = "box1",
                          img(
                            class = "top-image app-image",
                            src = paste0(tolower(top_apps$app[[i]]), ".png")
                          ),
                          h3(class = "top-text", top_apps$app[i], style = "font-weight:bold"),
                          class = "top-box apps-box"
                        )
      )
    }
    topapps
  })
  
  output$apps_boxes <- renderUI(topapps())
  
  us_boxes <- list()
  
  us_boxes[[1]]=a( id="Natalia-box", style="cursor:pointer;",#href = GIT_LINKS[["Natalia"]], target="_blank",
                   box(
                     width=4,
                     class= "box11",
                     img(
                       class= "us-image",
                       src= "Natalia.png"
                     ),
                     h3(class="top-text", "Natalia", style= "font-weight:bold"),
                     div(paste("Calm and quiet person. Usually she listens to slow music and uses computer to study and (sometimes) play computer games."),
                         br(),
                         br(),
                         br(),
                         HTML("Total time spent in front of computer screen during December and January: <b>244.74h</b>") ,
                         br(),
                         HTML("Total listening time during December: <b>84.02h</b> "),
                         class="description", style="margin-top: -30px; margin-left: 20px; margin-right: 20px; margin-bottom:20px;"),
                     class="us-box"
                   ))
  
  us_boxes[[3]]=a( id="Tymek-box", style="cursor:pointer;", #href = GIT_LINKS[["Tymek"]], target="_blank",
                   box(
                     width=4,
                     class= "box11",
                     img(
                       class= "us-image",
                       src= "Tymek.png",
                     ),
                     h3(class="top-text", "Tymek", style= "font-weight:bold"),
                     div(paste("Tymek likes more energetic and powerful music. Most of the time he uses computer to study and watch videos with Robert Makłowicz."),
                         br(),
                         br(),
                         HTML("Total time spent in front of computer screen during December and January: <b>178.20h</b>") ,
                         br(),
                         HTML("Total listening time during December: <b>81.86h</b> "),
                         class="description", style="margin-top: -30px; margin-left: 20px; margin-right: 20px; margin-bottom:20px;"),
                     class="us-box"
                   ))
  
  us_boxes[[2]]=a( id="Wojtek-box", style="cursor:pointer;",#href = GIT_LINKS[["Wojtek"]], target="_blank",
                   box(
                     width=4,
                     class= "box11",
                     img(
                       class= "us-image",
                       src= "Wojtek.png"
                     ),
                     h3(class="top-text", "Wojtek", style= "font-weight:bold"),
                     div(paste("Wojtek is definitely a night owl with wide range of music taste. He uses computer to study, but also to play games."),
                         br(),
                         br(),
                         br(),
                         HTML(" Total time spent in front of computer screen during December and January: <b>300.89h</b>") ,
                         br(),
                         HTML("Total listening time during December: <b>130.07h</b>")
                         , class="description", style="margin-top: -30px; margin-left: 20px; margin-right: 20px; margin-bottom:20px;"
                     ),
                     class="us-box"
                   ))
  output$us <- renderUI(us_boxes)
  
  #####
  # titles
  
  output$artist_boxes_title <- renderUI({
    h3(paste0(input$user, "'s favourite artists"), class = "text-fav green-text-fav")
  })
  
  output$genre_boxes_title <- renderUI({
    h3(paste0(input$user, "'s favourite genres"), class = "text-fav green-text-fav")
  })
  
  output$app_boxes_title <- renderUI({
    h3(paste0(input$user, "'s most used apps"), class = "text-fav green-text-fav")
  })
  
  
  
  
  
  # plots for spotify
  output$spotify_bumps_1 <- renderPlot({
    pagorki_fill(df_spotify_date_filtered()) +
      labs(title = "Listening to music during the day") +
      xlab("hour") +
      ylab("factor of listened songs") +
      theme(legend.position = "none")
    
  })
  
  output$spotify_bumps_energy <- renderPlot({
    # filtrowanie energii
    
    
    pagorki_color(df_spotify_date_filtered(), fill_column = "energy_level") +
      labs(title = "Energy level of listened music during the day") +
      xlab("hour") +
      ylab("factor of listened songs") +
      guides(color = guide_legend(title = "energy level"))
    
    
  })
  
  output$spotify_heatmap <- renderPlot({
    generate_githeatmap(
      df_spotify_date_filtered(),
      title = "Number of listened songs per day",
      
      start_date = input$Dates[1], #START_DATE, 
      end_date = input$Dates[2], #END_DATE, 
      type = "spotify",
      legend_title = "number of listened songs per day"
    )
  })
  
  
  # plots for computer
  
  
  output$computer_bumps <- renderPlot({
    pagorki_color(df_computer_date_filtered()) +
      labs(title = "Frequency of window changes during the day") +
      xlab("hour") +
      ylab("factor of changed windows") +
      guides(color = guide_legend(title = "app type"))
  })
  
  output$computer_heatmap <- renderPlot({
    generate_githeatmap(
      df_computer_date_filtered(),
      title = "Frequency of changing pages per day",
      start_date = input$Dates[1],
      end_date = input$Dates[2],
      type = "computer",
      legend_title = "number of window changes per day"
    )
  })
  
  
  output$merge_bumps <- renderPlot({
    df <- df_computer_date_filtered() %>%
      select(startTime, duration, app, date) %>%
      rename(name = app) %>%
      mutate(category = "computer")
    
    df <- df_spotify_date_filtered() %>%
      select(startTime, duration, trackName, date) %>%
      rename(name = trackName) %>%
      mutate(category = "spotify") %>% bind_rows(df)
    
    pagorki_fill(df,
                 fill_column = "category",
                 weight = FALSE) +
      aes(fill = category) +
      scale_fill_manual(values = c("spotify" = '#1DB954',
                                   "computer" = "#c30010")) +
      labs(title = "Activity on Spotify and on the computer \n during the day")
    
  })
  
  
  output$alluvial <- renderPlot({
    alluvial_merged(df_merged_date_filtered())  +
      labs(title="Seconds the computer and Spotify were used", 
           subtitle="broken down by top artist, music pace and category of apps used")
  })
  
  output$merge_energy <- renderPlot({
    energy_plot(df_merged_date_filtered())
  })
  
  
  #### test
  output$test <- renderText({
    width()
  })
  
}


shinyApp(ui, server)
