library(shiny)
library(plotly)
library(lubridate)
library(ggplot2)
library(dplyr)
library(bslib)
library(shinyWidgets)
library(shinycssloaders)

source("code.R")

t <- list(
  family = "Rockwell",
  size = 16,
  color = "#f7fafc")

ui <- fluidPage(
  
    theme = bs_theme(bootswatch = "cyborg"),   
    setBackgroundColor("#navbarColor02"),
    tags$head(tags$link(rel = "stylesheet",type="text/css",href="bootstrap.min.css"),
              tags$style('* .selectize-input { font-size: 18px; line-height: 28px; color: black} .selectize-dropdown { font-size: 20px; line-height: 17px; color: #f7fafc} * {font-family: "Rockwell"; font-size: 20px;}
                         #text {text-align:center; display: block; padding:75px; }')
              ),
    
    titlePanel(h1("Phone Usage Analysis", align = "center")),
    
    fluidRow(
      
      column(4, 
             span(
        dateRangeInput(
        "date",
        "Choose date range:",
        min = min(df$Date),
        max = max(df$Date),
        start = as.Date("2022-12-20"), 
        end = as.Date("2023-01-04"),
        weekstart = 1
            ),
        style='font-size: 20px; color: #f7fafc'
        ),
        offset = 1),
      
      column(4,
             span(selectInput(
               "appl",
               "Choose application:",
               choices = unique(df$App),                  
               selected = NULL
             ),
             style='font-family: "Rockwell"; font-size: 20px; color: #f7fafc')
             ),
      column(3,
             span(selectInput(
               "dev",
               "Choose device:",
               choices = list(
                 "Samsung",
                 "Huawei",
                 "All" = "All"
               ),
               selected = "All"
             ),
             style='font-family: "Rockwell"; font-size: 20px; color: #f7fafc'
             )
      )
    ),
    
    fluidRow(
      
      column(5, 
             plotlyOutput("plot1") %>% withSpinner(type = 4, size = 2, color = "#0c32f2"),
             offset = 1),
      column(5,
             plotlyOutput("plot2")  %>% withSpinner(type = 7, size = 2, color = "#0c32f2"))
      
    ),
    fluidRow(
      
      column(5,
             plotlyOutput("plot3")  %>% withSpinner(type = 6, size = 2, color = "#0c32f2"),
             offset = 1),
      column(5,
             span(htmlOutput("text")  %>% withSpinner(type = 5, size = 2, color = "#0c32f2"),
                  style = "color: #f7fafc"))
      
    ),
    tags$footer(HTML("<footer><small><b>&copy; Grinchik Gleb Kyryk Viktoriia 2023</b></small></footer>"), 
                align="right", 
                style="position:relative; bottom:0; width:97%; height:20px; color: white; padding: 0px; background-color: transparent; font-size: 16px")
)


server <- function(input, output) {

    output$plot1 <- renderPlotly({
    col1 = "#d7e5f7" 
    col2 = "#0c32f2"
    
    if (input$dev == "All") {
      
      df %>% 
        filter(Date <= as.POSIXct(input$date[2]) & Date >= as.POSIXct(input$date[1])) %>%
        mutate(wd = wday(Date, label = TRUE, week_start = 1, locale="English_United States"), w = isoweek(Date)) %>% 
        mutate(w = ifelse(w < 15, w + 52, w)) %>%
        group_by(wd, Date, w) %>% 
        summarise(time = sum(UsageTime)) -> data
      
      df %>% 
        filter(Date <= as.POSIXct(input$date[2]) & Date >= as.POSIXct(input$date[1])) %>%
        group_by(App, Date) %>%
        summarise(time = sum(UsageTime)) -> dfa
      
      plot_ly(data,
              x = ~wd,
              y = ~as.factor(w),
              z = ~time/3600,
              type = "heatmap",
              colors = colorRamp(c(col1, col2)),
              hoverinfo = 'text',
              text = ~paste('</br> Date: ', Date,
                            '</br> Time: ', format(time/3600, digits = 3))
              ) %>%
        layout(
          yaxis = list(autorange = "reversed", title = "Week", showgrid = FALSE, 
                       ticktext = ~ifelse(w > 52, w - 52, w), tickvals = ~as.factor(w)),
          xaxis = list(title = "", showgrid = FALSE),
          paper_bgcolor='rgba(0,0,0,0)',
          plot_bgcolor='rgba(0,0,0,0)',
          title = "Phone Calendar",
          font = t,
          margin = list(t = 50),
          hoverlabel = list(bgcolor = col2)
        ) %>% colorbar(title = "Usage Time (hrs)", font = t)
      
    } else {
      
      df %>% 
        filter(Date <= as.POSIXct(input$date[2]) & Date >= as.POSIXct(input$date[1]), Device == input$dev) %>%
        mutate(wd = wday(Date, label = TRUE, week_start = 1, locale="English_United States"), w = isoweek(Date)) %>% 
        mutate(w = ifelse(w < 15, w + 52, w)) %>%
        group_by(wd, Date, w) %>% 
        summarise(time = sum(UsageTime)) -> data
      
      plot_ly(data,
              x = ~wd,
              y = ~as.factor(w),
              z = ~time/3600,
              type = "heatmap",
              colors = colorRamp(c(col1, col2)),
              hoverinfo = 'text',
              text = ~paste('</br> Date: ', Date,
                            '</br> Time: ', format(time/3600, digits = 3))
      ) %>% 
        layout(
          yaxis = list(autorange = "reversed", showgrid = FALSE, title = "Week",
                       ticktext = ~ifelse(w > 52, w - 52, w), tickvals = ~as.factor(w)),
          xaxis = list(title = "", showgrid = FALSE),
          paper_bgcolor='rgba(0,0,0,0)',
          plot_bgcolor='rgba(0,0,0,0)',
          title = "Phone Calendar",
          font = t,
          margin = list(t = 50),
          hoverlabel = list(bgcolor = col2)
        ) %>% colorbar(title = "Usage Time (hrs)", font = t)
      
    }
    
  })
  
  output$plot2 <- renderPlotly({
    
    col1 = "#13f0d2" 
    col2 = "#0c32f2"
    
  if (input$dev == "All") {
    
    plot_ly(df %>% filter(Date <= as.POSIXct(input$date[2]) & Date >= as.POSIXct(input$date[1]),
                          App == input$appl),
            type = "bar",
            y = ~UsageTime/3600,
            x = ~Date,
            color = ~Device,
            colors = c(col2, col1),
            hoverinfo = 'text',
            text = ~paste('</br> Device: ', Device,
                          '</br> Time: ', format(UsageTime/3600, digits = 1)),
            textposition = "none") %>%
      layout(
        title = paste(input$appl, "Time Statistics"),
        yaxis = list(title = "Usage Time (hrs)"),
        barmode = "stack",
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)',
        font = t,
        margin = list(t = 50)
      )
    
  } else {
    
    plot_ly(df %>% filter(Date <= as.POSIXct(input$date[2]) & Date >= as.POSIXct(input$date[1]),
                          App == input$appl, Device == input$dev),
            type = "bar",
            y = ~UsageTime/3600,
            x = ~Date,
            marker = list(color = "#0c32f2"),
            hoverinfo = 'text',
            text = ~paste('</br> Device: ', Device,
                          '</br> Time: ', format(UsageTime/3600, digits = 1)),
            textposition = "none") %>%
      layout(
        title = paste(input$appl, "Time Statistics"),
        yaxis = list(title = "Usage Time (hrs)"),
        barmode = "stack",
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)',
        font = t,
        margin = list(t = 50)
      ) 
    
  } 
    
  })
  
  output$plot3 <- renderPlotly({
    
    if (input$dev == "All") {
      
      plot_ly(df %>% filter(Date <= as.POSIXct(input$date[2]) & Date >= as.POSIXct(input$date[1])) %>%
                group_by(App) %>% summarise(time = sum(UsageTime), count = sum(UsageCount)) %>% filter(time != 0),
              y = ~time/3600,
              x = ~count,
              color = ~App,
              colors = c("Adblock Browser" = "#14F1D2",
                         "Chrome" = "#3DD7D7",
                         "Cool Reader" = "#1832B1",
                         "Game Dev Story" = "#4C7AE9",
                         "SOVA V RE" = "#8CFFFB",
                         "Telegram" = "#36B4B0",
                         "VK" = "#92F1EE",
                         "YouTube" = "#56A8F0",
                         "Yandex Music" =  "#145E9F",
                         "Main Screen One UI" = "#0E5089",
                         "Google"= "#0882ED",
                         "GuitarTuna" = "#07589E"),
              size = 15,
              hoverinfo = 'text',
              text = ~paste('</br> Application: ', App,
                            '</br> Usage Time: ', format(time/3600, digits = 3),
                            '</br> Times Used: ', count)) %>%
        layout(xaxis = list(title = "Times Used"),
               yaxis = list(title = "Usage Time (hrs)"),
               paper_bgcolor='rgba(0,0,0,0)',
               plot_bgcolor='rgba(0,0,0,0)',
               font = t,
               title = "Dependance between times and usage time of apps",
               margin = list(t = 50, b = 10))
      
    } else {
      
      plot_ly(df %>% filter(Date <= as.POSIXct(input$date[2]) & Date >= as.POSIXct(input$date[1]), Device == input$dev) %>%
                group_by(App) %>% summarise(time = sum(UsageTime), count = sum(UsageCount)) %>% filter(time != 0),
              y = ~time/3600,
              x = ~count,
              color = ~App,
              colors = c("Adblock Browser" = "#14F1D2",
                         "Chrome" = "#3DD7D7",
                         "Cool Reader" = "#1832B1",
                         "Game Dev Story" = "#4C7AE9",
                         "SOVA V RE" = "#8CFFFB",
                         "Telegram" = "#36B4B0",
                         "VK" = "#92F1EE",
                         "YouTube" = "#56A8F0",
                         "Yandex Music" =  "#145E9F",
                         "Main Screen One UI" = "#0E5089",
                         "Google"= "#0882ED",
                         "GuitarTuna" = "#07589E"),
              size = 15,
              hoverinfo = 'text',
              text = ~paste('</br> Application: ', App,
                            '</br> Usage Time: ', format(time/3600, digits = 3),
                            '</br> Times Used: ', count),
              textposition = "none") %>%
        layout(xaxis = list(title = "Times Used"),
               yaxis = list(title = "Usage Time (hrs)"),
               paper_bgcolor='rgba(0,0,0,0)',
               plot_bgcolor='rgba(0,0,0,0)',
               font = t,
               title = "Dependance between times and usage time of apps",
               margin = list(t = 50, b = 10))
      
    }
    
  })
  
  output$text = renderText({
    
    if (input$dev == "All") {
    df %>% 
      filter(Date <= as.POSIXct(input$date[2]) & Date >= as.POSIXct(input$date[1])) %>%
      mutate(wd = wday(Date, label = TRUE, week_start = 1, locale="English_United States"), w = isoweek(Date)) %>% 
      group_by(wd, Date, w) %>% 
      summarise(time = sum(UsageTime)) -> df1
    
    m <- max(df1$time)
    d <- df1$Date[which(df1$time == m)]
    t <- df1$time[which(df1$time == m)]
    day <- paste("Day when device was used mostly: ", format(d, "20%y/%m/%d"))
    hrs <- m %/% 3600
    mins <- (m - hrs * 3600) %/% 60
    secs <- (m - hrs * 3600 - mins * 60)
    time <- paste("In that day phone was used for ", hrs, "h ", mins, "m ", secs, "s.", sep="")
    
    
    df %>%
      filter(Date <= as.POSIXct(input$date[2]) & Date >= as.POSIXct(input$date[1]), Date == d) %>%
      group_by(App) %>% 
      summarise(time = sum(UsageTime)) -> df2
    
    a <- df2$App[which(df2$time == max(df2$time))]
    
    applic <- paste("Most used application in that day:", a)
    
    tim <- sum(df1$time)
    
    hrs <- tim %/% 3600
    mins <- (tim - hrs * 3600) %/% 60
    secs <- (tim - hrs * 3600 - mins * 60)
    
    sp <- paste("In choosen data range ", hrs, "h ", mins, "m ", secs, "s", " was spent using the phone", sep="")
    
    paste(sp, day, time, applic, sep = "<br>")
    } else {
      
      df %>% 
        filter(Date <= as.POSIXct(input$date[2]) & Date >= as.POSIXct(input$date[1]), Device == input$dev) %>%
        mutate(wd = wday(Date, label = TRUE, week_start = 1, locale="English_United States"), w = isoweek(Date)) %>% 
        group_by(wd, Date, w) %>% 
        summarise(time = sum(UsageTime)) -> df1
      
      m <- max(df1$time)
      d <- df1$Date[which(df1$time == m)]
      t <- df1$time[which(df1$time == m)]
      day <- paste("Day when device was used mostly: ", format(d, "20%y/%m/%d"))
      hrs <- m %/% 3600
      mins <- (m - hrs * 3600) %/% 60
      secs <- (m - hrs * 3600 - mins * 60)
      time <- paste("In that day phone was used for ", hrs, "h ", mins, "m ", secs, "s.", sep="")
      
      
      df %>%
        filter(Date <= as.POSIXct(input$date[2]) & Date >= as.POSIXct(input$date[1]), Date == d, Device == input$dev) %>%
        group_by(App) %>% 
        summarise(time = sum(UsageTime)) -> df2
      
      a <- df2$App[which(df2$time == max(df2$time))]
      
      applic <- paste("Most used application in that day:", a)
      
      tim <- sum(df1$time)
      
      hrs <- tim %/% 3600
      mins <- (tim - hrs * 3600) %/% 60
      secs <- (tim - hrs * 3600 - mins * 60)
      
      sp <- paste("In choosen data range ", hrs, "h ", mins, "m ", secs, "s", " was spent using the phone", sep="")
      
      paste(sp, day, time, applic, sep = "<br>")
      
    }
    
  })
  
}

shinyApp(ui = ui, server = server)
