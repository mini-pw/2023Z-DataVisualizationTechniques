library(shinycssloaders)
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(plotly)
library(vistime)
library(lubridate)
library(stringr)
library(packrat)
library(rsconnect)
library(bslib)
library(shinyWidgets)

WW_wykres2 <- read.csv("wykres_2_WW", sep=",", header=TRUE, fileEncoding ="UTF-8",stringsAsFactors=FALSE)
ST_wykres2 <- read.csv("wykres_2_ST", sep=",",header=TRUE, fileEncoding ="UTF-8",stringsAsFactors=FALSE)
MM_wykres2 <- read.csv("wykres_2_MM", sep=",", header=TRUE,fileEncoding ="UTF-8",stringsAsFactors=FALSE)

MM_wykres3 <- read.csv("wykres_3_MM", sep=",",header=TRUE, fileEncoding ="UTF-8",stringsAsFactors=FALSE)
WW_wykres3 <- read.csv("wykres_3_WW", sep=",", header=TRUE,fileEncoding ="UTF-8",stringsAsFactors=FALSE)
ST_wykres3 <- read.csv("wykres_3_ST", sep=",", header=TRUE,fileEncoding ="UTF-8",stringsAsFactors=FALSE)

ST <- read.csv("wykres_1_5_ST", sep=",",header=TRUE, fileEncoding ="UTF-8",stringsAsFactors=FALSE)
WW <- read.csv("wykres_1_5_WW", sep=",",header=TRUE, fileEncoding ="UTF-8",stringsAsFactors=FALSE)
MM <- read.csv("wykres_1_5_MM", sep=",",header=TRUE, fileEncoding ="UTF-8",stringsAsFactors=FALSE)

ST_wykres4 <- read.csv("wykres_4_ST", sep=",",header=TRUE, fileEncoding ="UTF-8",stringsAsFactors=FALSE)
WW_wykres4 <- read.csv("wykres_4_WW", sep=",",header=TRUE, fileEncoding ="UTF-8",stringsAsFactors=FALSE)
MM_wykres4 <- read.csv("wykres_4_MM", sep=",",header=TRUE, fileEncoding ="UTF-8",stringsAsFactors=FALSE)

ST_wykres6 <- read.csv("wykres_6_ST", sep=",",header=TRUE, fileEncoding ="UTF-8",stringsAsFactors=FALSE)
WW_wykres6 <- read.csv("wykres_6_WW", sep=",",header=TRUE, fileEncoding ="UTF-8",stringsAsFactors=FALSE)
MM_wykres6 <- read.csv("wykres_6_MM", sep=",",header=TRUE, fileEncoding ="UTF-8",stringsAsFactors=FALSE)


source('ui8.R')

server <- function(input, output, session) {
  
  
  output$plot1 <- renderPlotly({
    zaczytanie <-function(df){
      df <- df  %>% 
        filter(day >= input$dateRange[1]  & day <= input$dateRange[2])%>%
        filter(week_day %in% input$dni_tygodnia )%>%
        filter(type %in% input$urzadzenie) %>%
        select(-c(category))
      
      czas <- df %>%
        summarise(days = as.Date(input$dateRange[2]) - as.Date(input$dateRange[1]) + 1)
      
      df <- df %>% 
        group_by(hour) %>%
        summarise(duration = as.integer(sum(duration)/as.integer(czas$days)))
      return(df)
    }
    
    dodanie_zer <- function(df, i) {
      czyZmiana <- df %>% 
        filter(hour == i)
      
      if (nrow(czyZmiana) != 1){
        df <- rbind(df ,data.frame(hour = i, duration = 0))
      }
      return(df)
    }
    
    wykres <- function(df, show) {
      df <- df %>%
        arrange(desc(hour))
      plt <- plot_ly(df, x = ~hour,
                     y = ~duration,
                     type = "scatter",
                     mode = "lines+markers",
                     marker = list(size = 10, color = "#2c3e50"),
                     line = list(color = "#2c3e50"),
                     showlegend = show,
                     hoverinfo = 'text',
                     text = ~paste('</br> Czas łączny',
                                   '</br> Godzina: ', hour, ":00 - ", hour ,":59",
                                   '</br> Średnio minut: ', duration)
      )
      return(plt)
    }
    
    wykresST <- zaczytanie(ST)
    wykresWW <- zaczytanie(WW)
    wykresMM <- zaczytanie(MM)
    
    for (i in 0:23){
      wykresST <- dodanie_zer(wykresST, i)
      wykresWW <- dodanie_zer(wykresWW, i)
      wykresMM <- dodanie_zer(wykresMM, i)
    }
    
    p1 <- wykres(wykresWW, T)
    p2 <- wykres(wykresST, F)
    p3 <- wykres(wykresMM, F)
    
    subplot(p1,p2,p3,nrows = 1, shareX = TRUE, shareY = TRUE)  %>%
      layout(xaxis = list(title = ""),
             xaxis2 = list(title = "Godzina"),
             xaxis3 = list(title = ""),
             yaxis = list( title = "Średnia liczba minut"
             )) %>%
      layout(
        annotations = list(
          list(
            x = 0.16,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 1",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          ),
          list(
            x = 0.5,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 2",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          ),
          list(
            x = 0.85,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 3",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          )))
  })
  
  output$plot2 <- renderPlotly({
    
    
    zaczytanie <-function(df){
      df <- df  %>% 
        filter(day >= input$dateRange[1]  & day <= input$dateRange[2])%>%
        filter(week_day %in% input$dni_tygodnia )%>%
        filter(type %in% input$urzadzenie)
      
      czas <- df %>%
        summarise(days = as.Date(input$dateRange[2]) - as.Date(input$dateRange[1]) + 1)
      
      df <- df %>% 
        group_by(category, hour) %>%
        summarise(duration = as.integer(sum(duration)/as.integer(czas$days)))
      return(df)
    }
    
    dodanie_zer <- function(df, i) {
      czyZmiana <- df %>% 
        filter(hour == i)
      
      if (nrow(czyZmiana) != 3){
        if (nrow(czyZmiana %>% filter(category == "praca")) == 0){
          df <- rbind(df ,data.frame(hour = i,  category = "praca", duration = 0))
        }
        if (nrow(czyZmiana %>% filter(category == "rozrywka")) == 0){
          df <- rbind(df ,data.frame(hour = i,  category = "rozrywka", duration = 0))
        }
        if (nrow(czyZmiana %>% filter(category == "inne")) == 0){
          df <- rbind(df ,data.frame(hour = i,  category = "inne", duration = 0))
        }
      }
      return(df)
    }
    
    wykres <- function(df, show) {
      df <- df %>%
        arrange(desc(hour))
      plt <- plot_ly(df, x = ~hour,
                     y = ~duration,
                     type = "scatter",
                     mode = "lines+markers",
                     marker = list(size = 10),
                     color = ~category,
                     showlegend = show,
                     hoverinfo = 'text',
                     text = ~paste('</br> Aktywność: ', category,
                                   '</br> Godzina: ', hour, ":00 - ", hour ,":59",
                                   '</br> Średnio minut: ', duration)
      )
      return(plt)
    }
    
    
    
    wykresST <- zaczytanie(ST)
    wykresWW <- zaczytanie(WW)
    wykresMM <- zaczytanie(MM)
    
    for (i in 0:23){
      wykresST <- dodanie_zer(wykresST, i)
      wykresWW <- dodanie_zer(wykresWW, i)
      wykresMM <- dodanie_zer(wykresMM, i)
    }
    
    p1 <- wykres(wykresWW, T)
    p2 <- wykres(wykresST, F)
    p3 <- wykres(wykresMM, F)
    
    subplot(p1,p2,p3,nrows = 1, shareX = TRUE, shareY = TRUE)  %>%
      layout(xaxis = list(title = ""),
             xaxis2 = list(title = "Godzina"),
             xaxis3 = list(title = ""),
             yaxis = list( title = "Średnia liczba minut"
             )) %>%
      layout(
        legend = list(orientation = "h",
                      xanchor = "center",
                      x = 0.5, y = 1.3, bgcolor = "transparent"),
        annotations = list(
          list(
            x = 0.16,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 1",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          ),
          list(
            x = 0.5,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 2",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          ),
          list(
            x = 0.85,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 3",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          )))
  })
  
  output$plot3 <- renderPlotly({
    wykres <- function(df){
      df <- df %>%
        filter(day >= input$dateRange2[1]  & day <= input$dateRange2[2])%>%
        filter(hour >= input$godziny[1]  & hour <= input$godziny[2])%>%
        filter(week_day %in% input$dni_tygodnia2 )%>%
        filter(type %in% input$urzadzenie2) %>%
        group_by(app) %>%
        summarise(godziny = sum(duration)/3600)  %>%
        mutate(app = fct_reorder(app, godziny, .desc = TRUE) ) %>%
        arrange(desc(godziny)) %>%
        head(8)
      
      plot <- plot_ly(df ,x = ~app,
                      y = ~godziny,
                      type = "bar",
                      name=" ",
                      marker = list(color = "#2c3e50"),
                      hovertemplate = paste('%{x}', '<br>Liczba godzin: %{y:.2f}<br>')
      )
      return(plot)
    }
    
    p1 <- wykres(WW_wykres2)
    p2 <- wykres(ST_wykres2)
    p3 <- wykres(MM_wykres2)
    
    
    subplot(p1,p2,p3,nrows = 1, shareX = TRUE, shareY = TRUE)  %>%
      layout(xaxis = list(title = "", tickangle = 35),
             xaxis2 = list(title = "Aplikacje", tickangle = 35),
             xaxis3 = list(title = "", tickangle = 35),
             yaxis = list( title = "Liczba godzin"
             ),showlegend = F) %>%
      layout(
        annotations = list(
          list(
            x = 0.16,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 1",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          ),
          list(
            x = 0.5,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 2",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          ),
          list(
            x = 0.85,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 3",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          )))
  })
  output$plot4 <- renderPlotly({
    wykres <- function(df2,df,show){
      df2 <- df2 %>%
        filter(day >= input$dateRange2[1]  & day <= input$dateRange2[2])%>%
        filter(hour >= input$godziny[1]  & hour <= input$godziny[2])%>%
        filter(week_day %in% input$dni_tygodnia2 )%>%
        filter(type %in% input$urzadzenie2) %>%
        group_by(app) %>%
        summarise(godziny = sum(duration)/3600)  %>%
        mutate(app = fct_reorder(app, godziny, .desc = TRUE) ) %>%
        arrange(desc(godziny)) %>%
        head(8)
      df <- df %>%
        mutate(st = case_when(st == "AFK" ~ st,
                              T ~ "NIE AFK"))
      df <- df%>%
        filter(day >= input$dateRange2[1]  & day <= input$dateRange2[2])%>%
        filter(hour >= input$godziny[1]  & hour <= input$godziny[2])%>%
        filter(week_day %in% input$dni_tygodnia2 )%>%
        filter(type %in% input$urzadzenie2) %>%
        filter(app %in% df2$app) %>%
        group_by(app) %>%
        mutate(sum = sum(duration)) %>%
        group_by(app, st) %>%
        mutate(sum_g = sum(duration)) %>%
        mutate(percent = as.integer (sum_g)/as.integer (sum)) %>%
        summarise(percent = mean(percent)*100, duration = mean(duration)*100) %>%
        select(app, percent, st) %>% inner_join(df2, by = "app") %>%
        ungroup()%>%
        mutate(app = fct_reorder(app, godziny, .desc = TRUE) ) %>%
        arrange(desc(godziny))
      afk <- df %>% filter(st == "AFK")
      nafk <- df %>% filter(st == "NIE AFK")
      if(nrow(afk)==0&nrow(nafk)==0 ) {
        df <- rbind(data.frame(percent = 0, st = "AFK", app = " "),
                    data.frame(percent = 0, st = "NIE AFK", app = " ")) %>%
          pivot_wider(names_from = st, values_from = percent)
        fig <- plot_ly(df, x = ~app, y = ~`NIE AFK`, type = 'bar', name = "NIE AFK",showlegend = show,
                       marker = list(color = "#2c3e50"),
                       hovertemplate = paste('%{x}', '<br>Procent całej aktywności: %{y:.2f}<br>')) %>% 
          add_trace(y = ~AFK, name = "AFK",showlegend = show,
                    marker = list(color = "#25b09c")) %>%
          layout(yaxis = list(title = 'Procent'),xaxis = list(title = 'Aplikacje'), barmode = 'stack')
        fig
        return(fig)
      }
      if(nrow(afk)==0 ){
        for (i in 1:nrow(df)){
          df <- rbind(df , data.frame(percent = 0, st = "AFK", app = df[i,"app"], godziny = df[i,"godziny"]))
        }
      }
      if(nrow(nafk)==0 ){
        for (i in 1:nrow(df)){
          df <- rbind(df , data.frame(percent = 0, st = "NIE AFK", app = df[i,"app"], godziny = df[i,"godziny"]))
        }
      }
      df <- df %>%
        select(-godziny) %>%
        pivot_wider(names_from = st, values_from = percent) %>%  
        replace(is.na(.), 0)
      
      fig <- plot_ly(df, x = ~app, y = ~`NIE AFK`, type = 'bar', name = "NIE AFK",showlegend = show,
                     marker = list(color = "#2c3e50"),
                     hovertemplate = paste('%{x}', '<br>Procent całej aktywności: %{y:.2f}<br>')) %>% 
        add_trace(y = ~AFK, name = "AFK",showlegend = show,
                  marker = list(color = "#25b09c")) %>%
        layout(yaxis = list(title = 'Procent'),xaxis = list(title = 'Aplikacje'), barmode = 'stack')
      fig
      return(fig)
    }
    p1 <- wykres(WW_wykres2, WW_wykres4, T)
    p2 <- wykres(ST_wykres2, ST_wykres4, F)
    p3 <- wykres(MM_wykres2, MM_wykres4, F)
    
    subplot(p1,p2,p3,nrows = 1, shareX = TRUE, shareY = TRUE)  %>%
      layout(xaxis = list(title = "", tickangle = 35),
             xaxis2 = list(title = "Aplikacje", tickangle = 35),
             xaxis3 = list(title = "", tickangle = 35),
             yaxis = list( title = "Procent"
             )) %>%
      layout(
        legend = list(orientation = "h",
                      xanchor = "center",
                      x = 0.5, y = 1.3, bgcolor = "transparent"),
        annotations = list(
          list(
            x = 0.16,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 1",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          ),
          list(
            x = 0.5,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 2",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          ),
          list(
            x = 0.85,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 3",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          )))
    
  })
  
  
  output$plot7 <- renderPlotly({
    APP <- WW_wykres3 %>%
      filter(day == input$day, type %in% input$urzadzenie3) %>%
      mutate(duration = as.POSIXct(end) - as.POSIXct(start)) %>%
      group_by(app) %>%
      summarise(sum = sum(duration)) %>%
      arrange(desc(sum)) %>%
      head(8)
    
    df <- WW_wykres3 %>%
      filter(day == input$day, type %in% input$urzadzenie3, app %in% APP$app)
    
    
    if (nrow(df) == 0){
      df <- df %>% select(c(app,start, end, color, tooltip))
      
      start <- as.POSIXct(paste(input$day, "00:00:01", sep=" "))
      end <- as.POSIXct(paste(input$day, "23:59:59", sep=" "))
      
      df <- rbind(df,data.frame(app = " ", 
                                start = start,
                                end = end,
                                color = "transparent",
                                tooltip = "")
      ) %>% 
        mutate(tooltip = paste(app,"\n", "Od", format(start, "%H:%M:%S"), "do", format(end, "%H:%M:%S")))
    }
    
    plot<- df %>% vistime(
      col.event = "app",
      col.start = "start",
      col.end = "end",
      col.group = "app",
      show_labels = F
    ) %>%
      layout(
        xaxis = list(
          type = "date",
          tickformat = "%H:00",
          range = c(as.POSIXct(paste(input$day,"00:00:00")),
                    as.POSIXct(paste(input$day, "23:59:59")))
        ),
        annotations = list(
          list(
            x = 0.5,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 1",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          )
        )
      )
    
    plot
  })
  
  output$plot8 <- renderPlotly({
    APP <- ST_wykres3 %>%
      filter(day == input$day, type %in% input$urzadzenie3) %>%
      mutate(duration = as.POSIXct(end) - as.POSIXct(start)) %>%
      group_by(app) %>%
      summarise(sum = sum(duration)) %>%
      arrange(desc(sum)) %>%
      head(8)
    df <- ST_wykres3 %>%
      filter(day == input$day, type %in% input$urzadzenie3, app %in% APP$app)
    
    if (nrow(df) == 0){
      df <- df %>% select(c(app,start, end, color, tooltip))
      
      start <- as.POSIXct(paste(input$day, "00:00:01", sep=" "))
      end <- as.POSIXct(paste(input$day, "23:59:59", sep=" "))
      
      df <- rbind(df,data.frame(app = " ", 
                                start = start,
                                end = end,
                                color = "transparent",
                                tooltip = "")
      ) %>% 
        mutate(tooltip = paste(app,"\n", "Od", format(start, "%H:%M:%S"), "do", format(end, "%H:%M:%S")))
    }
    
    plot<- df %>% vistime(
      col.event = "app",
      col.start = "start",
      col.end = "end",
      col.group = "app",
      show_labels = F
    ) %>%
      layout(
        xaxis = list(
          type = "date",
          tickformat = "%H:00",
          range = c(as.POSIXct(paste(input$day,"00:00:00")),
                    as.POSIXct(paste(input$day, "23:59:59")))
        ),
        annotations = list(
          list(
            x = 0.5,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 2",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          )
        )
      )
    
    plot
  })
  output$plot9 <- renderPlotly({
    APP <- MM_wykres3 %>%
      filter(day == input$day, type %in% input$urzadzenie3) %>%
      mutate(duration = as.POSIXct(end) - as.POSIXct(start)) %>%
      group_by(app) %>%
      summarise(sum = sum(duration)) %>%
      arrange(desc(sum)) %>%
      head(8)
    df <- MM_wykres3 %>%
      filter(day == input$day, type %in% input$urzadzenie3, app %in% APP$app)
    
    if (nrow(df) == 0){
      df <- df %>% select(c(app,start, end, color, tooltip))
      
      start <- as.POSIXct(paste(input$day, "00:00:01", sep=" "))
      end <- as.POSIXct(paste(input$day, "23:59:59", sep=" "))
      
      df <- rbind(df,data.frame(app = " ", 
                                start = start,
                                end = end,
                                color = "transparent",
                                tooltip = "")
      ) %>% 
        mutate(tooltip = paste(app,"\n", "Od", format(start, "%H:%M:%S"), "do", format(end, "%H:%M:%S")))
    }
    
    plot<- df %>% vistime(
      col.event = "app",
      col.start = "start",
      col.end = "end",
      col.group = "app",
      show_labels = F
    ) %>%
      layout(
        xaxis = list(
          type = "date",
          tickformat = "%H:00",
          range = c(as.POSIXct(paste(input$day,"00:00:00")),
                    as.POSIXct(paste(input$day, "23:59:59")))
        ),
        annotations = list(
          list(
            x = 0.5,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 3",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          )
        )
      )
    
    plot
  })
  
  output$plot10 <- renderPlotly({
    plot_spotify <- function(df){
      
      df <- df %>%
        filter(day >= input$dateRange3[1]  & day <= input$dateRange3[2])%>%
        filter(hour >= input$godziny2[1]  & hour <= input$godziny2[2])%>%
        summarise(AFK = as.integer (sum(AFK)), praca = as.integer(sum(praca)), rozrywka = as.integer(sum(rozrywka)))%>%
        mutate(sum = AFK + rozrywka + praca) %>%
        mutate(AFK = 100*(AFK/sum), rozrywka = 100*(rozrywka/sum), praca = 100*(praca/sum))
      dt <- rbind(data.frame(Procent = df$AFK, "Aktywność" = "AFK"),
                  data.frame(Procent = df$praca, "Aktywność" = "praca"),
                  data.frame(Procent = df$rozrywka, "Aktywność" = "rozrywka"))
      
      plot_ly(dt,x =~ `Aktywność`, y = ~ `Procent`, type = 'bar',
              name = " ",
              showlegend = F,
              marker = list(color = "#2c3e50"),
              hovertemplate = paste('Aktywność: %{x}', '<br>Procent słuchania spotify: %{y:.2f}%<br>'))
    }
    p1 <- plot_spotify(WW_wykres6)
    p2 <- plot_spotify(ST_wykres6)
    p3 <- plot_spotify(MM_wykres6)
    
    subplot(p1,p2,p3,nrows = 1, shareX = TRUE, shareY = TRUE)  %>%
      layout(xaxis = list(title = "", tickangle = 35),
             xaxis2 = list(title = "Aktywności", tickangle = 35),
             xaxis3 = list(title = "", tickangle = 35),
             yaxis = list( title = "Procent", range = c(0,110))) %>%
      layout(
        annotations = list(
          list(
            x = 0.16,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 1",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          ),
          list(
            x = 0.5,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 2",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          ),
          list(
            x = 0.85,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 3",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          )))
  })
}