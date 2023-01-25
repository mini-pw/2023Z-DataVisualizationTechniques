library(shiny)
library(ggplot2)
library(magick)
library(cowplot)
library(gtools)
library(ggplot2)
library(spotifyr)
library(plotly)
library(datasets)
library(data.table)
library(scales)
library(fmsb)


shinyServer(function(input, output) {
  # ogarnianie danych piotr -------------------------------------------------------
  # setwd("C:/Users/piotr/Desktop/TWD-projekt2/app")
  
  id <- ''
  secret <- ''
  Sys.setenv(SPOTIFY_CLIENT_ID = id)
  Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
  access_token <- get_spotify_access_token()
  
  # df<-read.csv("../data/piotrData.csv")
  # start_month <- 2
  # end_month <- 11
  # 
  # top_tracks <- df %>% 
  #   filter(start_month <= month, month <= end_month) %>% 
  #   group_by(artistName, trackName) %>% 
  #   summarise(times = n(), total_played = sum(played))
  # 
  # top_artists <- top_tracks %>% 
  #   group_by(artistName) %>% 
  #   summarise(total_played = sum(total_played)) %>% 
  #   arrange(-total_played) %>% 
  #   head(9) 
  # 
  # top_track_per_artist <- top_tracks %>% 
  #   group_by(artistName) %>% 
  #   slice(which.max(times)) %>% 
  #   mutate(total_played_track = total_played) %>% 
  #   select(-total_played) 
  # 
  # top_artists <- merge(top_artists, top_track_per_artist, by = "artistName") %>% 
  #   arrange(-total_played)
  # 
  # top_tracks <- top_tracks %>% 
  #   arrange(-times) %>% 
  #   head(9)
  # top_tracks <- read.csv("../data/piotrTracks.csv")
  top_artists1 <- read.csv("data/piotrArtists.csv")
  top_artists2 <- read.csv("data/matiArtists.csv")
  top_artists3 <- read.csv("data/krzysztofArtists.csv")
  
  top_tracks1 <- read.csv("data/piotrTracks.csv")
  top_tracks2 <- read.csv("data/matiTracks.csv")
  top_tracks3 <- read.csv("data/krzysztofTracks.csv") 
  
  
  
  # piotr funckje ----------------------------------------------------------
  
  top_artists_urls <- function(i) {
    if (input$chart == "Piotrek") {
      top_artists <- top_artists1
    }
    if (input$chart == "Mati") {
      top_artists <- top_artists2
    }
    if (input$chart == "Krzysztof") {
      top_artists <- top_artists3
    }
    x <- pull(top_artists, artistName)[i]
    if(x=='Avi') {
      res <-  "https://i.scdn.co/image/ab6761610000e5eb455c981dc560842d13e98bb4"
    } else if(x=='Oki') {
      res <- "https://i.scdn.co/image/ab6761610000e5eb5d2cff79966021115a5f84b2"
    } else {
      res <- search_spotify(x, "artist")[[1, "images"]][[1]][1, "url"]
    }
    # for (i in 2:9)
    # {
    #   res <- c(res, search_spotify(top_artists[i], "artist")[[1, "images"]][[1]][1, "url"])
    # }
    res
  }
  
  artists_summary <- function(i) {
    res <- paste0("Miejsce ", i,": ")
    
    artist <- top_artists[i,]
    
    name <- pull(artist, artistName)
    
    res <- paste0(res, name, ".", "<br>", "Łącznie przesłuchane: ")
    
    res <- paste0(res, round(pull(artist, total_played), digits = 2)," minut.",
                  "<br>", "Ulubiony utwór: ",pull(artist, trackName), ".")
    res
    
    # res <- paste0(res, "\n", "Ulubiony utwór: ",)
    
  }
  
  artists_summary_table <- function(i) {
    if (input$chart == "Piotrek") {
      top_artists <- top_artists1
    }
    if (input$chart == "Mati") {
      top_artists <- top_artists2
    }
    if (input$chart == "Krzysztof") {
      top_artists <- top_artists3
    }
    
    artist <- top_artists[i,]
    
    name <- pull(artist, artistName)
    # name <- "sowon wnndwun wd hbhuujjj lorem ispsuem smwiw adplq"
    
    played <- paste0("Przez ostatni rok łącznie przesłuchane ", 
                     round(pull(artist, total_played), digits = 2), 
                     " minut, czyli ",  round( 100 * pull(artist, fraction_of_total), digits = 2),
                     "% całego czasu.")
    
    fav_track <- paste0("Ulubiony utwór od ", pull(artist, artistName), " to ", pull(artist, trackName),
                        " i przesłuchany był ", pull(artist, times), " razy.")
    # colnames(table)[1] <- paste("Miejsce", i)
    table <- data.frame(transpose(data.frame(name, played, fav_track)))
    colnames(table)[1] <- paste("Miejsce", i)
    table
    # print(colnames(table))
    # colnames(table) <- paste("Miejsce", i)
    
    # row.names(table) <- c("name", "played in total", "fav track")
    # row.names(table) <- ""
    
    
  }
  
  
  generate_caption <- function(start, end) {
    caption <- "W okresie "
    
    # start_mod12 <- start %% 12 
    # end_mod12 <- end %% 12
    
    startMonth <- ifelse(start %% 12 < 10, paste0(0, start %% 12),start %% 12)
    startYear <- ifelse(start - 12 > 0, max(df$year), min(df$year))
    
    endMonth <- ifelse(end %% 12 < 10, paste0(0, end %% 12),end %% 12)
    endYear <- ifelse(end - 12 > 0, max(df$year), min(df$year))
    
    start <- paste0( paste0(startMonth, "."), startYear ) 
    end <- paste0( paste0(endMonth, "."), endYear ) 
    
    caption <- paste0( paste0(  paste0(caption, start), "-" ), end )
    
    caption
  }
  
  
  
  
  dfKrzys <- read.csv('data/krzysztofData.csv')
  
  dfk<-read.csv('data/krzysztofData.csv')
  dfp<-read.csv('data/piotrData.csv')
  dfm<-read.csv('data/matiData.csv')
  
  # dfm %>% 
  #   mutate(hour = substr(endTime, 11, 16),
  #          day = substr(endTime, 9, 10),
  #          month = as.numeric(substr(endTime, 6, 7)),
  #          year = substr(endTime,1,4),
  #          played = msPlayed / 60000) %>% 
  #   filter(played > 0.5)%>%
  #   mutate(month = ifelse(year > min(year),month + 12, month))->dfm
  
  wyboryk<-dfk%>%group_by(artistName)%>%summarise(suma=sum(played))
  wyboryk<-head(wyboryk[order(wyboryk$suma,decreasing=TRUE),][,1],10)
  
  wyboryp<-dfp%>%group_by(artistName)%>%summarise(suma=sum(played))
  wyboryp<-head(wyboryp[order(wyboryp$suma,decreasing=TRUE),][,1],10)
  
  wyborym<-dfm%>%group_by(artistName)%>%summarise(suma=sum(played))
  wyborym<-head(wyborym[order(wyborym$suma,decreasing=TRUE),][,1],10)
  
  # funkcje krzys -------------------------------------------------
  
  
  dfKrzys <- read.csv('data/krzysztofData.csv')
  # funkcje mati ---------------------------------------------------------
  
  as.sunburstDF <- function(DF, valueCol = NULL){
    require(data.table)
    
    colNamesDF <- names(DF)
    
    if(is.data.table(DF)){
      DT <- copy(DF)
    } else {
      DT <- data.table(DF, stringsAsFactors = FALSE)
    }
    
    DT[, root := names(DF)[1]]
    colNamesDT <- names(DT)
    
    if(is.null(valueCol)){
      setcolorder(DT, c("root", colNamesDF))
    } else {
      setnames(DT, valueCol, "values", skip_absent=TRUE)
      setcolorder(DT, c("root", setdiff(colNamesDF, valueCol), "values"))
    }
    
    hierarchyCols <- setdiff(colNamesDT, "values")
    hierarchyList <- list()
    
    for(i in seq_along(hierarchyCols)){
      currentCols <- colNamesDT[1:i]
      if(is.null(valueCol)){
        currentDT <- unique(DT[, ..currentCols][, values := .N, by = currentCols], by = currentCols)
      } else {
        currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by=currentCols, .SDcols = "values"]
      }
      setnames(currentDT, length(currentCols), "labels")
      hierarchyList[[i]] <- currentDT
    }
    
    hierarchyDT <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)
    
    parentCols <- setdiff(names(hierarchyDT), c("labels", "values", valueCol))
    hierarchyDT[, parents := apply(.SD, 1, function(x){fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))}), .SDcols = parentCols]
    hierarchyDT[, ids := apply(.SD, 1, function(x){paste(x[!is.na(x)], collapse = " - ")}), .SDcols = c("parents", "labels")]
    hierarchyDT[, c(parentCols) := NULL]
    return(hierarchyDT)
  }
  # ogarnianie danych mati --------------------------------------------------
  
  genres_frame1 <- read.csv("data/top_artists_piotr.csv")
  genres_frame2 <- read.csv("data/top_artists_mati.csv")
  genres_frame3 <- read.csv("data/KrzysztofGatunki.csv")
  genres_frame <- genres_frame1
  
  rock <- c("modern rock", "rock", "garage rock", "post-grunge", "rap rock", "modern blues rock", "modern hard rock", "funk rock", "classic rock", "art rock", "hard rock", "album rock", "psychedelic rock", "indie rock", "indie garage rock", "stoner rock", "ukrainian rock", "piano rock")
  alternative <- c("alternative rock", "modern alternative rock", "permanent wave", "polish alternative", "alternative pop rock", "brighton indie", "indie poptimism", "polish alternative rock", "polish indie")
  metal <- c("alternative metal", "nu metal", "funk metal", "rap metal", "metal", "birmingham metal", "uk doom metal")
  pop <- c("pop", "alt z", "polish pop", "gauze pop", "shiver pop", "modern power pop", "dance pop", "pop rap", "pop dance", "uk pop", "art pop")
  hiphop <- c("trip hop", "polish hip hop", "hip hop", "rap", "polish trap", "polish alternative rap", "underground hip hop", "dark trap", "atl hip hop", "psychedelic hip hop", "polish old school hip hop", "detroit hip hop", "chicago rap")
  electronic <- c("electronica", "indietronica", "electropop", "polish electronica", "edm", "electro house")
  
  
  # sensowniejsze rzeczy ----------------------------------------------------
  
  
  output$top_artist_table1 <- renderTable(artists_summary_table(1))
  output$top_artist_table2 <- renderTable(artists_summary_table(2))
  output$top_artist_table3 <- renderTable(artists_summary_table(3))
  output$top_artist_table4 <- renderTable(artists_summary_table(4))
  output$top_artist_table5 <- renderTable(artists_summary_table(5))
  output$top_artist_table6 <- renderTable(artists_summary_table(6))
  output$top_artist_table7 <- renderTable(artists_summary_table(7))
  output$top_artist_table8 <- renderTable(artists_summary_table(8))
  output$top_artist_table9 <- renderTable(artists_summary_table(9))
  
  output$top_artists_img1 <- renderText({c('<img src="',
                                           top_artists_urls(1),
                                           '"style="width:230px;height:230px;">')})
  
  output$top_artists_img2 <- renderText({c('<img src="',
                                           top_artists_urls(2),
                                           '"style="width:230px;height:230px;">')})
  output$top_artists_img3 <- renderText({c('<img src="',
                                           top_artists_urls(3),
                                           '"style="width:230px;height:230px;">')})
  output$top_artists_img4 <- renderText({c('<img src="',
                                           top_artists_urls(4),
                                           '"style="width:230px;height:230px;">')})
  
  output$top_artists_img5 <- renderText({c('<img src="',
                                           top_artists_urls(5),
                                           '"style="width:230px;height:230px;">')})
  output$top_artists_img6 <- renderText({c('<img src="',
                                           top_artists_urls(6),
                                           '"style="width:230px;height:230px;">')})
  output$top_artists_img7 <- renderText({c('<img src="',
                                           top_artists_urls(7),
                                           '"style="width:230px;height:230px;">')})
  
  output$top_artists_img8 <- renderText({c('<img src="',
                                           top_artists_urls(8),
                                           '"style="width:230px;height:230px;">')})
  output$top_artists_img9 <- renderText({c('<img src="',
                                           top_artists_urls(9),
                                           '"style="width:230px;height:230px;">')})
  
  output$tracksPlot <- renderPlot({
    if (input$chart == "Piotrek") {
      top_tracks <- top_tracks1
    }
    if (input$chart == "Mati") {
      top_tracks <- top_tracks2
    }
    if (input$chart == "Krzysztof") {
      top_tracks <- top_tracks3
    }
    
    dayNight <- input$pora
    if(input$pora == "*") {
      dayNight <- c("day", "night")
    }
    
    start_month <- input$miesiac[1]
    end_month <- input$miesiac[2]
    
    
    
    
    top_tracks %>% 
      filter(month >= start_month) %>% 
      filter(month <= end_month) %>% 
      filter(day_or_night %in% dayNight) %>% 
      group_by(artistName, trackName) %>% 
      summarise(times = sum(times), total_played = sum(total_played)) %>% 
      mutate(name = paste0(artistName, " - ", trackName)) %>% 
      arrange(-times) %>%
      head(9) -> x
    x %>% 
      ggplot(aes(reorder(name,times), times,fill = times)) +
      # ggplot(aes(name, times,fill = times)) +
      geom_col() +
      ylab("Ilość odtworzeń") +
      xlab("Wykonawca i tytuł") +
      coord_flip() +
      # scale_y_continuous(breaks = seq(0, max(max(x$times),2), by = min( max(x$times)  %/% 7, 1 ) )) +
      scale_y_continuous(breaks= pretty_breaks()) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(title = "Najczęściej słuchane utwory") +
      # caption = generate_caption(start_month, end_month)) +
      theme_bw() +
      geom_text(aes(label = name), colour = "white", position=position_stack(vjust=0.5)) +
      theme(axis.text.y=element_blank(),
            legend.position = "none") -> plot
    
    plot
  })
  
  output$sunburstChart <- renderPlotly({
    if (input$chart == "Piotrek") {
      genres_frame <- genres_frame1
      colors <- c("#FF7F0E",'#636EFA', "#8C564B",'#00CC96','#D62728', '#AB63FA', "#17BECF" )
    }
    if (input$chart == "Mati") {
      genres_frame <- genres_frame2
      colors <- c('#636EFA','#D62728','#00CC96','#AB63FA', "#8C564B", "#17BECF", "#FF7F0E" )
    }
    if (input$chart == "Krzysztof") {
      genres_frame <- genres_frame3
      colors <- c("#FF7F0E", '#AB63FA', "#8C564B", '#636EFA','#00CC96', "#17BECF", '#D62728')
    }
    genres_sorted <- genres_frame %>% 
      arrange(desc(count))
    
    genres_sorted_named <- genres_sorted %>% 
      mutate(Genre = ifelse(genres %in% rock, "rock", ifelse(genres %in% alternative, "alternative", ifelse(genres %in% metal, "metal", ifelse(genres %in% pop, "pop", ifelse(genres %in% hiphop, "hiphop", ifelse(genres %in% electronic, "electronic", "other")))))))
    
    genres_sorted_named <- head(genres_sorted_named, 40)  
    
    DF <- genres_sorted_named
    setcolorder(DF, c("Genre", "genres", "count"))
    sunburstDF <- as.sunburstDF(DF, valueCol = "count")
    plot_ly(data = sunburstDF, ids = ~ids, labels= ~labels, 
            parents = ~parents, values= sunburstDF$values/sum(DF$count)*100, type='sunburst', branchvalues = 'total',
            hovertemplate = "%{label}: <br> %{value}% <extra></extra>") %>% 
      layout(sunburstcolorway=colors)
  })
  
  output$topChart <- renderPlotly({
    if (input$chart == "Piotrek") {
      genres_frame <- genres_frame1
    }
    if (input$chart == "Mati") {
      genres_frame <- genres_frame2
    }
    if (input$chart == "Krzysztof") {
      genres_frame <- genres_frame3
    }
    genres_sorted <- genres_frame %>% 
      arrange(desc(count))
    
    genres_sorted_named <- genres_sorted %>% 
      mutate(Genre = ifelse(genres %in% rock, "rock", ifelse(genres %in% alternative, "alternative", ifelse(genres %in% metal, "metal", ifelse(genres %in% pop, "pop", ifelse(genres %in% hiphop, "hiphop", ifelse(genres %in% electronic, "electronic", "other")))))))
    
    plot_ly(data = head(genres_sorted_named,10), y= ~genres, x= ~count, type = "bar", color = ~Genre, colors=c(rock='#636EFA',metal='#D62728',alternative='#00CC96',pop='#AB63FA', hiphop="#FF7F0E")) %>% 
      layout(yaxis = list(categoryorder = "total ascending"))
  })
  
  output$songs<-renderUI({
    if(input$chart=="Mati"){
      wybory<-wyborym
    } else if(input$chart=="Piotrek"){
      wybory<-wyboryp
    } else{
      wybory<-wyboryk
    }
    
    checkboxGroupInput("piosenki",
                       "Wybierz artystów, których bierzemy pod uwagę przy generowaniu drugiego wykresu",
                       choices = unique(wybory$artistName),
                       selected = unique(wybory$artistName)[1])
  })
  
  output$HeatMap<-renderPlot({
    if(input$chart=="Mati"){
      df<-dfm
    } else if(input$chart=="Piotrek"){
      df<-dfp
    } else{
      df<-dfk
    }
    
    dfheat<-df%>%
      filter(month %in% c(input$months,as.numeric(input$months)+12))%>%
      mutate(dzientyg=weekdays(as.Date(endTime)),godzina=substr(hour,1,3))%>%
      mutate(ktorydzien=case_when(
        dzientyg=="poniedziałek"~"1",
        dzientyg=="wtorek"~"2",
        dzientyg=="środa"~"3",
        dzientyg=="czwartek"~"4",
        dzientyg=="piątek"~"5",
        dzientyg=="sobota"~"6",
        TRUE~"7"
      ))%>%
      group_by(ktorydzien,dzientyg,godzina)%>%summarise(Suma=sum(played))
    
    etykiety<-c("poniedziałek","wtorek","środa","czwartek","piątek","sobota","niedziela")
    
    # ggplot(dfheat, aes(godzina,ktorydzien,fill=Suma))+
    #   geom_tile()+
    #   scale_fill_gradient(low="white", high="blue") +
    #   theme_bw()+
    #   scale_y_discrete(expand=c(0,0),labels=etykiety)+
    #   theme(panel.grid=element_blank())+
    #   labs(title = "Porównanie sumy przesłuchanych minut \n w zależności od dnia tygodnia oraz godziny",
    #        x="Godzina dnia",
    #        y="Dzień tygodnia")
    if(input$months < 6 && input$chart=="Piotrek") {
      ggplot() +
        labs(title = "Proszę wybierz miesiąc poźniejszy niż maj z powodu braku danych") 
    } else {
      
      ggplot(dfheat, aes(godzina,ktorydzien,fill=Suma))+
        geom_tile()+
        scale_fill_gradient(low="white", high="blue") +
        theme_bw()+
        scale_y_discrete(expand=c(0,0),labels=etykiety)+
        theme(panel.grid=element_blank())+
        labs(title = "Porównanie sumy przesłuchanych minut \n w zależności od dnia tygodnia oraz godziny",
             x="Godzina dnia",
             y="Dzień tygodnia")
    }
  })
  output$Animate<-renderPlot({
    if(input$chart=="Mati"){
      dff<-dfm
    } else if(input$chart=="Piotrek"){
      dff<-dfp
    } else{
      dff<-dfk
    }
    
    ramka<-dff%>%
      mutate(miesiac = ifelse(as.numeric(substr(endTime, 6, 7))==12,12,as.numeric(substr(endTime, 6, 7))))
    
    dfline<-ramka%>%filter(artistName %in% input$piosenki)%>%
      group_by(artistName,miesiac)%>%
      summarise(Suma=sum(played))
    
    ggplot(dfline, aes(miesiac, Suma,color=artistName))+
      geom_line()+
      theme_minimal()+
      labs(title="Porównanie sumy minut w przesłuchanych w miesiącu piosenek różnych artystów",
           x="Miesiąc",
           y="Suma minut")+
      scale_x_continuous(breaks = 1:12, labels=c("styczeń", "luty", "marzec", "kwiecień","maj",
                                                 "czerwiec","lipiec", "sierpień", "wrzesień", "październik",
                                                 "listopad", "grudzień"))
  })
  output$Hist<-renderPlot({
    if(input$chart=="Mati"){
      dfff<-dfm
    } else if(input$chart=="Piotrek"){
      dfff<-dfp
    } else{
      dfff<-dfk
    }
    
    dfhist<-dfff%>%mutate(dzientyg=weekdays(as.Date(dfff$endTime)),godzina=as.numeric(substr(hour,1,3)))%>%
      select(c("dzientyg","godzina"))
    
    dfpom<-dfhist%>%filter(dzientyg %in% input$dni)
    
    ggplot(dfpom, aes(x=godzina))+
      geom_histogram(bins = 24,color="black", fill="blue")+
      theme_classic()+
      labs(title="Ilość utworów przesłuchanych w zależności od godziny dnia",
           x="Godzina dnia",
           y="Ilość utworów")
  })
  
  output$textKrzys1<-renderText({
    "Ten oto wykres przedstawia jak dużo czasu spędzamy w dany dzień tygodnia o danej godzinie na odtwarzaniu utworów w aplikacji Spotify.
    Za pomocą wyboru numerów miesięcy wybieramy, które z nich będą brane pod uwagę na pierwszym wykresie."
  })
  output$textKrzys2<-renderText({
    "Wykres, który widzimy poniżej prezentuje zależność ilości przesłuchanych utworów od danych artystów w zależności od miesiąca.
    Do wyboru mamy 10 najczęściej słuchanych artystów"
  })
  output$textKrzys3<-renderText({
    "Poniższy wykres prezentuje ilość przesłuchanych utworów w zależności od godziny dnia. Możemy wybrać, 
    dla którego dnia tygodnia chcemy zobaczyć histogram."
  })
  
  # output$distPlot <- renderPlot({
  #   pom<-dfKrzys%>%filter(month %in% c(input$months,as.numeric(input$months)+12))%>%
  #     mutate(onlyHour=substr(hour,1,3))%>%
  #     group_by(onlyHour)%>%
  #     summarise(timePlayed=sum(played))
  #   ggplot(pom,aes(x=onlyHour, y=timePlayed))+
  #     geom_col()+
  #     labs(title = "Ilość przesłuchanych minut w zależności od pory dnia",
  #          x="Godzina",
  #          y="Ilość minut")
  # })
  
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Menu item", icon = icon("calendar")),
      menuItem("Menu item2", icon = icon("calendar")),
      menuItem("Menu item3", icon = icon("calendar")),
      dashboardSidebar(sidebarMenu(id = "menu", sidebarMenuOutput("menu")))
    )
  })
  
})