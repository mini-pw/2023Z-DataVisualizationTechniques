library(shiny)
library(ggplot2)
library(dplyr)
library(ggalluvial)

library(hms)
library(stringr)
library(plotly)

library(countrycode)
library(janitor)
library(padr)
library(hablar)
library(ggflags)
library(stringi)
library(lubridate)
library(tidyverse)
library(ggbump)

library(igraph)
library(ggraph)
library(visNetwork)

ViewingActivity <- read.csv("./ViewingActivity.csv")


# UI do częstotliwości
ui1 <- fluidPage(
  
  titlePanel("Ile czasu spędzono na oglądaniu Netflixa?"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId= "account1",
                  "Konto:",
                  choices=c("A"="Ewa K",
                            "B"="Ewa",
                            "C"="Debi"),
                  selected=0),
      shiny::markdown(
        "Wykres przedstawia ile godzin zostało poświęcone na oglądanie Netflixa
          każdego dnia. Pasek pod wykresem pozwala na ograniczenie interesującego
          nas zakresu czasowego."
      )
    ),
    
    mainPanel(
      shinycssloaders::withSpinner(
        shinyjqui::jqui_resizable(plotlyOutput("frequencyPlot")),
        type = 4,
        color = "#b1060f",
        size=0.5)
    )
  )
  
)

# UI do rankingu
ui2 <- fluidPage(
  
  titlePanel("Ulubione seriale na przestrzeni czasu"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId= "year",
                  "Rok:",
                  choices=c(2019,2020,2021,2022),
                  selected=2021),
      selectInput(inputId= "half",
                  "Połowa roku:",
                  choices=c("I" = 0,
                            "II" = 6),
                  selected="I"),
      shiny::markdown(
        "Ranking dotyczy wszystkich profili uwzględnionych łącznie. Obrazuje on
        w jaki sposób zmieniały się na przestrzeni miesięcy ulubione seriale.
        Jako ulubiony serial definiujemy taki, na którego oglądanie poświęcona
        została największa liczba minut."
      )
    ),
    
    mainPanel(
      shinycssloaders::withSpinner(
        shinyjqui::jqui_resizable(plotOutput("bumpPlot")),
        type = 4,
        color = "#b1060f",
        size=0.5)
    )
  )
)

# UI do alluviala
ui3 <- fluidPage(
  
  titlePanel("Jak często polegano na autoodtwarzaniu?"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId= "account3",
                  "Konto:",
                  choices=c("A"="Ewa K",
                            "B"="Ewa",
                            "C"="Debi"),
                  selected=0),
      shiny::markdown(
        "Wysokość czerwonych słupków po prawej oraz lewej stronie wykresu odpowiada
        liczbie minut poświęconych na Netflixa. Są one podzielone odpowiednio względem
        typu oglądanego contentu (film/serial) oraz według lat. Proporcje kolorów na
        'tasiemkach' mówią z kolei ile obejrzanego materiału zostało 'autoodtworzone'
        bez ingerencji użytkownika. W zdecydowanej większości przypadków dotyczy to 
        sytuacji, gdzie kilka odcinków danego serialu zostało obejrzane z rzędu."
      )
    ),
    
    mainPanel(
      shinycssloaders::withSpinner(
        shinyjqui::jqui_resizable(plotOutput("alluvialPlot")),
        type = 4,
        color = "#b1060f",
        size=0.5)
    )
  )
)

# UI do grafu
ui4 <- fluidPage(
  titlePanel("Graf zależności"),
  
  sidebarLayout(
    sidebarPanel(
      shiny::markdown(
        "Graf po prawej obrazuje w jaki sposób pokrywają się preferencje filmowe poszczególnych osób.
        Duże, niebieskie wierzchołki odpowiadają trzem kontom w serwisie Netflix. Białe wierzchołki z
        kolei odpowiadają filmom oraz serialom - jeżeli pomiędzy białym a niebieskim wierzchołkiem
        występuje krawędź, oznacza to, że dana produkcja oglądana była na danym koncie.
        
        Można łatwo zauważyć, że A i B mają podobny gust, gdyż wiele wierzchołków-filmów
        połączonych jest jednocześnie z obydwoma kontami. Z kolei C ma odmienny gust od innych,
        widać na pierwszy rzut oka, że produkcje obejrzane na tym koncie tworzą oddzielny klaster."
      )
    ),
    mainPanel(
      shinycssloaders::withSpinner(
        shinyjqui::jqui_resizable(visNetworkOutput("graph_vis")),
        type = 4,
        color = "#b1060f",
        size=0.5)
      
    )
  )
)

ui <- navbarPage(
  title = div(img(src="logo.png",
                  style="margin-top: 12px;
                               padding-right:10px;
                               padding-bottom:10px",
                  height = 60)),
  windowTitle="Netflix App",
  tabPanel("Częstotliwość oglądania", ui1, icon = icon("video")),
  tabPanel("Ranking ulubionych tytułów", ui2, icon = icon("ranking-star")),
  tabPanel("Autoodtwarzanie", ui3, icon = icon("rotate")),
  tabPanel("Graf zależności", ui4, icon = icon("share-nodes")),
  footer = shiny::HTML("
                  <footer class='text-center text-sm-start' style='width:100%;'>
                  <hr>
                  <p class='text-center' style='font-size:12px;'>
                    © 2022 Autorzy:
                    <a class='text-dark' href='https://github.com/gogolon'>Jakub Lange</a>,
                    <a class='text-dark' href='https://github.com/Koliber216'>Mateusz Kubita</a>,
                    <a class='text-dark' href='https://github.com/AKapich'>Aleks Kapich</a>
                  </p>
                  </footer>
                  "),
  theme = bslib::bs_theme(bootswatch = "cyborg"))


server <- function(input, output) {
  
  output$frequencyPlot <- renderPlotly({
    
    df <- ViewingActivity %>%
      filter(Profile.Name==input$account1) %>%
      select(c('Start.Time', 'Duration')) %>%
      mutate(Start.Time = as.Date(word(Start.Time, 1))) %>%
      tidyr::complete(Start.Time = seq.Date(min(Start.Time), max(Start.Time), by="day")) %>%
      mutate(Duration = ifelse(is.na(Duration), hms::hms(0), hms::as_hms(Duration))) %>%
      group_by(Start.Time) %>%
      summarize(Duration = sum(Duration) / 60)
    
    fig <- plot_ly(df, x = ~Start.Time) %>% config(locale = 'pl')
    fig <- fig %>% add_lines(line = list(color = '#b1060f'),
                             y = ~Duration,
                             name = '',
                             customdata=hms::hms(round(df$Duration * 60)),
                             hovertemplate = paste('%{x}:<br>', 'Spędzony czas: %{customdata}','<extra></extra>',
                                                   sep = ''))
    fig <- fig %>% layout(
      title = 'Spędzony czas',
      font = list(color = 'white'),
      xaxis = list(title = 'Zakres czasu', rangeslider = list(type = "date")),
      yaxis = list(title = 'Minuty', range=c(0,1000)),
      plot_bgcolor='transparent',
      paper_bgcolor='transparent')
  })
  
  
  output$bumpPlot <- renderPlot({
    WYBRANY_ROK <- input$year
    WYBRANA_CZESC_ROKU <- c((1+as.numeric(input$half)):(6+as.numeric(input$half)))
    
    df_nf <- ViewingActivity %>%
      mutate(miesiac = stri_sub(Start.Time, 6,7),
             rok = stri_sub(Start.Time, 1,4),
             Type = case_when(
               !(stringr::str_detect(Title, "Odcinek")) ~ "Movie",
               stringr::str_detect(Title, "Odcinek") ~ "Series"),
             Title.General = sub("\\:.*", "", Title),
             Duration_minutes = round(lubridate::period_to_seconds(lubridate::hms(Duration))/60, 2)) %>%
      filter(Type=="Series")
    
    df_nf <- df_nf %>% 
      group_by(Title.General,miesiac,rok) %>% 
      summarise(suma_duration = sum(Duration_minutes))
    
    df_nf$miesiac <- as.numeric(as.character(df_nf$miesiac))
    
    df_nf <- df_nf %>% 
      filter(suma_duration >10,
             rok==WYBRANY_ROK,
             miesiac %in% WYBRANA_CZESC_ROKU) %>% 
      arrange(miesiac, -suma_duration)
    
    df_nf <- df_nf %>% 
      group_by(miesiac) %>% 
      mutate(rank = rank(-suma_duration, ties.method = "random")) %>% 
      ungroup()
    
    df_nf <- df_nf %>% 
      filter(Title.General %in% (df_nf %>% 
                                   group_by(Title.General) %>% 
                                   summarise(n = n()) %>% 
                                   arrange(-n) %>% 
                                   head(5) %>% 
                                   pull(Title.General)))
    
    df_nf <- df_nf %>% 
      group_by(miesiac) %>% 
      mutate(rank = rank(-suma_duration, ties.method = "random")) %>% 
      ungroup()
    
    
    firstC <- df_nf %>% 
      group_by(Title.General) %>% 
      summarise(n = n()) %>% 
      arrange(-n) %>% 
      head(5) %>% 
      pull(Title.General)
    
    secondC <- WYBRANA_CZESC_ROKU
    thirdC <- rep(0,length(WYBRANA_CZESC_ROKU))
    
    df_to_join <- data.frame(miesiac = rep(secondC,5), 
                             Title.General = rep(firstC,length(WYBRANA_CZESC_ROKU)), 
                             suma_duration = rep(thirdC,5))
    
    df_nf <- left_join(df_to_join,df_nf,
                       by = c('Title.General' ='Title.General', 'miesiac' = 'miesiac'))
    
    df_nf[is.na(df_nf)] = 0
    
    df_nf <- df_nf %>% 
      select(Title.General,
             miesiac,
             suma_duration.y,
             rank )
    
    #rerank them
    df_nf <- df_nf %>% 
      group_by(miesiac) %>% 
      mutate(rank = rank(-suma_duration.y, ties.method = "random")) %>% 
      ungroup()
    
    df_nf %>% 
      ggplot(aes(miesiac, rank, group = Title.General, color = Title.General, fill = Title.General)) +
      geom_bump(aes(smooth = 10), size = 3, lineend = "round") +
      geom_point(size = 6, show.legend = F) +
      scale_color_manual(values = c(wesanderson::wes_palette("GrandBudapest2"), wesanderson::wes_palette("GrandBudapest1"), wesanderson::wes_palette("BottleRocket2"))) +
      scale_fill_manual(values = c(wesanderson::wes_palette("GrandBudapest2"), wesanderson::wes_palette("GrandBudapest1"), wesanderson::wes_palette("BottleRocket2"))) +
      scale_y_reverse(breaks = 1:100) +
      scale_x_continuous(breaks = df_nf$miesiac %>% unique()) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            text = element_text(color = "white"),
            plot.title = element_text(hjust =0.5,size = 26),
            axis.text = element_text(size=15, colour = 'white'),
            axis.title = element_text(size=17),
            legend.key.size = unit(1,'cm'),
            legend.title = element_text(size=16),
            legend.text = element_text(size = 14)) +
      labs(title = "Popularność seriali wśród wszystkich profili",
           x="Miesiąc",
           y= "Ranking",
           color ="Serial") +
      scale_color_brewer(palette = "RdBu")
    
  }, bg='transparent')
  
  
  output$alluvialPlot <- renderPlot({
    
    df <- ViewingActivity %>% 
      filter(Profile.Name==input$account3,
             Supplemental.Video.Type=="") %>%
      mutate(Type = case_when(
        !(stringr::str_detect(Title, "Odcinek")) ~ "Film",
        stringr::str_detect(Title, "Odcinek") ~ "Serial"),
        Title.General = sub("\\:.*", "", Title),
        Duration.Seconds = lubridate::period_to_seconds(lubridate::hms(Duration)),
        Autoplayed = case_when(
          !(stringr::str_detect(Attributes, "Autoplayed")) ~ "Nie",
          stringr::str_detect(Attributes, "Autoplayed") ~ "Tak"),
        Year = substr(Start.Time, 1, 4)
      ) %>%
      group_by(Title.General, Autoplayed, Year, Type) %>% 
      summarise(Duration.General = sum(Duration.Seconds))
    
    ggplot(df,
           aes(y = Duration.General, axis1 = Type, axis2 = Year)) +
      geom_alluvium(aes(fill = Autoplayed), width = 1/12) +
      geom_stratum(width = 1/12, fill = "#b1060f", color = "white") +
      geom_label(color ='white', fill='transparent', stat = "stratum", aes(label = after_stat(stratum)), label.size = NA) +
      scale_fill_manual(values = c("#940404", "#6696e3"), name = "Autoodtwarzanie") +
      theme(
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill='transparent'), 
        legend.box.background = element_rect(fill='transparent'),
        legend.text = element_text(color = 'white', size=14),
        legend.title = element_text(color = 'white', size=16),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
      )
  }, bg="transparent")
  
  output$graph_vis <- renderVisNetwork({
    
    df_nf <- ViewingActivity %>%
      filter(Profile.Name != "Iza") %>% 
      mutate(Profile.Name = case_when(
        Profile.Name == "Ewa K" ~ "A",
        Profile.Name == "Ewa" ~ "B",
        Profile.Name == "Debi"~ "C")) %>% 
      select(Profile.Name, Title) %>% 
      distinct()
    
    colnames(df_nf) <- c("from", "to")
    
    # create graph object
    karate <- igraph::graph_from_data_frame(df_nf, 
                                            directed = FALSE)
    
    # different colors and shapes
    V(karate)$color <- ifelse(V(karate)$name %in% c("A", "B", "C"),
                              "#6696e3", 
                              "white")
                              
    V(karate)$shape <- "circle"
    
    V(karate)$font.size = ifelse(V(karate)$name %in% c("A", "B", "C"),
                                 150, 
                                 0)
    
    # more visible edges 
    E(karate)$color = "#b1060f"
      E(karate)$width <- 1
      
      background2 = "rgba(187, 153, 171, 1)"
      visIgraph(karate, layout = "layout_with_fr",
                randomSeed = 420) %>% 
        visEdges(arrows = "to",
                 smooth = list(enabled = TRUE, type = "curvedCW")) %>% 
        visLayout(improvedLayout = TRUE)
  })
}

shinyApp(ui = ui, server = server)