library(shiny)
library(dplyr)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(shinycssloaders)
library(reshape2)
library(tidyquant)


df <- read.csv("dane.csv")

wybrane_kierunki <- c("Slawistyka",
                     "Studia amerykanistyczne",
                     "Filologia włoska",
                     "Filologia romańska",
                     "Germanistyka",
                     "Muzykologia",
                     "Filologia angielska",
                     "Filologia polska")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

server <- function(input, output, session) {

  output$zarobki <- renderPlot({
    pomocnicza <- df[df$P_ROKDYP == input$rok,]
    pomocnicza <- pomocnicza[pomocnicza$P_KIERUNEK_NAZWA%in% input$kierunki, ]
    pomocnicza <- pomocnicza %>%
      group_by(P_KIERUNEK_NAZWA) %>%
      top_n(1, P_N_WZUS) %>% as.data.frame()
    pomocnicza <- pomocnicza[,c(ncol(pomocnicza)-15-1, 551:610)]
    pomocnicza <- t(pomocnicza)
    pomocnicza <- melt(pomocnicza ,  id.vars = colnames(pomocnicza))
    mapowanie <- pomocnicza[pomocnicza$Var1 == "P_KIERUNEK_NAZWA", c(2,3)]
    dane <- pomocnicza[pomocnicza$Var1 != "P_KIERUNEK_NAZWA",]
    dane_z_mapowaniem <- merge(x=dane,y=mapowanie, 
                               by="Var2", all.x=TRUE)
  
    y_limit =  quantile(as.numeric(sub(",", ".", dane_z_mapowaniem$value.x, fixed = TRUE)), c(0, .20, .40, .60, .80 , 1), na.rm = TRUE) 
    
    ggplot(dane_z_mapowaniem, aes(Var1, value.x, group=factor(value.y))) + 
      geom_ma(aes(color=factor(value.y)), ma_fun = SMA, n = 3, lwd=2, linetype=1) +
      theme_bw() +
      xlab("Miesiące od ukończenia licencjatu") +
      ylab("Średnia krocząca względnego wskażnika \nzarobków z 3 miesięcy")  + 
      scale_y_discrete(breaks=as.character(sub(".", ",", y_limit, fixed = TRUE))) +
      scale_x_discrete(breaks=c("P_WWZ_MIES_12","P_WWZ_MIES_24","P_WWZ_MIES_36","P_WWZ_MIES_48","P_WWZ_MIES_60"),
                       labels=c("12", "24", "36","48","60")) +
      theme(axis.text=element_text(size=12), 
            axis.title=element_text(size=12),
            legend.title=element_text(size=14), 
            legend.text=element_text(size=12)) +
      scale_colour_manual(values=cbPalette) +
      labs(colour = "Nazwa kierunku")  
  })
  
  output$bezrobocie <- renderPlot({
    pomocnicza2 <- df[df$P_ROKDYP == input$rok2,]
    pomocnicza2 <- pomocnicza2[pomocnicza2$P_KIERUNEK_NAZWA%in% input$kierunki2, ]
    pomocnicza2 <- pomocnicza2 %>%
      group_by(P_KIERUNEK_NAZWA) %>%
      top_n(1, P_N_WZUS) %>% as.data.frame()
    pomocnicza2 <- pomocnicza2[,c(ncol(pomocnicza2)-15-1, 611:670)]
    pomocnicza2 <- t(pomocnicza2)
    
    pomocnicza2 <- melt(pomocnicza2 ,  id.vars = colnames(pomocnicza2))
    mapowanie2 <- pomocnicza2[pomocnicza2$Var1 == "P_KIERUNEK_NAZWA", c(2,3)]
    dane2 <- pomocnicza2[pomocnicza2$Var1 != "P_KIERUNEK_NAZWA",]
    dane_z_mapowaniem2 <- merge(x=dane2,y=mapowanie2, 
                               by="Var2", all.x=TRUE)
    
    tmp <- as.numeric(sub(",", ".", dane_z_mapowaniem2$value.x, fixed = TRUE))
    tmp <- tmp[tmp != 0]
    y_limit2 =  quantile(tmp, c(0, .40, .60, .80, .90 , 1), na.rm = TRUE) 
    
    
    ggplot(dane_z_mapowaniem2, aes(Var1, value.x, group=factor(value.y))) + 
      geom_ma(aes(color=factor(value.y)), ma_fun = SMA, n = 3, lwd=2, linetype=1) +
      theme_bw() +
      xlab("Miesiące od ukończenia licencjatu") +
      ylab("Średnia krocząca względnego wskażnika \nbezrobocia z 3 miesięcy")   + 
      scale_y_discrete(breaks=as.character(sub(".", ",", y_limit2, fixed = TRUE))) +
      scale_x_discrete(breaks=c("P_WWB_MIES_12","P_WWB_MIES_24","P_WWB_MIES_36","P_WWB_MIES_48","P_WWB_MIES_60"),
                       labels=c("12", "24", "36","48","60")) +
      theme(axis.text=element_text(size=12), 
            axis.title=element_text(size=12),
            legend.title=element_text(size=14), 
            legend.text=element_text(size=12)) + 
      scale_colour_manual(values=cbPalette) +
      labs(colour = "Nazwa kierunku") 
  })
}


ui1 <- fluidPage(titlePanel("Zarobki"),
                 shiny::markdown(
                   "Względny wskażnik zarobków pokazuje jak wynagrodzenie absolwenta ze wszystkich źródeł ma się do średnich zarobków w jego miejscu zamieszkania. Im większa wartość tym lepiej. 
                    Wartości powyżej 1 oznaczają, że przeciętnie absolwenci zarabiają powyżej średniej wynagrodzeń w swoich miejscach zamieszkania."
                 ),
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("rok", "Rok otrzymania licencjatu: ", c("2015", "2016", "2017", "2018", "2019", "2020")),
                     checkboxGroupInput(
                       inputId = "kierunki",
                       label = "Kierunki: ",
                       choices = wybrane_kierunki,
                       selected = wybrane_kierunki),
                     width = 3
                   ),
                   mainPanel(shinycssloaders::withSpinner(
                             plotOutput("zarobki"),
                             type = 4,
                             color = "#000080",
                             size = 1.5),
                             width = 9)
                 ))

ui2 <- fluidPage(titlePanel("Bezrobocie"),
                 shiny::markdown(
                   "Względny wskażnik bezrobocia pokazuje jak bezrobocie absolwentów ma się do stopy bezrobocia w ich miejscu zamieszkania. Im mniejsza wartość tym lepiej. 
                   Wartości poniżej 1 oznaczają, że przeciętnie bezrobocie wśród absolwentów jest niższe niż stopa bezrobocia w ich miejscu zamieszkania."
                 ),
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("rok2", "Rok otrzymania licencjatu: ", c("2015", "2016", "2017", "2018", "2019", "2020")),
                     checkboxGroupInput(
                       inputId = "kierunki2",
                       label = "Kierunki: ",
                       choices = wybrane_kierunki,
                       selected = wybrane_kierunki),
                     width = 3
                   ),
                   mainPanel(shinycssloaders::withSpinner(
                     plotOutput("bezrobocie"),
                     type = 4,
                     color = "#000080",
                     size = 1.5),
                     width = 9)
                   
                 ))

app_ui <- navbarPage(
  title = span( "Analiza ekonomicznych losów absolwentów wybranych kierunków humanistycznych na Uniwersytecie Warszawskim"),
  tabPanel("Zarobki", ui1, icon = icon(name = "glyphicon glyphicon-usd", lib = "glyphicon")),
  tabPanel("Bezrobocie", ui2, icon = icon(name = "glyphicon glyphicon-briefcase", lib = "glyphicon")),
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  footer = shiny::HTML("
                   <footer class='text-center text-sm-start' style='width:100%;background-color: rgb(54, 58, 60)'>
                   <hr>
                   <p class='text-center' style='font-size:12px;color:white'>
                     © 2022 Copyright: Mateusz Nizwantowski
                   </p>
                   </footer>
                   "),
  header = tags$head(tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css"))
)


shinyApp(app_ui, server)
