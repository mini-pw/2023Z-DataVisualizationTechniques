#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(ggplot2)
library(forcats)
library(data.table)
library(wordcloud2)
library(RColorBrewer)
library(reshape2)
# Let's get the data
#setwd("D:/IAD/sem3/wizualizacja danych/projekt2")


#we used fread from data.table package, it's faster than fread
magda_merged_message_to_work <- na.omit(fread("magda_merged_message_to_work.csv",encoding = "UTF-8"))
magda_merged_common_words <- na.omit(fread("Magdalena Jeczeń_merged_common_words.csv",encoding = "UTF-8"))
ola_merged_message_to_work <- na.omit(fread("ola_merged_message_to_work.csv",encoding = "UTF-8"))
ola_merged_common_words <- na.omit(fread("Aleksandra Kulczycka_merged_common_words.csv",encoding = "UTF-8"))
lukasz_merged_message_to_work <- na.omit(fread("lukasz_merged_message_to_work.csv",encoding = "UTF-8"))
lukasz_merged_common_words <- na.omit(fread("Łukasz Grabarski_merged_common_words.csv",encoding = "UTF-8"))
magda_merged_time_to_work <- na.omit(fread("magda_merged_time_to_work.csv",encoding = "UTF-8"))
ola_merged_time_to_work <- na.omit(fread("ola_merged_time_to_work.csv",encoding = "UTF-8"))
lukasz_merged_time_to_work <- na.omit(fread("lukasz_merged_time_to_work.csv",encoding = "UTF-8"))

################################################################################
#####################              UI                ###########################
################################################################################
# Define UI1 for application 
ui1 <- fluidPage(
  
  # Application title
  titlePanel("Witamy na stronie głównej."),
  mainPanel(
    h6("Nasza aplikacja stworzona została na potrzeby przedmiotu Techniki Wizualizacji Danych, w ramach projektu dotyczącego naszych przyzwyczajeń, preferencji oraz zachowań. Przedstawiliśmy w niej statystyki danych zaczerpniętych z aplikacji messenger z ostatnich 5-8 lat użytkowania. 
             Dla wizualizacji wyników stworzyliśmy wykresy w oparciu o najciekawsze naszym zdaniem informacje. Nazywamy się:"),
    fluidRow(
      column(3,h1("Ola"),
             img(src = "bajka.png",  height = 140, width = 140)),
      column(3,
             h2("Magda"),
             img(src = "brawurka.png", height = 140, width = 140)),
      
      column(3,
             h3("Łukasz"),
             img(src = "bojka.png", height = 140, width = 140))),
    h6("Razem tworzymy atoMiNIówki i dzielnie stawiamy czoła wszystkim wizualizacjom!")
    
  ))


# Define UI2 for application 
ui2 <- fluidPage(
  
  # Application title
  titlePanel("Udział w konwersacji zależnie od liczby osób"),
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      
      shiny::markdown(
        "Wykres przedstawia procentowy udział w konwersacjach grupowych wybranego użytkownika w zależności od 
          liczby członków. Możemy zauważyć, kto z nas jest najmniej 'wygadaną' osobą, kto jest nieśmiały w większych grupach,
          a także komu zdarza się zazwyczaj nie odpisywać na wiadomości...")
      
    ),
    
    mainPanel(shinycssloaders::withSpinner(plotly::plotlyOutput("myPlot2", height = 400),
                                           type = getOption("spinner.type", default = 8),
                                           color = getOption("spinner.color", default = 'pink'),
                                           color.background = getOption("spinner.color.background", default ='white'))
              
    )
  ))

# Define UI3 for application 
ui3 <- fluidPage(
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: purple}")),
  
  
  # Application title
  titlePanel("Liczba wiadomości w zależności od pory dnia"),
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "year",
        "wybierz rok",
        min = 2015,
        max = 2022,
        value = c(2015,2022),
        step = 1,
        sep =''),
      sliderInput(
        "day",
        "wybierz miesiąc",
        min = 1,
        max = 12,
        value = c(1,12),
        step = 1
      ),
      shiny::markdown(
        "Wykres przedstawia średnią liczbę wiadomości wysyłanych oraz odbieranych przez użytkownika w konkretnych godzinach na przestrzeni całego dnia. Dane przedstawione są w zależności od wybranego zakresu dat: miesięcy, a także lat począwszy od 2015 roku.
        Doskonale odwzorowuje to zmiany w naszych codziennych nawykach komunikacyjnych wraz z dojrzewaniem, a także np rożnicami pomiędzy okresami świątecznymi/wakacyjnymi, czy rokiem szkolnym.")
      
    ),
    
    mainPanel(shinycssloaders::withSpinner(plotly::plotlyOutput("myPlot3", height = 450),
                                           type = getOption("spinner.type", default = 8),
                                           color = getOption("spinner.color", default = 'pink'),
                                           color.background = getOption("spinner.color.background", default ='white'))
              
    )
  ))
# Define UI2 for application 
ui4 <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("text", label = h3("wpisz nazwę konwersacji"), value = "Ela Kulczycka"),
      
      
      shiny::markdown(
        "Wykres przedstawia słowa z największą różnicą występowania pomiędzy uczestnikami konwersacji. Dla zobrazowania użytkownik może wpisać np.: Dla Oli 'Magdalena Jeczeń'. Dla Łukasza 'Aleksandra Kulczycka'. Dla Magdy 'Łukasz Grabarski'"))
    ,
    mainPanel(shinycssloaders::withSpinner(plotly::plotlyOutput("myPlot4",height =400),
                                           type = getOption("spinner.type", default = 8),
                                           color = getOption("spinner.color", default = 'pink'),
                                           color.background = getOption("spinner.color.background", default ='#FFDFF8'))
    )))



# Merge two UI in one app, choose theme and add a footer with the link
app_ui <- navbarPage(
  collapsible=TRUE,
  title = div(h3("Messenger Statystyki")),
  selectInput("person",
              "Wybierz użytkownika:",
              choices = c("Ola" = "ola",
                          "Magda" = "magda",
                          "Łukasz" = "lukasz")),
  tabPanel("menu",icon = icon("home"), ui1),
  
  tabPanel("aktywność na forum",icon = icon("user-group"), ui2),
  tabPanel("schemat dzienny",icon = icon("hourglass-start"), ui3),
  tabPanel("zróżnicowane słownictwo",icon = icon("font"), ui4),
  theme = bslib::bs_theme( bg = "#FFFFFF", fg = "black", primary = "#FFB5EE", bootswatch = "sketchy"
  ),
  footer = shiny::HTML("
                  <footer class='text-center text-sm-start' style='width:100%;'>
                  <hr>
                  <p class='text-center' style='font-size:12px;'>
                    © 2022 Copyright:
                    <a class='text-dark' href='https://github.com/akulczycka'>Aleksandra</a>
                    <a class='text-dark' href='https://github.com/m24jeczen'>Magdalena</a>
                    <a class='text-dark' href='https://github.com/LukaszGrabarski'>Łukasz</a>

                  </p>
                  </footer>
                  ")
)
# Define server logic required to draw plots
server <- function(input, output) {
  output$value <- renderPrint({ input$text })
  
  
  ##nie zadziałało, ale miało perspektywy przed sobą
  
  # data <- reactiveValues(merged_message = ola_merged_message)
  # 
  # observeEvent( input$person, {
  #   if(input$person == "ola"){
  #      merged_message <- ola_merged_message}
  #   else if (input$person == "lukasz"){
  #     merged_message <- lukasz_merged_message}
  #   else if(input$person == "magda"){
  #     merged_message <- magda_merged_message}
  # })
  
  
  output$myPlot2 <- renderPlotly({
    
    p11 <- ola_merged_message_to_work %>% 
      group_by(participants_grouped) %>%
      summarise(p_of_my_mess = (sum(my_messages)/sum(all_messages))*100)%>%
      ggplot(aes(x = fct_relevel(participants_grouped, c("2","[3,5]","[6,10]","[11,20]",">20")), y = p_of_my_mess) )+
      geom_col(fill = "lightblue") +
      theme_minimal() +
      labs(title = "Udział procentowy w konwersacjach zależnie od liczby członków",
           x = "Liczba osób w konwersacji",
           y = "% moich wiadomości") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    p12 <- magda_merged_message_to_work %>% 
      group_by(participants_grouped) %>%
      summarise(p_of_my_mess = (sum(my_messages)/sum(all_messages))*100)%>%
      ggplot(aes(x = fct_relevel(participants_grouped, c("2","[3,5]","[6,10]","[11,20]",">20")), y = p_of_my_mess) )+
      geom_col(fill = "lightblue") +
      theme_minimal() +
      labs(title = "Udział procentowy moich wiadomości w konwersacjach zależnie od liczby osób w grupie",
           x = "Liczba osób",
           y = "% moich wiadomości") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    p13 <- lukasz_merged_message_to_work %>% 
      group_by(participants_grouped) %>%
      summarise(p_of_my_mess = (sum(my_messages)/sum(all_messages))*100)%>%
      ggplot(aes(x = fct_relevel(participants_grouped, c("2","[3,5]","[6,10]","[11,20]",">20")), y = p_of_my_mess) )+
      geom_col(fill = "lightblue") +
      theme_minimal() +
      labs(title = "Udział procentowy moich wiadomości w konwersacjach zależnie od liczby osób w grupie",
           x = "Liczba osób",
           y = "% moich wiadomości") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    switch(input$person,
           "ola" = p11,
           "magda" = p12,
           "lukasz" = p13)
    
  })
  
  output$myPlot3 <- renderPlotly({
    
    
    p31 <- ola_merged_time_to_work %>% 
      filter(year_only == input$year) %>% 
      filter(month_only == input$day) %>% 
      group_by(hour_only) %>% 
      summarise(n_of_messages = n()) %>% 
      arrange(hour_only) %>% 
      ggplot(aes(x = hour_only, y = n_of_messages)) +
      geom_col(fill = "lightblue") + 
      theme_minimal() +
      scale_x_continuous(breaks = 0:23) + 
      labs(title = "Liczba wiadomości zależnie od h",
           x = "Godzina",
           y = "Liczba wiadomości") + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    p32 <- magda_merged_time_to_work %>% 
      filter(year_only == input$year) %>% 
      filter(month_only == input$day) %>% 
      group_by(hour_only) %>% 
      summarise(n_of_messages = n()) %>% 
      arrange(hour_only) %>% 
      ggplot(aes(x = hour_only, y = n_of_messages)) +
      geom_col(fill = "lightblue") + 
      theme_minimal() +
      scale_x_continuous(breaks = 0:23) + 
      labs(title = "Liczba wiadomości zależnie od h",
           x = "Godzina",
           y = "Liczba wiadomości") + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    p33 <- lukasz_merged_time_to_work %>% 
      filter(year_only == input$year) %>% 
      filter(month_only == input$day) %>% 
      group_by(hour_only) %>% 
      summarise(n_of_messages = n()) %>% 
      arrange(hour_only) %>% 
      ggplot(aes(x = hour_only, y = n_of_messages)) +
      geom_col(fill = "lightblue") + 
      theme_minimal() +
      scale_x_continuous(breaks = 0:23) + 
      labs(title = "Liczba wiadomości zależnie od h",
           x = "Godzina",
           y = "Liczba wiadomości") + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    switch(input$person,
           "ola" = p31,
           "magda" = p32,
           "lukasz" = p33)
    
  })
  output$myPlot4 <- renderPlotly({
    
    example_file <- input$text
    merged_common_words <- ola_merged_common_words
    words_to_use <- merged_common_words %>% 
      filter(file == example_file) %>% 
      mutate(roznica = abs(my_count - other_count)) %>% 
      arrange(desc(roznica)) %>% 
      head(5)
    words_to_use <- words_to_use$word
    
    merged_common_words_to_work <- merged_common_words %>% 
      filter(file == example_file) %>% 
      filter(word %in% words_to_use)
    
    
    dfm <- melt(merged_common_words_to_work[,c('word','my_count','other_count')],id.vars = 1)
    p41 <- ggplot(dfm, aes(x = word, y = value)) + 
      geom_bar(aes(fill = variable),position="stack", stat="identity" ) +
      scale_y_log10() + 
      theme_minimal() +
      labs(title = "Słowa z największą różnicą występowania",
           x = "Słowo",
           y = "Ile razy słowo zostało użyte w konwersacji") + 
      scale_fill_discrete(NULL) +
      theme(legend.title = element_blank()) + 
      scale_fill_discrete(labels=c('Ja', 'Uczestnik konwersacji'))
    p41
    
    merged_common_words <- magda_merged_common_words
    words_to_use <- merged_common_words %>% 
      filter(file == example_file) %>% 
      mutate(roznica = abs(my_count - other_count)) %>% 
      arrange(desc(roznica)) %>% 
      head(5)
    words_to_use <- words_to_use$word
    
    merged_common_words_to_work <- merged_common_words %>% 
      filter(file == example_file) %>% 
      filter(word %in% words_to_use)
    
    
    dfm <- melt(merged_common_words_to_work[,c('word','my_count','other_count')],id.vars = 1)
    p42 <- ggplot(dfm, aes(x = word, y = value)) + 
      geom_bar(aes(fill = variable),position="stack", stat="identity" ) +
      scale_y_log10() + 
      theme_minimal() +
      labs(title = "Słowa z największą różnicą występowania",
           x = "Słowo",
           y = "Ile razy słowo zostało użyte w konwersacji") + 
      scale_fill_discrete(NULL) +
      theme(legend.title = element_blank()) + 
      scale_fill_discrete(labels=c('Ja', 'Uczestnik konwersacji'))
    p42
    
    merged_common_words <- lukasz_merged_common_words
    words_to_use <- merged_common_words %>% 
      filter(file == example_file) %>% 
      mutate(roznica = abs(my_count - other_count)) %>% 
      arrange(desc(roznica)) %>% 
      head(5)
    words_to_use <- words_to_use$word
    
    merged_common_words_to_work <- merged_common_words %>% 
      filter(file == example_file) %>% 
      filter(word %in% words_to_use)
    
    
    dfm <- melt(merged_common_words_to_work[,c('word','my_count','other_count')],id.vars = 1)
    p43 <- ggplot(dfm, aes(x = word, y = value)) + 
      geom_bar(aes(fill = variable),position="stack", stat="identity" ) +
      scale_y_log10() + 
      theme_minimal() +
      labs(title = "Słowa z największą różnicą występowania",
           x = "Słowo",
           y = "Ile razy słowo zostało użyte w konwersacji") + 
      theme(legend.title = element_blank()) + 
      scale_fill_discrete(labels=c('Ja', 'Uczestnik konwersacji'))
    p43
    # p41 <- filter(ola_merged_message_to_work, participants > 5)%>%
    #   ggplot(aes(file, participants)) +
    #   geom_point(color = "lightblue") +
    #   theme_minimal() +
    #   labs(title = "Liczba osób w konwersacjach grupowych powyżej 5 członków",
    #        x = "konwersacja",
    #        y = "Liczba osób w konwersacji") + 
    #   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x=element_blank(),
    #         axis.ticks.x=element_blank())
    # p42 <- filter(magda_merged_message_to_work, participants > 5)%>%
    #   ggplot(aes(file, participants)) +
    #   geom_point(color = "lightblue") +
    #   theme_minimal() +
    #   labs(title = "Liczba wiadomości zależnie od h",
    #        x = "konwersacja",
    #        y = "Liczba osób w konwersacji") + 
    #   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x=element_blank(),
    #         axis.ticks.x=element_blank())
    # p43 <- filter(lukasz_merged_message_to_work, participants > 5)%>%
    #   ggplot(aes(file, participants)) +
    #   geom_point(color = "lightblue") +
    #   theme_minimal() +
    #   labs(title = "Liczba wiadomości zależnie od h",
    #        x = "konwersacja",
    #        y = "Liczba osób w konwersacji") + 
    #   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x=element_blank(),
    #         axis.ticks.x=element_blank())
    
    
    switch(input$person,
           "ola" = p41,
           "magda" = p42,
           "lukasz" = p43)
  })
}


# Run the application 
shinyApp(ui = app_ui, server = server)
