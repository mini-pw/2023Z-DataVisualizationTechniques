library(ggplot2)
library(dplyr)
library(lubridate)
library(zoo)
library(shiny)
library(bslib)
library(circular)

# Functions
cols = c("#375A7F", "#CE5A7F")

# all messages
get_data <- function(path){
  all_messages<-read.csv(path, header=F)
  
  all_messages<-all_messages %>% 
    rename("sender_name" = "V1",
           "timestamp_ms" = "V2",
           "content" = "V3",
           "convo_name" = "V4",
           "number_of_participants" = "V5") %>% 
    mutate(time_normal = as_datetime(timestamp_ms/1000),
           time_normal = with_tz(time_normal, "Europe/Amsterdam"),
           floordate = floor_date(time_normal, "month")) %>% 
    mutate(hour = hour(time_normal),
           year = substring(time_normal, 1,4),
           month = substring(time_normal, 6,7),
           month_date = as.Date(as.yearmon(paste0(year, "-", month))))
  all_messages <- all_messages %>% 
    mutate(group = ifelse(number_of_participants > 2, TRUE, FALSE))
  
  rename_convo_name(all_messages) -> all_messages
  
  v <- all_messages %>%
    filter(hour == 0)
  v <- v %>%
    mutate(hour = 24)
  rbind(all_messages, v) -> all_messages
  
  all_messages
}

# rename convo names
rename_convo_name <- function(all_messages){
  groups <- get_group_names(all_messages)
  privs <- get_non_group_names(all_messages)
  
  g <- 1:length(groups)
  p <- 1:length(privs)
  
  G <- data.frame(groups, g)
  P <- data.frame(privs, p)
  
  G %>% 
    mutate(g = paste("group", g, sep = "_")) -> G
  
  P %>% 
    mutate(p = paste("priv", p, sep = "_")) -> P
  
  
  P %>% 
    rename(groups = privs, g = p) %>% 
    add_row(G) %>% 
    rename(old = groups, new = g) -> N
  
  
  left_join(all_messages, N, by = c("convo_name" = "old")) %>% 
    mutate(convo_name = new) %>% 
    select(-new) -> all_messages
  all_messages
}

# group names
get_group_names <- function(all_messages){
  convos <- all_messages %>% 
    select(convo_name, number_of_participants, group) %>% 
    group_by(convo_name) %>% 
    summarise(group = max(group))
  group_names <- convos %>% 
    filter(group == TRUE)
  group_names <- group_names$convo_name
  group_names
}

# non group names
get_non_group_names <- function(all_messages){
  convos <- all_messages %>% 
    select(convo_name, number_of_participants, group) %>% 
    group_by(convo_name) %>% 
    summarise(group = max(group))
  conversation_names <- convos %>% 
    filter(group == FALSE)
  conversation_names <- conversation_names$convo_name
  conversation_names
}

# select data frame
select_data <- function(all_messages_k, all_messages_j, input_name){
  if(input_name == name_j){
    all_messages <- all_messages_j
  }
  else if(input_name == name_k){
    all_messages <- all_messages_k
  }
  else{
    all_messages <- add_row(all_messages_j, all_messages_k)
  }
  all_messages
}

# get top 25 most common convos
number <- 25
get_common_convos <- function(all_messages, input_name){
  common_convos<-(select_data(all_messages_k, all_messages_j, input_name) %>% 
                    filter(sender_name == input_name) %>% 
                    count(convo_name) %>% 
                    arrange(-n) %>% 
                    head(number))$convo_name
}

# filter by name
get_names <- function(input_name){
  names <- c(name_k, name_j)
  if(input_name == name_k){
    names <- name_k
  }
  else if(input_name == name_j){
    names <- name_j
  }
  names
}


# Preparing data
# odkomentować, gdy będą ścieżki
# path <- "./all_messages.csv"
# all_messages_k <- get_data(path)
# group_names_k <- get_group_names(all_messages_k)
# non_group_names_k <- get_non_group_names(all_messages_k)

path <- "./all_messages_j.csv"
all_messages_j <- get_data(path)
group_names_j <- get_group_names(all_messages_j)
non_group_names_j <- get_non_group_names(all_messages_j)
name_k <- "Kuba Seliga"
name_j <- "Jakub Sawicki"


# Pages
ui1 <- fluidPage(
  
  # Page title
  titlePanel("Konwersacje grupowe vs prywatne"),
  
  textOutput("text1"),
  
  # choosing input
  sidebarLayout(
    sidebarPanel(
      
      selectInput("name1", "Wybierz osobę", c(name_k, name_j, "łącznie")),
      
      radioButtons(
        "plot1type",
        "Zmień typ wykresu:",
        choices = c("Wykres pokazujący ilość wiadomości" = "count",
                    "Wykres zsumowany do 100%" = "fill"),
        selected = c("count")
      ),
      
      sliderInput(
        "slider1date",
        "Wybierz badany zakres",
        
        min = as.Date("2016-01-01"),
        max = as.Date("2023-01-01"),
        value=c(as.Date("2016-01-01"), as.Date("2023-01-01")),
        timeFormat="%b %Y"),
      textOutput("SliderText"),
      
      width = 3
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("groups_vs_non_groups")
    )
  )
)


ui4 <- fluidPage(titlePanel("Nasz udział w naszych najczęstszych rozmowach"),
                 textOutput("text2"),
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("name4", "Wybierz osobę", c(name_k, name_j)),
                     
                     sliderInput(
                       "slider2date",
                       "Wybierz badany zakres",
                       
                       min = as.Date("2016-01-01"),
                       max = as.Date("2023-01-01"),
                       value=c(as.Date("2016-01-01"), as.Date("2023-01-01")),
                       timeFormat="%b %Y",
                       step = 31,
                       #animate = animationOptions(step = as.Date(1)))
                       #animate = T
                       animate = animationOptions(interval = 500)
                     ),
                     
                     selectInput(
                       "showMax",
                       "Pokaż maksymalnie:",
                       choices = c("5", "10", "15", "20", "25"),
                       selected = "10"
                     ),
                     
                     width = 3
                   ),
                   mainPanel <- mainPanel(
                     plotOutput("my_contribution_to_common_convos")
                   )
                 ))

ui5 <- fluidPage(titlePanel("Łączna ilość naszych wiadomości wg godziny wysłania"),
                 textOutput("text3"),
                 sidebarLayout(
                   sidebarPanel(
                     
                     selectInput("name5", "Wybierz osobę", c(name_k, name_j, "łącznie")),
                     
                     checkboxGroupInput(
                       "wday1",
                       "W które dni tygodnia?",
                       choices = c("Poniedziałek" = "2",
                                   "Wtorek" = "3",
                                   "Środa" = "4",
                                   "Czwartek" = "5",
                                   "Piątek" = "6",
                                   "Sobota" = "7",
                                   "Niedziela" = "1"),
                       selected = c("1","2","3","4","5","6","7")
                     ),
                     
                     actionButton("working_days", label = "Zaznacz dni robocze"),
                     
                     actionButton("weekend", label = "Zaznacz weekend"),
                     
                     actionButton("all_days", label = "Zaznacz wszystko"),
                     
                     selectInput("which_year", "Wybierz rok",
                                 c("wszystkie", 2022, 2021, 2020, 2019, 2018, 2017, 2016)),
                     
                     radioButtons(
                       "message_type",
                       "Jakie wiadomości?",
                       choices = c("Wysłane", "Otrzymane", "Wszystkie"),
                       selected = c("Wysłane")
                     ),
                     
                     width = 3
                   ),
                   mainPanel <- mainPanel(
                     plotOutput("my_messages_on_clock")
                   )
                 ))

# Main page
app_ui <- navbarPage(
  
  # Application title
  title = "Analiza danych z aplikacji Messenger",
  
  # Panels
  tabPanel("W grupie raźniej?", ui1, icon = icon("user-group")),
  tabPanel("Udziały w rozmowie", ui4, icon = icon("star-half-stroke")),
  tabPanel("Zegar wiadomości", ui5, icon = icon("clock")),
  
  # Appearance
  theme = bslib::bs_theme(bootswatch = "darkly"),
  
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  © 2023 Copyright: Jakub Sawicki & Kuba Seliga
                </p>
                </footer>
                "),
  header = tags$head(tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css"))
)



# Generating charts
server <- function(input, output, session) {
  
  # button listener - working day
  observeEvent(input$working_days,{
    wd <- c("2", "3", "4", "5", "6")
    updateCheckboxGroupInput(session,"wday1", selected = wd)
  })
  
  # button listener - weekend
  observeEvent(input$weekend,{
    w <- c("1", "7")
    updateCheckboxGroupInput(session,"wday1", selected = w)
  })
  
  # button listener - all days
  observeEvent(input$all_days,{
    all <- c("1", "2", "3", "4", "5", "6", "7")
    updateCheckboxGroupInput(session,"wday1", selected = all)
  })
  
  # text for different panels description
  # text for first panel
  output$text1 <- renderText(({
    
    paste0("Częściej rozmawiamy w grupach, czy 1 na 1? Poniższy wykres obrazuje, jak nasze preferencje co do tej kwestii zmieniały się w czasie. ",
           "Domyślnie, wykres przedstawia liczbę wiadomości wysłanych przez wybranego użytkownika w danym miesiącu, z podziałem na konwersacje ",
           "grupowe i prywatne. Wykres skumulowany do 100% przedstawia, jaki % wiadomości zostało przez nas wysłanych w tych dwóch typach konwersacji, ",
           "na wybranej przestrzeni czasu.")
  }))
  
  output$text2 <- renderText(({
    
    paste0("Jaką część wszystkich wiadomości stanowią te wysłane przez nas? Na tym wykresie widzimy, jaki % spośród wszystkich wiadomości ",
           "w wybranym okresie został wysłany przez wybraną osobę w danych konwersacjach. Aby uzyskać znaczące dane, konwersacje są posortowane malejąco ",
           "po ilości wysłanych przez wybraną osobę wiadomości. Można wybrać, ile z tych konwersacji chcemy zobaczyć, a animacja pozwala ukazać płynne zmiany.")
  }))
  
  output$text3 <- renderText(({
    
    paste0("O której godzinie piszemy najczęściej? Odpowiedź to jak zwykle w życiu: 'to zależy'. Ten wykres pokazuje dla każdej z 24 godzin, ",
           "ile wiadomości zostało łącznie wysłanych lub odebranych przez wybraną osobę w ciągu trwania tej godziny, w całym wybranym roku. Możemy wybrać, z których dni tygodnia chcemy uzyskać",
           " dane i na przykład zaobserwować, jak długo autorzy śpią w weekendy nie wysyłając wiadomości.")
  }))
  
  
  output$groups_vs_non_groups <- renderPlot({
    all_messages <- select_data(all_messages_k, all_messages_j, input$name1)
    
    all_messages %>% 
      dplyr::filter(sender_name %in% get_names(input$name1),
                    floordate >= input$slider1date[1],
                    floordate <= input$slider1date[2]) %>%
      group_by(month_date, group) %>%
      summarise(count = n()) -> plot
    
    if (input$plot1type == "fill") {
      plot %>% ggplot(aes(x = month_date, y = count, fill = group)) +
        geom_bar(position = 'fill', stat='identity')+ ylab("Procent wysłanych wiadomości") +
        scale_y_continuous(labels=scales::percent)->p
    } else {
      plot %>% ggplot(aes(x = month_date, y = count, fill = group)) +
        geom_bar(stat='identity')+ ylab("Ilość wysłanych wiadomości") ->p
    }
    
    p + xlab("Miesiące lat") + 
      theme_minimal()+
      theme(text=element_text(size=18),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())+
      guides(fill=guide_legend(title="Typ konwersacji"))+
      scale_fill_manual(values=cols, labels=c('prywatna', 'grupowa'))
    
  })
  
  
  output$my_contribution_to_common_convos <- renderPlot({
    all_messages <- select_data(all_messages_k, all_messages_j, input$name4)
    
    all_messages <- all_messages %>%
      dplyr::filter(floordate >= input$slider2date[1],
                    floordate <= input$slider2date[2])
    
    # zrobiłem bez common convos, żeby top konwersacje odświeżały się razem z datami
    # teraz get_common_convos() jest nie używane nigdzie w tych wykresach co mają zostać
    
    #common_convos <- get_common_convos(all_messages, input$name4)
    
    my_top_convos <- all_messages %>% 
      mutate(by_me = case_when(
        sender_name %in% get_names(input$name4) ~TRUE,
        TRUE~FALSE)) %>% 
      filter(by_me == T) %>% 
      group_by(convo_name) %>% 
      summarise(count = n()) %>% 
      arrange(desc(count)) %>% 
      head(as.integer(input$showMax))
    
    
    all_messages %>% 
      filter(convo_name %in% my_top_convos$convo_name) %>%
      mutate(by_me = case_when(
        sender_name %in% get_names(input$name4) ~TRUE,
        TRUE~FALSE)) %>%
      group_by(convo_name, by_me) %>% 
      summarise(count = n()) %>% 
      ggplot(aes(y=reorder(convo_name, count), x=count, fill = by_me))+
      geom_bar(position = 'fill', stat='identity')+
      scale_x_continuous(labels=scales::percent)+
      xlab("Procent wiadomości w konwersacji") +
      ylab("Nazwa konwersacji")+
      theme_minimal()+
      theme(text=element_text(size=18),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())+
      guides(fill=guide_legend(title="Autor wiadomości"))+
      scale_fill_manual(values=cols, labels=c('inni', as.character(input$name4)))
  })
  
  output$my_messages_on_clock <- renderPlot({
    
    all_messages <- select_data(all_messages_k, all_messages_j, input$name5)
    
    if (input$which_year=="wszystkie"){
      yr<-c(2016:2022)
    } else {
      yr<-input$which_year
    }
    
    if (input$message_type=="Wszystkie") {
      all_messages2 <- all_messages
    }
    
    if (input$message_type=="Wysłane") {
      all_messages2 <- all_messages %>% 
        filter(sender_name == input$name5)
    }
    
    if (input$message_type=="Otrzymane") {
      all_messages2 <- all_messages %>% 
        filter(sender_name != input$name5)
      }
    
    messages_by_time <- as.data.frame(
      all_messages2 %>% 
        filter(year %in% yr) %>% 
        select(time_normal, hour) %>% 
        mutate(wday = wday(time_normal)) %>% 
        filter(wday %in% as.numeric(input$wday1)))
    
    messages_by_time$hour <- circular((messages_by_time$hour),
                                      units="hours", template="clock24")
    #### stary
    #rose.diag(messages_by_time$hour, bin = 24, col = "navy", main = "Liczba wiadomości wg godziny", 
    #prop = 2.5, shrink = 0.9)
    
    
    #### nowy z osią
    ggplot() + 
      geom_histogram(data = messages_by_time, aes(x = hour), 
                     position = "stack", colour = "#375A7F", bins = 25,
                     boundary = 0, fill = "#CE5A7F") +
      
      theme_minimal()+
      theme(text=element_text(size=18))+
      #xlim(0, 24) +
      scale_x_continuous(breaks = seq(0, 23, 1),
                         minor_breaks = NULL
      )+
      coord_polar()+
      #xlim(0, 23) +
      xlab(" ") +
      ylab("Ilość wiadomości")
    
  })
}

# Run the application 
shinyApp(ui = app_ui, server = server)
