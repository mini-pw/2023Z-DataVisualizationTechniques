### loading libraries ###
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyverse)
library(shinycssloaders)

### loading data ###
# michal
dane1 <- read_csv("dane1.csv")
# misza
dane2 <- read_csv("dane2.csv")
colnames(dane2)[1] <- "App name"
# hubert
dane3 <-
  read_csv("dane3.csv", col_types = cols(Time = col_time(format = "%H:%M:%S")))

dane <- rbind(dane1, dane2, dane3)
colnames(dane)[1] <- "AppName"
#dane <- rbind(dane2, dane3)


### creating first page ###
UIPage1 <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Do czego służą nam programy i aplikacje?"),
  
  fluidRow(
    wellPanel(textOutput("text1")),
    
    sidebarPanel(
      checkboxGroupInput(
        "student1",
        "Wybierz studentów, dla których wyświetlą się statystyki:",
        c("Student 1", "Student 2", "Student 3"),
        selected = c("Student 1", "Student 2", "Student 3")
      ),
      
      sliderInput(
        "day1",
        "Wybierz dni stycznia:",
        value = c(1, 14),
        min = 1,
        max = 14,
        step = 1
      ),
      
      checkboxGroupInput(
        "dev1",
        "Wybierz urządzenie:",
        c("Komputer", "Smartphone"),
        selected =  c("Komputer", "Smartphone")
      )
    ),
    
    mainPanel(plotOutput("plot1"))
  )
)

### creating second page ###
UIPage2 <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Jaka jest zależność między czasem spędzonym w aplikacji i porą dnia?"),
  
  fluidRow(
    wellPanel(textOutput("text2")),
    
    sidebarPanel(
      checkboxGroupInput(
        "app2",
        "Wybierz aplikacje:",
        c("YouTube", "Chrome", "Messenger", "MATLAB", "RStudio"),
        selected = c("Chrome", "Messenger")
      ),
      
      checkboxGroupInput(
        "student2",
        "Wybierz studentów, dla których wyświetlą się statystyki:",
        c("Student 1", "Student 2", "Student 3"),
        selected =  c("Student 1", "Student 2", "Student 3")
      ),
      
      sliderInput(
        "day2",
        "Wybierz dni stycznia:",
        value = c(1, 14),
        min = 1,
        max = 14,
        step = 1
      ),
      
      checkboxGroupInput(
        "dev2",
        "Wybierz urządzenie",
        c("Komputer", "Smartphone"),
        selected =  c("Komputer", "Smartphone")
      ),
    ),
    
    mainPanel(plotOutput("plot2"))
  )
)

### creating third page ###
UIPage3 <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Jak często zaglądamy do telefonu?"),
  
  fluidRow(
    wellPanel(textOutput("text3")),
    
    sidebarPanel(
      checkboxGroupInput(
        "student3",
        "Wybierz studentów, dla których wyświetlą się statystyki:",
        c("Student 1", "Student 2", "Student 3"),
        selected =  c("Student 1", "Student 2", "Student 3")
      ),
      
      checkboxGroupInput(
        "week3",
        "Wybierz tygodnie stycznia brane pod uwagę:",
        c("Tydzień 1", "Tydzień 2"),
        selected =  c("Tydzień 1", "Tydzień 2")
      )
    ),
    
    mainPanel(plotOutput("plot3"))
  )
)

### creating fourth page ###
UIPage4 <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Najpopularniejsze aplikacje"),
  
  fluidRow(
    wellPanel(textOutput("text4")),
    
    sidebarPanel(
      checkboxGroupInput(
        "student4",
        "Wybierz studentów, dla których wyświetlą się statystyki:",
        c("Student 1", "Student 2", "Student 3"),
        selected =  c("Student 1", "Student 2", "Student 3")
      ),
      
      checkboxGroupInput(
        "dev4",
        "Wybierz urządzenie:",
        c("Komputer", "Smartphone"),
        selected =  c("Komputer", "Smartphone")
      ),
      
      numericInput(
        "app4",
        "Wybierz liczbę aplikacji znajdujących się na wykresie",
        5,
        min = 2,
        max = 10,
        step = 1
      ),
      
      sliderInput(
        "day4",
        "Wybierz dni stycznia:",
        value = c(1, 14),
        min = 1,
        max = 14,
        step = 1
      )
    ),
    
    mainPanel(plotOutput("plot4"))
  )
)

### creating fifth page ###
UIPage5 <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Przeznaczenie aplikacji a czas spędzony w niej w ciągu dnia"),
  
  fluidRow(
    wellPanel(textOutput("text5")),
    
    sidebarPanel(
      checkboxGroupInput(
        "student5",
        "Wybierz studentów, dla których wyświetlą się statystyki:",
        c("Student 1", "Student 2", "Student 3"),
        selected =  c("Student 1", "Student 2", "Student 3")
      ),
      
      checkboxGroupInput(
        "dev5",
        "Wybierz urządzenie:",
        c("Komputer", "Smartphone"),
        selected =  c("Komputer", "Smartphone")
      ),
      
      checkboxGroupInput(
        "week5",
        "Wybierz tygodnie stycznia brane pod uwagę:",
        c("Tydzień 1" = 1, "Tydzień 2" = 2),
        selected =  c(1, 2)
      ),
      
      checkboxGroupInput(
        "weekday5",
        "Wybierz dni tygodnia brane pod uwagę:",
        c(
          "Poniedziałek" = 2,
          "Wtorek" = 3,
          "Środa" = 4,
          "Czwartek" = 5,
          "Piątek" = 6,
          "Sobota" = 7,
          "Niedziela" = 1
        ),
        selected =  c(2, 3, 4, 5, 6)
      ),
      
      checkboxGroupInput(
        "category5",
        "Wybierz kategorie aplikacji brane pod uwagę:",
        c(
          "Internet",
          "Komunikacja",
          "Praca",
          "Rozrywka",
          "Zakupy",
          "Inne"
        ),
        selected =  c("Praca", "Rozrywka")
      )
    ),
    
    mainPanel(plotOutput("plot5"))
  )
)

### creating sixth page ###
UIPage6 <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Jak długo trwają sesje?"),
  
  fluidRow(
    wellPanel(textOutput("text6")),
    
    sidebarPanel(
      checkboxGroupInput(
        "student6",
        "Wybierz studentów, dla których wyświetlą się statystyki:",
        c("Student 1", "Student 2", "Student 3"),
        selected = c("Student 1", "Student 2", "Student 3")
      ),
      
      sliderInput(
        "day6",
        "Wybierz dni stycznia:",
        value = c(1, 14),
        min = 1,
        max = 14,
        step = 1
      )
    ),
    
    mainPanel(plotOutput("plot6"))
  )
)

### server management ###
server <- function(input, output) {
  ## rendering first column first page ##
  output$text1 <- renderText({
    "Zbadajmy jak wiele czasu poświęcieliśmy na aplikacje różnych kategorii.
    Kto poświęca najwięcej czasu na pracę? Kto na gry? Możemy też wywnioskować,
    kiedy i jaki student miał sesję albo wolne (wybierając odpowiedni przedział
    i patrząc na co spędzał najwięcej czasu."
  })
  output$plot1 <- renderPlot({
    dane %>%
      filter(
        AppName != "Screen on (locked)" &
          AppName != "Screen on (unlocked)" &
          AppName != "Screen off (locked)" &
          AppName != "Device shutdown" &
          AppName != "Device boot"
      ) %>%
      
      filter (student %in% input$student1) %>%
      filter(dev %in% input$dev1) %>%
      filter(input$day1[1] <= day & day <= input$day1[2]) %>%
      
      group_by(type) %>%
      summarise(total = sum(Duration)) %>%
      mutate(total = round(total / 3600, 2)) %>%
      ggplot(aes(x = type, y = total)) +
      geom_col(fill = "#123456") +
      theme_classic() +
      labs(title = "Łączna ilość czasu spędzona na kategorię",
           x = "Kategorie",
           y = "Czas [h]")
  })
  
  ## rendering first column first page ##
  
  ## rendering first column second page ##
  output$text2 <- renderText({
    "Zastanawialiśmy się, w jakich porach dnia z jakich aplikacji korzystamy.
    W tym celu zrobiliśmy wykres zależności między średnim czasem spędzonym w
    aplikacji a porą dnia. Połączyliśmy dane z komputera i smartphona. W
    przypadku wesji przegladarkowych aplikacji, takich jak YouTube czy Facebook,
    na podstawie tytułu strony internetowej ustawiliśmy nazwę aplikacji na
    odpawiadającą wersję na smartphonie. Metoda działa, niestety, tylko dla
    przeglądarki komputerowej, ponieważ aplikacja śledząca użycie telefonu nie
    zapisuje odwiedzonych stron internetowych. Z wykresu wynika, że zależnie od
    aplikacji okresy zwiększonej aktywności przypadają na inne pory dnia. Np.
    aplikacje rozrywkowe są najdłużej używane rankiem oraz wieczorem, czyli
    w godzinach wolnych od pracy."
  })
  output$plot2 <- renderPlot({
    app = input$app2
    filter(
      dane,
      AppName %in% app,
      student %in% input$student2,
      dev %in% input$dev2,
      day >= input$day2[1],
      day <= input$day2[2]
    ) -> dane4
    for (i in app) {
      dane4 <- add_row(
        dane4,
        AppName = i,
        hour = 0:23,
        Duration = 0
      )
    }
    dane4 %>%
      filter(Duration < 3 * 3600) %>%
      #sumujemy sesje w ciagu kazdej godziny na kazdym urządzeniu dla każdego
      group_by(AppName, hour, dev, student, day) %>%
      summarise(Duration = sum(Duration)) %>%
      #liczymy średnią z tego
      group_by(AppName, hour) %>%
      summarise(Duration = mean(Duration)) %>%
      ggplot(aes(hour, Duration / 60, color = AppName)) +
      geom_point() +
      geom_line() +
      theme_minimal() +
      labs(title = "Średni czas spędzony w aplikacji w ciągu doby",
           x = "Godzina",
           y = "Minuty w ciągu godziny") +
      scale_x_continuous(breaks = 0:23, expand = c(0, 0)) +
      scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30),
                         expand = c(0, 0)) +
      guides(color = guide_legend(title = "Aplikacja"))
  })
  
  ## rendering first column third page ##
  output$text3 <- renderText({
    "W tym wykresie chcieliśmy prześledzić, jak często odblokowujemy telefon i
    czy jest to związane z godziną oraz dniem tygodnia. W tym celu zrobiliśmy
    heatmapę, w której oś X stanowią dni tygodnia, a oś Y godziny w ciągu dnia.
    Wraz z wzrostem średniej liczby odblokowań w danej porze rośnie nasycenie
    koloru na odpowiadającym polu heatmapy. Wyniki pokazały, że najczęściej
    zaglądamy do telefonu w godzinach zajęć w tygodniu. Rzadziej w ciągu weekendu.
    Analizując wykres trzeba pamiętać, że liczba odblokowań nie musi iść w
    parze z czasem, spędzonym na jego użytkowaniu."
  })
  output$plot3 <- renderPlot({
    dane %>%
      filter(str_detect(AppName, 'unlocked'))
    head(dane)
    
    dane %>%
      filter(dev == "Smartphone", student %in% input$student3) %>%
      mutate(week = if_else(day <= 7, "Tydzień 1", "Tydzień 2")) %>%
      filter(week %in% input$week3) %>%
      filter(str_detect(AppName, 'unlocked')) %>%
      mutate(day = (day - 2) %% 7) %>% #przesunięcie żeby poniedziałek był 0
      group_by(student, day, hour, week) %>%
      count() %>%
      group_by(day, hour) %>%
      summarise(Średnia = mean(n)) %>%
      mutate(
        dzien = case_when(
          day == 0 ~ "Poniedziałek",
          day == 1 ~ "Wtorek",
          day == 2 ~ "Środa",
          day == 3 ~ "Czwartek",
          day == 4 ~ "Piątek",
          day == 5 ~ "Sobota",
          day == 6 ~ "Niedziela",
        )
      ) %>%
      mutate(dzien = forcats::fct_reorder(dzien, day)) %>%
      ggplot(aes(dzien, hour, fill = Średnia)) +
      geom_tile() + theme_classic() +
      scale_fill_gradient(high = "red", low = "white") +
      labs(title = "Średnia liczba odblokowań telefonu w różnych porach tygodnia",
           x = "Dzień tygodnia",
           y = "Godzina") +
      scale_y_continuous(breaks = 0:23, expand = c(0, 0)) +
      scale_x_discrete(expand = c(0, 0))
  })
  
  ## rendering second column third page ##
  output$text6 <- renderText({
    "Ten wykres przedstawia jakiej długości sesje przy telefonie są najbardziej
    popularne wśród nas. Przez sesję rozumie się moment od odblokowania do
    najbliższego zablokowania telefonu."
  })
  output$plot6 <- renderPlot({
    m <- function(pArg) {
      inGroup = F
      counter = 1
      startingCheck = T
      
      if (nrow(pArg) == 0) {
        col <- data.frame(matrix(nrow = 0, ncol = 1))
        names(col) <- c("grId")
        
        pArg <- cbind(pArg, col)
      } else
        for (i in 1:nrow(pArg)) {
          if (startingCheck == T) {
            pArg[i, "grId"] = 0
            
            if (pArg[i, "AppName"] == "Screen on (unlocked)") {
              startingCheck = F
            }
            
            next
          }
          
          # if in group
          if (inGroup == T) {
            # then mark it
            pArg[i, "grId"] = counter
          }
          
          # group started
          if (pArg[i, "AppName"] == "Screen off (locked)") {
            inGroup = T
          }
          
          # group ended
          if (pArg[i, "AppName"] == "Screen on (unlocked)") {
            inGroup = F
            counter = counter + 1
          }
        }
      
      pArg %>%
        filter(!is.na(grId)) %>%
        group_by(grId) %>%
        summarise(totalDuration = sum(as.integer(Duration))) %>%
        mutate(totalDurationInMinutes = round(totalDuration / 60, 1)) %>%
        mutate(
          DurMin = totalDurationInMinutes,
          totalDuration = NULL,
          totalDurationInMinutes = NULL
        ) %>%
        filter(DurMin <= 50) -> df_mod
      
      return(df_mod)
    }
    
    dane %>%
      filter(dev == "Smartphone") %>%
      
      filter(student %in% input$student6) %>%
      filter(input$day6[1] <= day & day <= input$day6[2]) %>%
      
      mutate(dev = NULL) %>%
      group_by(student) %>%
      group_modify( ~ m(.x)) -> df
    
    ggplot(df, aes(x = DurMin, fill = student)) +
      geom_density(alpha = 0.5) +
      theme_classic() +
      labs(title = "Jak dużo czasu spędzamy przy telefonie na sesję?",
           x = "Czas spędzony na sesję [m]",
           y = "Częstotliwość")
  })
  
  output$text4 <- renderText({
    "Ten wykres przedstawia jakie aplikacje są najczęściej wykorzystywane."
  })
  
  output$plot4 <- renderPlot({
    dane %>%
      filter(dev %in% input$dev4, student %in% input$student4) %>%
      filter(input$day4[1] <= day & day <= input$day4[2]) %>%
      filter(
        AppName != "Screen off (locked)" &
          AppName != "Screen on (unlocked)" &
          AppName != "Screen on (locked)" &
          AppName != "Device shutdown"
      ) %>%
      group_by(AppName, day) %>%
      summarise(sumTime = sum(Duration)) %>%
      summarise(avgTime = mean(sumTime) / 3600) %>%
      slice_max(avgTime, n = input$app4) %>%
      ggplot(aes(reorder(AppName, avgTime), avgTime)) +
      geom_col() +
      coord_flip() +
      geom_col(fill = "#123456") +
      theme_classic() +
      labs(title = "Najpopularniejsze aplikacje",
           y = "Średnia dzienna liczba spędzonych godzin",
           x = "Nazwa Aplikacji")
  })
  
  output$text5 <- renderText({
    "Chcieliśmy przeanalizować do jakich celów używamy nasze urządzenia. W tym celu dla każdej kategorii 
policzyliśmy średni czas w ciągu godziny i oznaczyliśmy na wykresie odpowiednim kolorem. 
Uznaliśmy, że postawimy słupki jeden na drugim by dostać całkowity średni czas spędzony przy urządzeniach, jako dodatkowa informacja. 
W tym wykresie postanowiliśmy rozdzielic dni na robocze i weekend, by zobaczyć czy jest jakaś zależność. 
Okazało się, że w ciągu weekendu jednak odpoczywamy i poświęcamy mniej czasu na prace."
  })
  
  output$plot5 <- renderPlot({
    dane %>%
      filter(day %% 7 %in% input$weekday5) %>%
      filter(day %/% 7 %in% input$week5) %>%
      filter(dev %in% input$dev5, student %in% input$student5) %>%
      filter(type %in% input$category5) %>%
      filter(
        AppName != "Screen off (locked)" &
          AppName != "Screen on (unlocked)" &
          AppName != "Screen on (locked)" &
          AppName != "Device shutdown"
      ) %>%
      group_by(type, hour, day) %>%
      summarise(sumTime = sum(Duration)) %>%
      summarise(avgTime = mean(sumTime) / (60 * length(input$student5))) %>%
      ggplot(aes(hour, avgTime, fill = type)) +
      geom_col() +
      theme_classic() +
      labs(title = "",
           y = "Minuty użytkowania urządzenia",
           x = "Godzina") +
      scale_fill_discrete(name = "Kategorie") +
      scale_x_continuous(breaks = 0:23, expand = c(0, 0))
  })
}

### packing pages in one ui layout ###
ui <- navbarPage(
  title = "Nasza Aktywność",
  tabPanel("Do czego służą aplikajce", UIPage1),
  tabPanel("Programy i Aplikacje", UIPage2),
  tabPanel("Odblokowania telefonu", UIPage3),
  tabPanel("Najpopularniejsze aplikacje", UIPage4),
  tabPanel("Na co poświęcamy czas w ciągu dnia?", UIPage5),
  tabPanel("Jak długo trwają nasze sesje przy telefonie", UIPage6),
)

### starting app ###
shinyApp(ui = ui, server = server)
