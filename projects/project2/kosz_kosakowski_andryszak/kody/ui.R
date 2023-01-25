library(shiny)
library(shinydashboard)
library(dplyr)
library(shinyWidgets)
library(shinycssloaders)
library(plotly)






# tu cos dziala -----------------------------------------------------




ui11 <-    fluidPage(
  tags$head(tags$style(HTML('* {font-family: "Arial"};
                            .navbar-background { background: #1DB954; }'))),
                     # h1("Najczęściej słuchani artyści z ubiegłego roku"),
                     titlePanel(h1("Najczęściej słuchani artyści z ubiegłego roku")),
  tags$h1(tags$style('h1 {font-family: "Arial";
                     font-size:40px};')),
                     # h5("Dane "),
                     fixedRow(
                       column(2, htmlOutput("top_artists_img1")),
                       column(2, tableOutput("top_artist_table1")),
                       column(2, htmlOutput("top_artists_img2")),
                       column(2, tableOutput("top_artist_table2")),
                       column(2, htmlOutput("top_artists_img3")),
                       column(2, tableOutput("top_artist_table3"))
                     ),
                     
                     fixedRow(
                       column(2, htmlOutput("top_artists_img4")),
                       column(2, tableOutput("top_artist_table4")),
                       column(2, htmlOutput("top_artists_img5")),
                       column(2, tableOutput("top_artist_table5")),
                       column(2, htmlOutput("top_artists_img6")),
                       column(2, tableOutput("top_artist_table6"))
                     ),
                     fixedRow(
                       column(2, htmlOutput("top_artists_img7")),
                       column(2, tableOutput("top_artist_table7")),
                       column(2, htmlOutput("top_artists_img8")),
                       column(2, tableOutput("top_artist_table8")),
                       column(2, htmlOutput("top_artists_img9")),
                       column(2, tableOutput("top_artist_table9"))
                     )
)


ui12 <- fluidPage(
  
  titlePanel(h1("Ulubione utwory")),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(
        inputId = "pora",
        label = "Wybór pory dnia:",
        choices = c("Cała doba" = "*", "Dzień" = "day", "Noc" = "night")
      ),
      
      sliderInput("miesiac", "Wybór miesięcy:",
                  min = 1, max = 12, value = c(1, 12), step = 1, ticks = F
      ),
      shiny::HTML(
        "<p style='width: 100%; margin-left:2%; font-size:22px'> Wykres przedstawia 9 utworów najwięcej razy przesłuchanych <br>w wybranym okresie 
        odpowiednio <br>w ciągu dnia, czyli między godzinami 6 a 22, w nocy, lub przez całą dobę.</p>"
      ),tags$head(tags$style(HTML('* {font-family: "Arial"};')))
      
    ),
    
    # Show a plot
    mainPanel(
      shinycssloaders::withSpinner(
        plotOutput("tracksPlot",
                   height = "600px"),
        type = 1,
        color = "#be03fc",
        size = 1 # getOption("spinner.size", default = 1)
      )
    )
  )
)



# ui2 ---------------------------------------------------------------------


ui21 <- fluidPage(
  titlePanel(h1("Gatunki")),
  withSpinner(plotlyOutput("sunburstChart")),
  shiny::HTML(
    "</br><p style='width: 60%; margin-left:20%; font-size:21px'> Wykres przedstawia udział poszczególnych gatunków muzycznych czterdziestu
      najpopularniejszych artystów słuchanych orzez daną osobę od początku korzystania z platformy Spotify. 
      Przy zliczaniu udziału
      poszczególnych gatunków przypisywana jest waga zależna od tego, jak często słuchany jest
      artysta reprezentujący dany gatunek.</p>"
  ),
  tags$head(tags$style(HTML('* {font-family: "Arial"};')))
)

ui22 <- fluidPage(
  titlePanel(h1("Gatunki")),
  withSpinner(plotlyOutput("topChart")),
  shiny::HTML(
    "</br><p style='width: 60%; margin-left:20%; font-size:21px'> Wykres przedstawia 
      dziesięć najczęściej słuchanych gatunków muzycznych przez daną osobę od początku korzystania z platformy Spotify.
      Wartość poszczególnych gatunków zależy od tego, jak często słuchany jest artysta reprezentujący dany gatunek.</p>"
  ),
  tags$head(tags$style(HTML('* {font-family: "Arial"};')))
)


# ui3 ---------------------------------------------------------------------


ui3 <- fluidPage(
  
  titlePanel("Analiza czasu i ilości przesłuchanych utworów w aplikacji Spotify"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("months",
                         "Wybierz miesiące, dla których danych ma być wygenerowany pierwszy wykres",
                         choices=c(1,2,3,4,5,6,7,8,9,10,11,12),
                         selected=8),
      uiOutput("songs"),
      selectInput("dni",
                  "Wybierz dzień tygodnia, dla którego ma być wygenerowany trzeci wykres",
                  choices=c("poniedziałek", "wtorek", "środa", "czwartek","piątek","sobota","niedziela"))
    ),
    mainPanel(
      shiny::HTML(
        "</br><p style='width: 100%; margin-bottom:20px; font-size:19px'> Ten oto wykres przedstawia jak dużo czasu spędzamy w dany dzień tygodnia o danej godzinie na odtwarzaniu utworów w aplikacji Spotify.
    Za pomocą wyboru numerów miesięcy wybieramy, które z nich będą brane pod uwagę na pierwszym wykresie.</p>"
      ),
      plotOutput("HeatMap"),
      shiny::HTML(
        "</br><p style='width: 100%; margin-bottom:20px; font-size:19px'> Wykres, który widzimy poniżej prezentuje zależność ilości przesłuchanych utworów od danych artystów w zależności od miesiąca.
    Do wyboru mamy 10 najczęściej słuchanych artystów.</p>"
      ),
      plotOutput("Animate"),
      shiny::HTML(
        "</br><p style='width: 100%; margin-bottom:20px; font-size:19px'> Poniższy wykres prezentuje ilość przesłuchanych utworów w zależności od godziny dnia. Możemy wybrać, 
    dla którego dnia tygodnia chcemy zobaczyć histogram.</p>"
      ),
      plotOutput("Hist")
    )
  )
)


# sklejenie calosci -------------------------------------------------------



shinyUI(
  dashboardPage(
    dashboardHeader(title = "Spotify"),
    dashboardSidebar(
      sidebarMenu(id = 'wykonawcy',
                  # first menu item
                  # menuItem("Wykonawcy i artyści", tabName = "wykonawcy"),
                  
                  menuItem('Wykonawcy i utwory',
                           #icon = icon('line-chart'),
                           menuSubItem('Najczęściej słuchani wykonawcy',
                                       tabName = 'wykonawcy'
                                       #icon = icon('line-chart')
                           ),
                           menuSubItem('Ulubione utwory',
                                       tabName = 'utwory'
                                       #icon = icon('line-chart')
                                       
                           )),
                  
                  # second menu item with 2 sub menus
                  menuItem('Gatunki',
                           #icon = icon('line-chart'),
                           menuSubItem('Rozkład gatunków',
                                       tabName = 'chart1'
                                       #icon = icon('line-chart')
                           ),
                           menuSubItem('Top 10 gatunków',
                                       tabName = 'chart2'
                                       #icon = icon('line-chart')
                                       
                           )),
                  menuItem('Analiza czasowa', tabName = "rozklad")),
      selectInput("chart", "Wybierz osobę:",
                  c("Piotrek", "Mati", "Krzysztof"))
    ),
    
    
    dashboardBody(
      tags$head(tags$style(HTML('
                            .skin-blue .main-header .logo { background-color: #168d40; }
                                .skin-blue .main-header .logo:hover { background-color: #168d40; }
                                .skin-blue .main-header .navbar { background-color: #1DB954; }
                                .skin-blue .main-header .navbar .sidebar-toggle:hover { background-color: #168d40; }'))),
      tabItems(
        tabItem("wykonawcy", ui11),
        tabItem("utwory", ui12),
        tabItem("chart1", ui21),
        tabItem("chart2", ui22),
        tabItem("rozklad", ui3)
      )
    )
  )
)
