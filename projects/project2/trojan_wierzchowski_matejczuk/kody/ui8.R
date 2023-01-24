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
ui0 <- fluidPage(
  titlePanel("O projekcie"),
  p("Witaj!") ,
  p(style="text-align: justify;", "Niniejszy dashboard przygotowany został przez trójkę studentów w ramach przedmiotu Techniki Wizualizacji Danych na kierunku Inżynieria I Analiza Danych.
  Zawarte w nim wizualizacje dadzą Ci wgląd w aktywność jego autorów (dalej danonków) na telefonie i komputerze osobistym, jaka miała miejsce w okresie od 12 grudnia 2022 do 
  8 stycznia 2023. Dane potrzebne do przeprowadzenia takiej analizy zebrane zostały przy użyciu open-source'owego trackera Activity Watch oraz zaczerpnięte ze Spotify."),
p(a(href="https://github.com/wierzchw/TWD_ProjektJA", "Repozytorium")),
p("Autorzy:"),
p("Sebastian Trojan - ", a(href="https://github.com/SebastianTrojan", "SebastianTrojan")),
p("Michał Matejczuk - ", a(href="https://github.com/matejczukm", "matejczukm")),
p("Wiktor Wierzchowski - ", a(href="https://github.com/wierzchw", "wierzchw"))
)

ui1 <- fluidPage(
  titlePanel("Uśredniony dzień"),
  p(style="text-align: justify;","Poniższy panel zawiera informacje na temat średniej liczby minut spędzonych przy urządzeniach w
  każdej godzinie doby, wraz z umieszczonym pod spodem rozbiciem na trzy kategorie: 
  praca, rozrywka oraz inne. Jako pracę oflagowane zostały wszelkie aktywności związane ze
  studiami, organizacją dnia, itd. Natomiast za rozrywką kryją się gry, filmy, seriale,
  media społecznościowe, itp. Na ogół późniejsze godziny wiązały się z większą liczbą minut spędzoną na rozrywce i 
  mniejszą na pracy, choć Danonek 1 wydaje się wyłamywać z tego trendu. Jest on też osobą u której najwięcej aktywności załapało się 
  do kategorii inne, wciąż jest to jednak zdecydowana mniejszość. Na tytuł nocnego marka zdecydowanie w pierwszej kolejności zasługuje Danonek 3."),
  # Application title
  tags$hr(style="border-color: black;"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    fluidRow(
      column(3, dateRangeInput('dateRange',
                               label = 'Zakres dni',
                               start = "2022-12-12", end = "2023-01-20",
                               min = "2022-12-12", max = "2023-01-20"
      )
      ),
      column(3, checkboxGroupInput("urzadzenie", "Urządzenie:",
                                   c("telefon" = "telefon",
                                     "komputer" = "komputer"),
                                   selected=c("telefon", "komputer"),inline = TRUE)
      ),
      column(5, checkboxGroupInput("dni_tygodnia", "Dni tygodnia:",
                                   c("poniedziałek" = "poniedziałek",
                                     "wtorek" = "wtorek",
                                     "środa" = "środa",
                                     "czwartek" = "czwartek",
                                     "piątek" = "piątek",
                                     "sobota" = "sobota",
                                     "niedziela" = "niedziela"),selected=c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela"),inline = TRUE)
      ),
      width = 12
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(column(12,h4("Całkowity czas na urządzeniach"), p("  "),
               column(12, shinycssloaders::withSpinner( plotlyOutput("plot1", width = "150%"),
                                                                                        type = getOption("spinner.type", default = 1),
                                                                                        color = getOption("spinner.color", default = "#2c3e50"),
                                                                                        size = getOption("spinner.size", default = 1)),
                      column(12,h4("Czas na urządzeniach z podziałem na kategorie"),
                      column(12,shinycssloaders::withSpinner(plotlyOutput("plot2", width = "150%"),
                                                                                               type = getOption("spinner.type", default = 1),
                                                                                               color = getOption("spinner.color", default = "#2c3e50"),
                                                                                               size = getOption("spinner.size", default = 1))))))
               
      )
    )
    
  )
)

ui2 <- fluidPage(
  titlePanel("Top aplikacje"),
  setSliderColor(c("#2c3e50","#2c3e50"), c(1,2)),
  p(style="text-align: justify;","W tej sekcji prezentowane jest zestawienie aplikacji, w których spędziliśmy najwięcej czasu oraz jaką część
  tego czasu tracker zarejestrował jako status afk (okres braku aktywności z myszy i klawiatury przez co najmniej 3 minuty). 
  Żadna aktywność z telefonu nie została oznaczona jako afk, ponieważ tracker mierzył aktywność wyłącznie gdy ekran był odblokowany oraz 
  w żaden sposób nie mierzył czy użytkownik jest afk czy nie. Dla danych z komputera takim statusem pochwalić się mogą jedynie gry i środowiska do programowania. 
  U wszystkich najczęściej używaną aplikacją desktopową jest przeglądarka, jednak patrząc całościowo u Danonka 2 wyprzedza ją 
  YouTube."),
  # Application title
  # Application title
  tags$hr(style="border-color: black;"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    fluidRow(
      column(3, dateRangeInput('dateRange2',
                               label = 'Zakres dni',
                               start = "2022-12-12", end = "2023-01-20",
                               min = "2022-12-12", max = "2023-01-20")
      ),
      column(3, checkboxGroupInput("urzadzenie2", "Urządzenie:",
                                   c("telefon" = "tel",
                                     "komputer" = "komp"),
                                   selected=c("tel", "komp"),inline = TRUE)
      ),
      column(5, checkboxGroupInput("dni_tygodnia2", "Dni tygodnia:",
                                   c("poniedziałek" = 1,
                                     "wtorek" = 2,
                                     "środa" = 3,
                                     "czwartek" = 4,
                                     "piątek" = 5,
                                     "sobota" = 6,
                                     "niedziela" = 7),selected=c(1,2,3,4,5,6,7),inline = TRUE)
      ),
      column(7, sliderInput("godziny",
                            "Zakres godzin:",
                            min = 0,
                            max = 23,
                            value = c(0,23))
      ),
      width = 12
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(column(12,h4("Top 8 najczęciej używanych aplikacji"), p("  "),
        column(12, shinycssloaders::withSpinner(plotlyOutput("plot3", width = "150%"),
                                                                                         type = getOption("spinner.type", default = 1),
                                                                                         color = getOption("spinner.color", default = "#2c3e50"),
                                                                                         size = getOption("spinner.size", default = 1))),
               column(12,h4("Status użytkownika podczas korzystania z aplikacji"),
                      column(12, shinycssloaders::withSpinner( plotlyOutput("plot4", width = "150%"),
                                                                                               type = getOption("spinner.type", default = 1),
                                                                                               color = getOption("spinner.color", default = "#2c3e50"),
                                                                                               size = getOption("spinner.size", default = 1)))))
               
      )
    )
  )
)

ui3 <- fluidPage(
  
  # Application title
  titlePanel("Przeglądarka dni"),

  p(style="text-align: justify;","Pod tą zakładką kryje się możliwość dokładnego przeglądania aktywności z wybranego, pojedynczego dnia. Jest to świetny sposób na przejrzenie nietypowych
  dni w roku po których spodziewamy się ciekawych wyników.
  W analizowanym przez nas okresie miały miejsce Wigilia, Sylwester jak również rekreacyjne wyjazdy. Zdołacie znaleźć je wszystkie?"),
  # Application title
  tags$hr(style="border-color: black;"),
  
  
  sidebarLayout(
    fluidRow(
      column(3, dateInput('day', 
                          label = 'Dzień',
                          value = "2022-12-12",
                          min = "2022-12-12", max = "2023-01-20")
      ),
      column(3, checkboxGroupInput("urzadzenie3", "Urządzenie:",
                                   c("telefon" = "tel",
                                     "komputer" = "komp"),
                                   selected=c("tel", "komp"),inline = TRUE)
      ),
      width = 12
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(column(12,h4("Użytkowanie aplikacji w ciągu dnia"),
        column(12,shinycssloaders::withSpinner(plotlyOutput("plot7",width = "150%"),
                                                                                        type = getOption("spinner.type", default = 1),
                                                                                        color = getOption("spinner.color", default = "#2c3e50"),
                                                                                        size = getOption("spinner.size", default = 1))),
                      column(12, shinycssloaders::withSpinner( plotlyOutput("plot8" , width = "150%"),
                                                                                                type = getOption("spinner.type", default = 1),
                                                                                                color = getOption("spinner.color", default = "#2c3e50"),
                                                                                                size = getOption("spinner.size", default = 1)))),
                      column(12, shinycssloaders::withSpinner( plotlyOutput("plot9", width = "150%"),
                                                                                                type = getOption("spinner.type", default = 1),
                                                                                                color = getOption("spinner.color", default = "#2c3e50"),
                                                                                                size = getOption("spinner.size", default = 1)))
               
      )
    )
  )
)

ui4 <- fluidPage(
  titlePanel("Słuchanie muzyki"),
  p(style="text-align: justify;", "Najlepsze okoliczności do słuchania muzyki są bardzo subiektywną kwestią. Poniższe wykresy przedstawiają jak procentowo 
  rozkłada się czas spędzony na słuchaniu muzyki na platformie Spotify. I tak od razu widzimy że dla Danonka 2 słuchanie muzyki jest aktywnością samą w sobie i 
  rzadko kiedy towarzyszy jej coś innego. Podczas gdy Danonki 1 oraz 3 od czasu do czasu umilają sobie pracę i odpoczynek odrobiną muzyki."),
  setSliderColor(c("#2c3e50"), c(1)),
  
  # Application title
  tags$hr(style="border-color: black;"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    fluidRow(
      column(3, dateRangeInput('dateRange3',
                               label = 'Zakres dni',
                               start = "2022-12-12", end = "2023-01-20",
                               min = "2022-12-12", max = "2023-01-20")
      ),
      column(7, sliderInput("godziny2",
                            "Zakres godzin:",
                            min = 0,
                            max = 23,
                            value = c(0,23))
      ),
      width = 12
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(column(12,h4("Aktywności podczas słuchania muzyki"), p("  "),
                      column(12, shinycssloaders::withSpinner(plotlyOutput("plot10", width = "150%"),
                                                                                        type = getOption("spinner.type", default = 1),
                                                                                        color = getOption("spinner.color", default = "#2c3e50"),
                                                                                        size = getOption("spinner.size", default = 1))))
      )
    )
  )
  
  
  
)
