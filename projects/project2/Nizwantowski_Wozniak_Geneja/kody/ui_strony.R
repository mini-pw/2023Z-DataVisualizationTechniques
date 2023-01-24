
              
              


dane <- read.csv("data.csv", encoding = "UTF-8", header = T, sep = ",")
library(dplyr)

library(shinyWidgets)
library(shinycssloaders)






ui2 <- fluidPage(
  setBackgroundColor("#202124"),
  tags$div(
  tags$span(titlePanel("Wstęp"),style = "color:white", align = "center"),
  tags$br(),
  span(tags$p("Celem projektu było stworzenie dashboardu o nas, dlatego po ustaleniach co nas łączy
              ustaliliśmy, że każdy z nas potrafi grać w szachy. Z tego powodu zdecydowaliśmy się na projekt szachowy. Aby każdy startował z tego samego poziomu założyliśmy 
              nowe konta na platformie Lichess i zaczęliśmy grę. Zebraliśmy danę z 
              około półtora miesiąca gry. Naszym celem było zwizualizowanie naszych 
              różnych statystyk z gier. Poniżej podczas przedstawiania siebie identyfikujemy się jako figury szachowe w odpowiednich kolorach.
         Te barwy oraz nazwy figur odpowiadają nam na wszystkich wizualizacjach. 
          Warto zaznaczyć, że większość gier graliśmy w trybie blitz, tempem 5+0, 
          więc najlepsze wyniki są po wybraniu,
          dokładnie tego tryby. Czasami też graliśmy inne tryby, ale 
          wizualizacje zaprezentowane na ich podstawie, nie są aż tak dokładne ze względu na małą 
          liczbę gier. Oprócz zwykłych partii rozwiązywaliśmy także zadania, aby poprawić swoje umiejętności.",
              style = "color:white"), style = "font-size:20px", align = "justify")),
  tags$br(),
  tags$br(),
  tags$span(titlePanel("O nas"),style = "color:white", align = "center"),
  tags$br(),
  tags$br(),
  fluidRow(
  column(4, align = "center",
         tags$div(tags$img(src = "cos.svg", height = 300, width = 300, deleteFile = F),
                  tags$br(),
                  span(tags$p("Początkujący szachista. W trakcie przygotowywania projektu rozegrał więcej partii niż w ciągu 
                              poprzednich 22 lat swojego życia. Jego ulubioną figurą 
                              jest skoczek i choć w szachy nie grywał, to w dzieciństwie
                              idąc na podłodze wyłożonej kwadratami chodził po niej w taki sposób, w jaki porusza się ta figura.",
                              style = "color:white"), style = "font-size:20px", align = "justify"))),
  column(4, align = "center",
         tags$div(tags$img(src = "cos2.svg", height = 300, width = 300, deleteFile = F),
                  span(tags$p("Kiedyś w podstawówce chodziłem przez dwa miesiące na zajęcia dodatkowe,
                              na których graliśmy z Panem od historii w szachy. Zrobiliśmy sobie nawet
                              turniej i zająłem 2 albo 3 miejsce, ale tylko dlatego, że przekupiłem
                              przeciwnika podróbką mlecznej kanapki z biedronki, profit. Więcej 
                              doświadczenia nie mam.",
                              style = "color:white"), style = "font-size:20px", align = "justify"))),
  column(4, align = "center",
         tags$div(tags$img(src = "cos3.svg", height = 300, width = 300, deleteFile = F),
                  span(tags$p("Szachista z predyspozycjami na bycie niezłym graczem, 
                         ale niestosujący zasady OGR. W młodości grał lepiej i nawet zdobywał mało znaczące medale. Posiada czwartą kategorię,
                         ale szczyt jego umiejętności i formy przypadał na wiek 10-12 lat.",
                         style = "color:white"), style = "font-size:20px", align = "justify")))),
  tags$br(),
  tags$br(),
    fluidRow(column(2,
           tags$style("input[type=checkbox] {
                    transform: scale(1.9);
           }"),
          
           
           tags$style("#uzytkownik {
                    font-size:17px;
                    color:white;
           }"),
           tags$style("#kolor {
                    font-size:17px;
                    color:white;
           }"),
           
        tags$span(checkboxGroupInput('uzytkownik', 'Gracz', choices = c(
          "Niebieski" = "LauPaSat",
          "Czerwony" = "nizwant",
          "Zielony" = "wiktorw123"
        ), selected = c("nizwant", "wiktorw123", "LauPaSat"))), style = "height:15"),
     column(2,
    checkboxGroupInput('kolor', 'Kolor', choices = c(
      "Biały" = "white",
      "Czarne" = "black"
    ), selected = c("white", "black"))
       
     ),
    column(3, 
           span(span(selectInput("typ_gry", "Typ gry", choices = c(
             "Bullet" = "bullet",
             "Blitz" = "blitz",
             "Rapid" = "rapid",
             "Classical" = "classical"), selected = "blitz"), style = "color:white"), style = "font-size:17px")),
    column(5,setSliderColor(c("#00c2cb"), c(1)),
           tags$style(type = 'text/css', '#date .irs-grid-text {font-size: 17px}'),
           div( id = "date",
           span(span(sliderInput("date",
                         "Data",
                         min = as.Date("2022-12-07","%Y-%m-%d"),
                         max = as.Date("2023-01-21","%Y-%m-%d"),
                         value= c(as.Date("2022-12-07","%Y-%m-%d"),as.Date("2023-01-21","%Y-%m-%d")),
                         timeFormat="%Y-%m-%d")), style = "color:white"), style="font-size:17px"))),
  tags$br(),
  tags$br(),
  tags$div(
  plotOutput("rating", width = "auto", height = "500px")%>% withSpinner(color="#0dc5c1"),
  tags$br(),
  tags$br(),
  span(tags$p("Powyższy wykres przedstawia rating graczy na przestrzeni analizowanego czasu.
              Można zobaczyć, że po początkowych spadkach nasze ratingi się mocno ustabilizowały, co wynika z tego, że żaden z nas nie był na poziomie ratingu ~1500. Zawodnicy
              na tym poziomie byli poza naszym zasięgiem i musieliśmy znaleźć nasz optymalny ranking. Uwagę 
              przykuwa fakt, że wraz z upływem czasu, ratingi gorszych graczy zaczęły lekko rosnąć. 
              Jest to najpewniej spowodowane tym, że robiąc zagadki szachowe lub obserwując grę swoją i przeciwnika zaczęli oni poznawać podstawowe zasady szachów. Pozwoliło
              im to na częstsze wygrane z pojawiającymi się nowymi graczami, którzy nie mieli jeszcze
              takich doświadczeń. W przypadku innych trybów gry niż blitz, wyniki nie są równie ciekawe,
              ze względu na mniejszą liczbę rozegranych partii.",
              style = "color:white"), style = "font-size:20px", align = "justify")),
  tags$br(),
  tags$br(),
  tags$div(
  plotOutput("rozklad", width = "auto", height = "500px")%>% withSpinner(color="#0dc5c1"),
  tags$br(),
  tags$br(),
  span(tags$p("Powyższy wykres przedstawia rozkład liczby ruchów po jakiej  kończyły się partie. Można zobaczyć, że bez względu na ranking, 
              najczęstsza liczba ruchów jest mocno zbliżona. Widać natomiast, że  zawodnik najlepszy w porównaniu do innych graczy gra więcej partii, w których liczba ruchów jest duża wysoka.
              Jest to najpewniej spowodowane tym, że 
              na wyższym poziomie rzadziej popełniane są rażące błędy, przez co jest 
              większa szansa na zagranie dłuższej partii. Warto zauważyć, że w przypadku 
              gry białymi rozkład jest bardzo równomierny, natomiast gra czarnymi powoduje, 
              że na wykresie występują skoki. Jest to najpewniej spowodowane 
              tym, że grając czarnymi jesteśmy bardziej zależni od przeciwnika, który pierwszy wykonuje ruch. Z tego powodu nie zawsze możemy zagrać 
              wariant, który jest dla nas komfortowy.",
              style = "color:white"), style = "font-size:20px", align = "justify")),
  tags$br(),
  tags$br(),
  tags$div(
  plotOutput("procenty", width = "auto", height = "500px")%>% withSpinner(color="#0dc5c1"),
  tags$br(),
  tags$br(),
  span(tags$p("Z powyższego wykresu odczytać możemy na jakie sposoby wygrywaliśmy i przegrywaliśmy. Na przykład ja (czerwony) przeczytałem w internecie, że na poziomie poniżej 1000 punktów nie opłaca się poddawać bo istnieje szansa, że przeciwnik zrobi krytyczny błąd, który pozwoli wrócić nam do gry. I rzeczywiście kilkakrotnie zdarzyło mi się stracić we wczesnych fazach gry królową, jednak nieuwaga przeciwnika skutkowała w pomyłkach, które doprowadziły do mojej wygranej. Ciekawy jest też fakt, że niebieski jedną trzecią swoich gier wygrywa na czas, podczas gdy statystycznie w jego grach wykonuje się najmniej ruchów.",
            style = "color:white"), style = "font-size:20px", align = "justify")),
  tags$br(),
  tags$br(),
tags$div(
  plotOutput("dzienne", width = "auto", height = "500px")%>% withSpinner(color="#0dc5c1"),
  tags$br(),
  tags$br(),
  span(tags$p("Powyższy wykres przedstawia liczbę dziennych gier graczy na 
              przestrzeni analizowanego czasu. Wykres jest mocno poszarpany, 
              ponieważ partie szachów graliśmy w czasie wolnym, a jak wiadomo na studiach jest on rozłożony nierównomiernie. Z dużej liczby skoków wykresu, 
              można wywnioskować, że gdy danego dnia usiądzie się do szachownicy, to często 
              liczba gier jest większa niż jedna.",
            style = "color:white"), style = "font-size:20px", align = "justify")),
  
    )


ui1 <- fluidPage(

  
  
)


  
  




              

            
              
              
              
              
              
              
              
