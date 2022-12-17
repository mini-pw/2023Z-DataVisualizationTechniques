library(shiny)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(stringr)
library(tidyr)


#############################################################################
#                       Wstępne przygotowanie danych                        #
#############################################################################

faculty_map = list(
  "Wydzial Matematyki i Nauk Informacyjnych" = "[MiNI]",
  "Wydzial Elektryczny" = "[WE]",
  "Wydzial Elektroniki i Technik Informacyjnych" = "[EiTI]",
  "Wydzial Mechatroniki" = "[WM]",
  "Wydzial Mechaniczny Energetyki i Lotnictwa" = "[MEiL]",
  "Wydzial Mechaniczny Technologiczny" = "[WMT]",
  "Wydzial Geodezji i Kartografii" = "[GiK]",
  "Wydzial Zarzadzania" = "[WZ]",
  "Wydzial Samochodow i Maszyn Roboczych" = "[SiMR]"
)

# tworzymy customową paletę kolorów do wykresu
paleta = setNames(object = c("firebrick2", "green3", "cadetblue3",
                             "slateblue3", "yellow3", "seagreen",
                             "mistyrose4", "red2", "hotpink"),
                  nm = paste(unlist(faculty_map),
                             names(faculty_map),
                             sep=" - "))

# Wstepnie filtrujemy tylko dane dotyczace studiow stacjonarnych
# pierwszego stopnia na PW

# Jak stworzona została ramka danych df_graduates na podstawie wstępnej ramki
# danych ze strony https://www.ela.nauka.gov.pl/pl/experts/source-data

# df_graduates <- readxl::read_excel("./graduates-major-data.xlsx") %>%
#   filter(P_NAZWA_UCZELNI=="Politechnika Warszawska",
#          P_POZIOM_TEKST_PL=="Studia pierwszego stopnia",
#          P_FORMA=="S",
#          P_ROKDYP>=2017)
df_graduates <- read.csv("./graduates.csv")[2:688]

# reszta przeksztalcania danych dla 1 wykresu
df <- df_graduates %>%
  group_by(Faculty=P_NAZWA_JEDN, Career=P_KIERUNEK_NAZWA) %>%
  summarise(Zarobki.Ndosw = mean(P_E_ZAR_ETAT_NDOSW),
            Zarobki.Dosw = mean(P_E_ZAR_ETAT_DOSW),
            Zarobki.Wszyscy = mean(P_E_ZAR_ETAT)) %>%
  na.omit()

df$Faculty <- str_replace_all(df$Faculty,
                              c("Ĺ‚"="l", "Ăl"="o", "Ä…"="a", "ĹĽ"="z", "Ăł"="o"))
df$Career <- str_replace_all(df$Career,
                             c("Ĺ‚"="l", "Ăl"="o", "Ä…"="a", "ĹĽ"="z", "Ăł"="o"))
df <- df %>% mutate(ID=paste(faculty_map[Faculty], Career, sep=" "),
                    Legenda=paste(faculty_map[Faculty], Faculty, sep=" - "))

# reszta przeksztalcania danych dla 2 wykresu
df2 <- df_graduates %>%
  select(Faculty = P_NAZWA_JEDN,
         c(551:586)) %>%
  na.omit() %>% 
  group_by(Faculty) %>% 
  summarize(across(cols=everything(), .fns = mean))

df2$Faculty <- str_replace_all(df2$Faculty,
                               c("Ĺ‚"="l", "Ăl"="o", "Ä…"="a",
                                 "ĹĽ"="z","Ăł"="o", "Ĺš"="S"))

df2 <- as.data.frame(t(df2))
df2 <- janitor::row_to_names(df2, 1)


#############################################################################
#                          Komponenty aplikacji                             #
#############################################################################

ui1 <- fluidPage(
  titlePanel("Srednie miesieczne wynagrodzenie
               z tytulu umow o prace (po uzyskaniu dyplomu)
               wsrod absolwentow PW."),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId= "experience",
                  "Doswiadczenie absolwentow w pracy (okres
                        przed uzyskaniem dyplomu):",
                  choices=c("Bez doswiadczenia"=0,
                            "Z doswiadczeniem"=1,
                            "Wszyscy"=2),
                  selected=0),
      shiny::markdown(
        "Dane dotycza tylko absolwentow studiow stacjonarnych
       pierwszego stopnia, ktorzy otrzymali dyplom w roku 2017
       badz pozniej."
      )
    ),
    
    mainPanel(
      shinycssloaders::withSpinner(
        shinyjqui::jqui_resizable(plotOutput("barPlot")),
        type = 4,
        color = "aliceblue",
        size=0.5)
    )
  )
)

ui2 <- fluidPage(
  titlePanel("Wzgledny Wskaznik Zarobkow (WWZ) w kolejnych miesiacach
  po uzyskaniu dyplomu wsrod absolwentow PW (wzgledem wydzialow)."),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId= "faculty",
                  "Wydzial PW:",
                  choices=colnames(df2),
                  selected="Wydzial Matematyki i Nauk Informacyjnych"),
      shiny::markdown(
        "Dane dotycza tylko absolwentow studiow stacjonarnych
       pierwszego stopnia, ktorzy otrzymali dyplom w roku 2017
       badz pozniej.
       
       **Uwaga!** Ze wzgledu na nieuwzglednienie rocznikow dla
       ktorych zabraklo danych dla wszystkich 36 miesiecy, niektore wykresy
       generowane sa przy niewielkiej probce danych. Nalezy brac to pod uwage
       podczas odczytywania informacji.
      
       Wzgledny Wskaznik Zarobkow to srednia wartosc ilorazu sredniego
       miesiecznego wynagrodzenia absolwenta do sredniego miesiecznego
       wynagrodzenia w powiecie zamieszkanym przez absolwenta."
      )
    ),
    mainPanel(
      shinycssloaders::withSpinner(
        shinyjqui::jqui_resizable(plotOutput("scatterPlot")),
        type = 4,
        color = "aliceblue",
        size=0.5)
    )
  )
)

ui <- navbarPage(
  title = "Zarobki absolwentow PW",
  tabPanel("Srednie zarobki", ui1, icon = icon("dollar-sign")),
  tabPanel("Wzgledny Wskaznik Zarobkow", ui2, icon = icon("compass")),
  footer = shiny::HTML("
                  <footer class='text-center text-sm-start' style='width:100%;'>
                  <hr>
                  <p class='text-center' style='font-size:12px;'>
                    © 2022 Autor:
                    <a class='text-dark' href='https://github.com/AKapich'>Aleks Kapich</a>
                  </p>
                  </footer>
                  "),
  theme = bslib::bs_theme(bootswatch = "superhero")
)

server <- function(input, output) {
  
  output$barPlot <- renderPlot({
    # Wybranie odpowiednich danych w zaleznoSci od inputu
    df <- df[c(1,2,3+as.numeric(input$experience),6,7)]
    colnames(df)[3] <- "Zarobki"
    df <- df %>% 
      arrange(desc(Zarobki)) %>%
      head(20)
    
    # Stworzenie wykresu
    ggplot(data=df, aes(x= reorder(ID, Zarobki),
                        y=Zarobki,
                        fill=Legenda)) +
      geom_col() +
      scale_y_continuous(expand=c(0,0),
                         limits=c(0,1.1*max(df$Zarobki))
      ) +
      scale_fill_manual(values = paleta)+
      labs(
        x = "Kierunek",
        y = "Zarobki [PLN]",
        title = "Top 20 kierunkow"
      ) +
      coord_flip() +
      theme_minimal()
  })
  
  
  output$scatterPlot <- renderPlot({
    # Wybranie odpowiednich danych w zaleznoSci od inputu
    df<- df2[input$faculty]
    colnames(df)[1] <- "Faculty"
    df["Index"] <- c(1:nrow(df))
    
    # Stworzenie wykresu
    ggplot(data=df, aes(x=Index, y=as.numeric(Faculty)))+
      geom_point()+
      scale_x_discrete(expand=c(0,0),
                       limits=factor(1:36)) +
      scale_y_continuous(limits=c(0.25, 1.75)) +
      labs(x="Miesiac po uzyskaniu dyplomu",
           y=paste("WWZ", input$faculty, sep=" - "),
           title="Wzgledny Wskaznik Zarobkow (WWZ) w kolejnych miesiacach po otrzymaniu dyplomu")+
      theme_minimal() +
      geom_smooth(method="lm")
  })
}

shinyApp(ui = ui, server = server)
