###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 9            ###
###########################################

### Zadania 

## 1 Stwórz kolejny interfejs graficzny (ui2), który będzie zawierał tytuł 
# "Rozkład ceny samochodów w zależności od roku produkcji"


## 2 Stwórz aplikację z zakładkami używając server, ui1 i ui2 - shiny::navbarPage.

## 3 Spróbuj zmienić wygląd aplikacji poprzez zastowanie stylu z biblioteki bslib.
# bslib::bs_theme, bslib::bootswatch_themes, bslib::page_navbar(theme)
# https://github.com/rstudio/bslib

## 4 Zmianna wyglądu: footer, css, shiny::HTML, shiny::markdown, icon

# przykład stworzenia footer-a https://mdbootstrap.com/docs/standard/navigation/footer/
# footer = shiny::HTML("
#                 <footer class='text-center text-sm-start' style='width:100%;'>
#                 <hr>
#                 <p class='text-center' style='font-size:12px;'>
#                   © 2021 Copyright:
#                   <a class='text-dark' href='https://www.mi2.ai/'>MI2</a>
#                 </p>
#                 </footer>
#                 ")

# przykład dodania CSS do HEAD
# header = tags$head(tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css"))

# możemy wykorzystać markdown w tworzeniu stron HTML
# shiny::markdown(
#   "Potrzebujemy: \n 1. suwak, na którym można wybrać zakres lat z którego rozpatrujemy samochody \n 2. wykres boxplot cen samochodów w PLN dla wybranych lat z suwaka"
# )

# icon
# icon = icon("database")


# 5 Uzupełnij drugi panel o:
# a) suwak, na którym można wybrać zakres lat z którego rozpatrujemy samochody
# b) wykres boxplot cen samochodów w PLN dla wybranych lat z suwaka

# 6 shinyjqui
# https://github.com/Yang-Tang/shinyjqui

# 7 shinycssloaders::withSpinner
# https://daattali.com/shiny/shinycssloaders-demo
# Wybierz i zastosuj spinnery do wykresów w aplikacji.

# 8 shiny::bindCache  
# https://shiny.rstudio.com/articles/caching.html
# Zwiększ rozmiar danych, zastosuj caching i zobacz różnicę.


library(shiny)
library(dplyr)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(shinycssloaders)


library(PogromcyDanych)
set.seed(123)
df <- na.omit(auta2012) %>% dplyr::sample_frac(0.01)

server <- function(input, output, session) {
  ## wykres zależności między mocą a ceną samochodu 
  ## w zależności od rodzaju paliwa, liczby drzwi, skrzyni biegów
  output$pointPlot <- renderPlot({
    ggplot(df %>% rename(color = input$color),
           aes(x = Cena.w.PLN, y = KM, color = color)) +
      geom_point() +
      scale_y_log10() +
      scale_x_log10() +
      theme_bw() +
      labs(
        x = "Cena [PLN]",
        y = "Moc [KM]",
        color = input$color,
        title = "Samochody: moc vs cena"
      )
  })
  ### Zadanie 8 ###
  #%>% bindCache(input$color)
  ### Koniec - Zadanie 8 ###
  
  ### Zadanie 5 ###
  output$boxPlot <- renderPlot({
    ggplot(df %>% filter(Rok.produkcji >= input$rok[1], Rok.produkcji <= input$rok[2]),
           aes(x = factor(Rok.produkcji),
               y = Cena.w.PLN)) +
      geom_boxplot() +
      theme_bw() +
      labs(
        x = "Rok produkcji",
        y = "Cena [PLN]",
        title = "Rozkład ceny samochodów w zależności od roku produkcji"
      )
    ### Koniec - Zadanie 5 ### 
    
  })
}


ui1 <- fluidPage(titlePanel("Samochody: moc vs cena"),
                 
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(
                       inputId = "color",
                       label = "Parametr:",
                       choices = c(
                         "Rodzaj paliwa" = "Rodzaj.paliwa",
                         "Liczba drzwi" = "Liczba.drzwi",
                         "Skrzynia biegow" = "Skrzynia.biegow"
                       )
                     ),
                     width = 3
                   ),
                   mainPanel(#plotOutput("pointPlot"),
                     ### Zadanie 7 ###
                     shinycssloaders::withSpinner(plotOutput("pointPlot"),
                                                  type = getOption("spinner.type", default = 1),
                                                  color = getOption("spinner.color", default = "#0275D8"),
                                                  size = getOption("spinner.size", default = 1)
                     ),
                     ### Koniec - Zadanie 7 ### 
                     width = 9)
                 ))


### Zadanie 1 ###
ui2 <- fluidPage(titlePanel("Rozkład ceny samochodów w zależności od roku produkcji"),
                 ### Koniec - Zadanie 1 ###                  
                 sidebarLayout(
                   sidebarPanel(
                     ### Zadanie 5 ###
                     sliderInput(
                       inputId = "rok",
                       label = "Lata:",
                       min = min(auta2012$Rok.produkcji),
                       max = max(auta2012$Rok.produkcji),
                       value = c(1980, 2007)
                     ),
                     ### Koniec - Zadanie 5 ### 
                     width = 3
                   ),
                   mainPanel(
                     ### Zadanie 4 ###
                     shiny::markdown(
                       "Potrzebujemy: \n 1. suwak, na którym można wybrać zakres lat z którego rozpatrujemy samochody \n 2. wykres boxplot cen samochodów w PLN dla wybranych lat z suwaka"
                     ),
                     ### Koniec - Zadanie 4 ### 
                     ### Zadanie 5 ###
                     plotOutput("boxPlot"),
                     ### Koniec - Zadanie 5 ### 
                     width = 9)
                 ))



runApp(shinyApp(ui, server))


### Zadanie 2 ###
app_ui <- navbarPage(
  title = "Analiza danych: auta2012",
  tabPanel("Wstęp", ui1),
  tabPanel("Więcej", ui2,
           ### Koniec - Zadanie 2 ### 
           ### Zadanie 4 ###
           icon = icon("database")
           ### Koniec - Zadanie 4 ###
  ),
  ### Zadanie 3 ###
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  ### Koniec - Zadanie 3 ###
  ### Zadanie 4 ###
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  © 2021 Copyright:
                  <a class='text-dark' href='https://www.mi2.ai/'>MI2</a>
                </p>
                </footer>
                "),
  header = tags$head(tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css"))
  ### Koniec - Zadanie 4 ###
)

shinyApp(app_ui, server)


