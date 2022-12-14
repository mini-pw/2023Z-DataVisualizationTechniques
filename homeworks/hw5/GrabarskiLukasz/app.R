library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(bslib)
library(scales)


 ela <- read.csv2("eula.csv", header = TRUE , dec = ",", sep = ";")
 
 ela %>% 
   select(P_FORMA,P_N,P_N_CZY_ETAT,P_N_DOSW_REKR,P_N_DOSW_STUDIA,P_N_NDOSW,
          P_N_SEMESTR,P_ZAR_ETAT_MAX,P_ME_ZAR,P_ZAR_ETAT_MIN,P_DZIEDZINA,
          P_NAZWA_JEDN,P_KIERUNEK_NAZWA,P_NAZWA_UCZELNI,P_CZY_BEZR,
          P_CZY_PRACA,P_PROC_DOSW,P_PROC_STUDIA,P_PROC_UKON,P_CZY_PRACA_P1,
          P_PROFIL,P_ROKDYP,P_PROC_MIES_BEZR,P_POZIOM,P_CZAS_ETAT,P_CZAS_PRACA,
          P_E_ZAR_ETAT,P_E_ZAR,P_E_ZAR_P1,P_WWB,P_WWB_DOSW_REKR,P_WWB_NDOSW,
          P_WWB_DOSW,P_WWB_DOSW_STUDIA,P_WWB_MIES_1,P_WWB_MIES_12,P_WWB_MIES_6,
          P_WWZ_DOSW_REKR,P_WWZ_P1_DOSW_REKR,P_WWZ_DOSW_STUDIA,P_WWZ,
          P_WWZ_MIES_1,P_WWZ_MIES_12,P_WWZ_MIES_6,
          P_WWZ_PODCZAS_STUDIOW_DOSW_REKR,P_WWZ_PODCZAS_STUDIOW_DOSW,
          P_WWZ_PODCZAS_STUDIOW_DOSW_STUDIA
) -> ela

 ela %>%
   filter(P_POZIOM == 1, P_FORMA == "S") %>%
   mutate(P_DZIEDZINA = if_else(P_DZIEDZINA == "", "Dziedzina inna", P_DZIEDZINA))-> ela
 
 
 # Define UI for application that draws a histogram
 ui <- fluidPage(
   titlePanel("Ekonomiczne Losy Absolwentów"),
   theme = bs_theme(version = 4, bootswatch = "superhero"),
   
   sidebarLayout(
     sidebarPanel(fluidRow(column(
       10,
       
       sliderInput(
         "rok",
         "Lata otrzymania dyplomu",
         value = c(min(ela$P_ROKDYP), max(ela$P_ROKDYP)),
         min = min(ela$P_ROKDYP),
         max = max(ela$P_ROKDYP),
         step = 1
       )
     )),
     fluidRow(column(
       10,
       checkboxGroupInput(
         "dziedzina",
         "Dziedziny studiów",
         choices = unique(ela$P_DZIEDZINA),
         selected = "Dziedzina nauk inzynieryjno-technicznych"
       ),
     )),
     
     fluidRow(column(
       10, uiOutput("kierunek_sel")
     ))),
     
     mainPanel(tabsetPanel(
       tabPanel("Density", plotlyOutput("density_salary")),
       tabPanel("WWZ", plotlyOutput("wwz"))
     ))
     
   )
 )
 
 # Define server logic required to draw a histogram
 server <- function(input, output, session) {
   observeEvent(input$dziedzina,{
     x <- filter(ela, P_DZIEDZINA %in% input[['dziedzina']])
     
     output[['kierunek_sel']] <-
       renderUI({
         selectizeInput(
           "kierunek",
           "Kierunki studiów",
           choices = unique(x[['P_KIERUNEK_NAZWA']]),
           multiple = TRUE,
           selected = head(x$P_KIERUNEK_NAZWA, 1),
           options = list(maxItems = 5)
         )
       })
     
     observeEvent(input$kierunek,{
       my_x <- unlist(
         filter(ela, P_KIERUNEK_NAZWA %in% input$kierunek) %>%
           summarize(mean(P_E_ZAR, na.rm = TRUE)))
       
       output$density_salary <- renderPlotly(
         ggplotly(
           ggplot(
             data = filter(ela,
                           P_ROKDYP %in% input$rok,
                           P_DZIEDZINA  %in% input$dziedzina),
             aes(x = P_E_ZAR, fill = P_DZIEDZINA)
           ) +
             geom_density(alpha = 0.2) +
             labs(
               title = "Rozkład średnich zarobków wg dziedzin studiów",
               x = "Zarobki",
               y = "Częstość",
               fill = "Dziedzina studiów"
             ) +
             
             geom_vline(aes(xintercept = unlist(
               filter(ela, P_KIERUNEK_NAZWA %in% input$kierunek) %>%
                 summarize(mean(P_E_ZAR, na.rm = TRUE))
             )),
             linetype = 'dashed',
             color = "#244882") +
             theme_minimal() +
             scale_y_continuous(labels = label_comma()) +
             geom_text(
               x = my_x,
               y = 0,
               label = "Śr. zarobki z wybranych kierunków",
               color = "#244882",
               angle = 90)
           
           
         ) %>%
           layout(legend = list(orientation = "h",
                                y = -0.5)) 
         
       )
       
     })
     
     output$wwz <- renderPlotly(ggplotly(
       ggplot(
         data = filter(ela, P_KIERUNEK_NAZWA %in% input$kierunek) %>%
           group_by(P_KIERUNEK_NAZWA) %>%
           summarise(E_P_WWZ = mean(P_WWZ, na.rm = TRUE)),
         aes(x = P_KIERUNEK_NAZWA,
             y = E_P_WWZ)
       ) +
         geom_col(fill = "#244882") +
         labs(title = "Względny Wskaźnik Zarobków dla wybranych kierunków",
              x = "Kierunki studiów",
              y = "WWZ") +
         theme_minimal() +
         theme(axis.text.x = element_text(angle = 15))
     ))
     
       
     })
     

   
 }
 
 # Run the application
 
 shinyApp(ui = ui, server = server)
 
 