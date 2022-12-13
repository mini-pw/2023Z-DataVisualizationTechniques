library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(shinythemes)


df <- read.csv("prepared_data.csv") # load pre-prepared data

dff <- read.csv("prepared_data_plot2.csv")

server <- function(input, output) {
  
  # generuj wykres średniego wynagrodzenia
  output$earningsPlot <- renderPlot({
    plt <- df
    
    if(input$school != "*") {
      plt <- plt %>% filter(WOJ_NAME == input$school)
    } 
    
    plt <- plt %>% 
      group_by(P_ROKDYP, P_DZIEDZINA) %>% 
      summarise(srednie_wynagrodzenie = mean(P_E_ZAR, na.rm=TRUE)) 
    plt %>% 
      filter(P_DZIEDZINA %in% input$dziedzina)  %>% 
      ggplot(aes(x = P_ROKDYP, y=srednie_wynagrodzenie, color = P_DZIEDZINA)) +
      geom_line() +
      labs(
        title = "Średnie wynagrodzenie absolwentów studiów z danej dziedziny",
        subtitle = ifelse(input$school == "*", "w całej Polsce", paste("w województwie", input$school)), 
        x = "Rok uzyskania dyplomu",
        y = "Średnie wynagrodzenie (w PLN)",
        color = "Dziedzina studiów"
      ) + theme_bw() +
      ylim(min(plt$srednie_wynagrodzenie, na.rm=TRUE), max(plt$srednie_wynagrodzenie, na.rm = TRUE)) +
      theme(panel.background = element_rect(fill = "transparent", colour = "black"),
            plot.background = element_rect(fill = "transparent", colour = NA))
    #ylim(2400, 5000)
  })
  
  # generuj wykres średniego czasu od uzyskania dyplomu do znalezienia pracy
  output$secondPlot <- renderPlot({
    plt <- dff
    
    if(input$region != "*") {
      plt <- plt %>% filter(WOJ_NAME == input$region)
    } 
    
    plt %>% 
      filter(P_ROKDYP %in% input$rok) %>% 
      group_by(P_DZIEDZINA, P_ROKDYP, dosw, P_WOJ, WOJ_NAME) %>% 
      summarise(sredni_czas = mean(sredni_czas, na.rm = T)) %>%
      ggplot(aes(fill = dosw, y = sredni_czas, x = P_DZIEDZINA)) +
      geom_col(position="dodge") +
      labs(
        title = "Średni czas od uzyskania dyplomu do podjęcia pierwszej pracy po uzyskaniu dyplomu \nwśród absolwentów studiów z danej dziedziny",
        subtitle = ifelse(input$region == "*", "w całej Polsce", paste("w województwie", input$region)),
        x = "Dziedzina",
        y = "Średni czas (w miesiącach)"
      ) + 
      theme_bw() +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      # theme(legend.text = element_text("Doświadczenie zawodowe przed uzyskaniem dyplomu")) +
      guides(fill=guide_legend(title=" Doświadczenie zawodowe przed \n uzyskaniem dyplomu")) +
      scale_y_continuous(expand = c(0, 0)) +
      ylim(min(dff$sredni_czas, na.rm=TRUE), max(dff$sredni_czas, na.rm = TRUE)) +
      theme(panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA))
    
  })
}


ui1 <- fluidPage(theme = shinytheme("cosmo"),
  
  # Application title
  titlePanel("Średnie wynagrodzenie absolwentów uczelni wyższych w zależności od regionu"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "school",
        label = "Wybór regionu:",
        choices = c("Cała polska" = "*", unique(pull(df, WOJ_NAME)))
      ) ,
      checkboxGroupInput("dziedzina", label = "Wybierz dziedziny studiów do pokazania na wykresie",
                         choices = unique(pull(df, P_DZIEDZINA)),
                         selected = unique(pull(df, P_DZIEDZINA))),),
    
    # Show a plot of the generated distribution
    mainPanel(
      shinycssloaders::withSpinner(
        plotOutput("earningsPlot",
                   height = "600px"), 
        type = 1, 
        color = "#00ff00", 
        size = 1 # getOption("spinner.size", default = 1)
      )
    )
  )
)




ui2 <- fluidPage(

  # Site title
  titlePanel("Średnie wynagrodzenie absolwentów uczelni wyższych w zależności od regionu"),

  sidebarLayout(
    sidebarPanel(
 
      selectInput(
        inputId = "region",
        label = "Wybór regionu:",
        choices = c("Cała polska" = "*", unique(pull(dff, WOJ_NAME)))
      ),

      checkboxGroupInput("rok", label = "Wybierz rok wydania dyplomu",
                         choices = unique(pull(dff, P_ROKDYP)),
                         selected = unique(pull(dff, P_ROKDYP))),
     
      ),

    # Show a plot
    mainPanel(
      shinycssloaders::withSpinner(
        plotOutput("secondPlot",
                   height = "600px"),
        type = 1,
        color = "#00ff00",
        size = 1 # getOption("spinner.size", default = 1)
      )
    )
  )
)
# ui2 <- fluidPage(
#   
#   # Site title
#   titlePanel("Średnie wynagrodzenie absolwentów uczelni wyższych w zależności od regionu"),
#   
#   fluidRow(
#     column(6, 
#            selectInput(
#                      inputId = "region",
#                      label = "Wybór regionu:",
#                      choices = c("Cała polska" = "*", unique(pull(dff, WOJ_NAME)))
#                    ),
# 
#                    checkboxGroupInput("rok", label = "Wybierz rok wydania dyplomu",
#                                       choices = unique(pull(dff, P_ROKDYP)),
#                                       selected = unique(pull(dff, P_ROKDYP))),)
#     ),
#   fluidRow(
#     shinycssloaders::withSpinner(
#               plotOutput("secondPlot"),
#               type = 1,
#               color = "#00ff00",
#               size = 1 # getOption("spinner.size", default = 1)
#             )
#   )
# )
  
  


app_ui <- navbarPage("Badanie losów studentów",
                     tabPanel("Wynagrodzenia", ui1),
                     tabPanel("Pierwsza praca po studiach", ui2),
                     theme = bs_theme(bootswatch = "cosmo"),
                     footer = shiny::HTML("
                        <footer class='text-center text-sm-start' style='width:100%;'>
                        <hr>
                        <p class='text-center' style='font-size:12px;'>
                          Piotr Kosakowski i Tymoteusz Kwieciński
                        </p>
                        </footer>
                      "),
                     header = tags$head(tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css"))
                     # tags$head(tags$style('
                     #                       body {
                     #                          font-family: Arial; 
                     #                          font-size: 20px; 
                     #                          font-style: italic;
                     #                          color: red;
                     #                       }'
                     #                                         ))
                     
)




# Run the application 
shinyApp(ui = app_ui, server = server)
