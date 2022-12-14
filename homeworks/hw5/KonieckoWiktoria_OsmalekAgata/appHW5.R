#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(dplyr)

min_bezrobocie <- function(df, rok){
    df %>% 
        filter(U_POZIOM == "1", U_N > 1000) %>% 
        select(U_ROKDYP, U_NAZWA_UCZELNI, U_WWB) -> df
    if(rok == 0) df %>% group_by(U_ROKDYP) -> df
    else{df %>% 
            filter(U_ROKDYP == rok) %>% 
            select(U_NAZWA_UCZELNI, U_WWB)} -> df
    df %>% 
        arrange(U_WWB) %>% 
        slice_head(n=10)
}

#df_major<-read.csv(file = "../graduates-major-data.csv", sep = ';')
#df_graduates<-read.csv(file = "../graduates-national-data.csv", sep = ';')
#df_institution<-read.csv(file = "../graduates-institution-data.csv", sep = ';')
df_students<-read.csv(file = "./students-national-data.csv", sep = ';')
df <- read.csv("graduates-institution-data.csv", sep = ";")
df$U_WWB <- as.double(lapply(
    strsplit(df$U_WWB, ","), FUN = paste, collapse = "."))

ui1 <- fluidPage(
    
    # Application title
    titlePanel("Duże uczelnie (>1000 absolwentów) o najniższym
               wskaźniku bezrobocia u absolwentów studiów 1. stopnia"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("rok", "Rok:", 2014:2020)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tableOutput("bezrobocie.tabelka")
        )
    ),
    plotlyOutput("bezrobocie.lineplot")
)

# Define UI for application that draws a histogram  students-national-data.csv
ui2 <- fluidPage(

    # Application title
    titlePanel("Stałość w wyborze kierunku studiów w Polsce po ukończeniu 1. roku kursu"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("what",
                      "Czy interesuje cię zmiana kierunku, czy przerwanie studiów?",
                      c("Zmiana", "Przerwa")
                      ),
          selectInput("studies",
                      "Długość kursu w latach:",
                      2:6
          )
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlotly({
        if(input$what == "Zmiana"){
          what <- df_students[,grepl(paste("PL_",input$studies,"L_KOREKTA_1", sep = ""),colnames(df_students))]
        }
        else{
          what <- df_students[,grepl(paste("PL_",input$studies,"L_PRZERWA_1", sep = ""),colnames(df_students))]
        }
      what<-stringr::str_replace(what, ",", ".")
      what <- as.data.frame(cbind(df_students$PL_ROK_OD, df_students$PL_POZIOM,what))
      colnames(what)<-c("Year","Degree","Percentage_of_students")
      what$Percentage_of_students<-as.numeric(what$Percentage_of_students)
      plot_ly(
        data = what,
        x = ~Year,
        y = ~Percentage_of_students,
        color = ~Degree,
        type = "bar") %>% 
        layout(
          title = "Procent studentów różnych stopni w latach 2015-2018",
          xaxis = list(title = "Rok"),
          yaxis = list(title = "Procent studentów[%]", range= c(0,100)),
          showlegend = T,
          legend=list(title=list(text='<b> Stopień studiów </b>'))
        )
    })
    output$bezrobocie.tabelka <- renderTable({
        min_bezrobocie(df, input$rok) %>% 
            rename("Wskaźnik bezrobocia" = "U_WWB", "Nazwa uczelni" = "U_NAZWA_UCZELNI")
    })
    output$bezrobocie.lineplot <- renderPlotly({
        temp_df <- min_bezrobocie(df, 0)
        temp_nazwy <- temp_df %>%
            group_by(U_NAZWA_UCZELNI) %>%
            count() %>%
            filter(n==7)
        temp_nazwy <- temp_nazwy$U_NAZWA_UCZELNI
        temp_df %>%
            filter(U_NAZWA_UCZELNI %in% temp_nazwy) %>% 
            ggplot(aes(x = U_ROKDYP, y = U_WWB, colour = U_NAZWA_UCZELNI)) +
            geom_line() +
            scale_y_continuous(limits = c(0, max(temp_df$U_WWB))) +
            labs(x = "Rok", y = "Wskaźnik bezrobocia")
            theme_minimal()
        ggplotly() %>% 
            style(hovertemplate = "Wskaźnik bezrobocia: %{y}")
    })
}

ui <- navbarPage("HW5", tabPanel("1", ui1), tabPanel("2",ui2))

# Run the application 
shinyApp(ui = ui, server = server)
