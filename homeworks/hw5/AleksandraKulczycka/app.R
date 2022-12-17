#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#
# We used data about graduates from https://www.ela.nauka.gov.pl/pl/experts/source-data 
#
#
#

#library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)


# Let's get the data
#setwd("D:/IAD/sem3/wizualizacja danych/AleksandraKulczycka")
df  = read.csv("graduates-national-data.csv", sep=";",encoding = "UTF-8")



# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Comparison Of The Number Of Graduates In Different Fields Based On The Year"),
  textOutput("text"),
  
  # Sidebar with input selectors for different years and fields 
  sidebarLayout(
    sidebarPanel(
      selectInput("Year",
                  "Choose the year:",
                  c("2015" = "2015",
                    "2016" = "2016",
                    "2017" = "2017",
                    "2018" = "2018",
                    "2019" = "2019",
                    '2020' = "2020"),
                  uiOutput("variable")
      ),
      checkboxGroupInput("Fields",
                         "Choose the Field:",
                         choices = c(
                           "humanities",
                           "engineering sciences",
                           "medical and health sciences",
                           "agricultural sciences",
                           "science and natural sciences",
                           "social sciences",
                           "theological sciences",
                           "arts") -> checked,
                         selected = checked
      )),
    
    # Choosing a spinner
    mainPanel(
      shinycssloaders::withSpinner(plotly::plotlyOutput("myPlot", height =600),
                                   type = getOption("spinner.type", default = 8),
                                   color = getOption("spinner.color", default = 'pink'),
                                   color.background = getOption("spinner.color.background", default ='white'))
    )
  ))

# Define UI2 for application 

ui2 <- fluidPage(
  
  # Application title
  titlePanel("Comparison of Relative Unemployment Rate in The First Year After Obtaining a Diploma"),
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      selectInput("Year2",
                  "Choose the year:",
                  c("2015" = "2015",
                    "2016" = "2016",
                    "2017" = "2017",
                    "2018" = "2018",
                    "2019" = "2019",
                    '2020' = "2020"),
                  uiOutput("variable2")),
      
      radioButtons('Field', 'choose the field to compare:', 
                   x <- c("humanities",
                          "engineering sciences",
                          "medical and health sciences",
                          "agricultural sciences",
                          "science and natural sciences",
                          "social sciences",
                          "theological sciences",
                          "arts"),
                   selected = "humanities"),
      helpText("The chart shows a comparison of the relative unemployment rate of students who just graduated 
               in a particular field with the unemployment  rate for all first degree graduates in the country (depending on the year)")
    ),
    
    mainPanel(shinycssloaders::withSpinner(plotly::plotlyOutput("myPlot2",height =600),
                                           type = getOption("spinner.type", default = 2),
                                           color = getOption("spinner.color", default = 'pink'),
                                           color.background = getOption("spinner.color.background", default ='#FFDFF8'))
              
    )
  )
)

# Merge two UI in one app, choose theme and add a footer with the link
app_ui <- navbarPage(
  collapsible=TRUE,
  title = div(h3("Stats for the polish first degree graduates")),
  tabPanel("How many managed to survive?",icon = icon("book"), ui),
  tabPanel("Was it worth it?",icon = icon("globe"), ui2),
  theme = bslib::bs_theme( bg = "#FFDFF8", fg = "black", primary = "#FFB5EE",
                           bootswatch = "minty"),
  footer = shiny::HTML("
                  <footer class='text-center text-sm-start' style='width:100%;'>
                  <hr>
                  <p class='text-center' style='font-size:12px;'>
                    © 2022 Copyright:
                    <a class='text-dark' href='https://github.com/akulczycka'>Aleksandra</a>
                  </p>
                  </footer>
                  ")
)
# Define server logic required to draw plots
server <- function(input, output) {
  
  
  
  df <- df%>%
    filter(PL_POZIOM == "1")%>%
    select(PL_ROKDYP,
           PL_liczebnosc_D1,
           PL_liczebnosc_D2,
           PL_liczebnosc_D3,
           PL_liczebnosc_D4,
           PL_liczebnosc_D5,
           PL_liczebnosc_D6,
           PL_liczebnosc_D7,
           PL_liczebnosc_D8,
           PL_WWB,
           PL_WWB_GRUP_P1_D1,
           PL_WWB_GRUP_P1_D2,
           PL_WWB_GRUP_P1_D3,
           PL_WWB_GRUP_P1_D4,
           PL_WWB_GRUP_P1_D5,
           PL_WWB_GRUP_P1_D6,
           PL_WWB_GRUP_P1_D7,
           PL_WWB_GRUP_P1_D8)
  
  
  
  fields <- data.frame(name = c("PL_liczebnosc_D1",
                                "PL_liczebnosc_D2",
                                "PL_liczebnosc_D3",
                                "PL_liczebnosc_D4",
                                "PL_liczebnosc_D5",
                                "PL_liczebnosc_D6",
                                "PL_liczebnosc_D7",
                                "PL_liczebnosc_D8",
                                "PL_WWB"),
                       field = c(
                         "humanities",
                         "engineering sciences",
                         "medical and health sciences",
                         "agricultural sciences",
                         "science and natural sciences",
                         "social sciences",
                         "theological sciences",
                         "arts",
                         "wskaznik"
                       ))
  
  
  
  
  output$myPlot <- renderPlotly({
    
    helper <- df%>%
      filter(PL_ROKDYP == input$Year)%>%
      select(fields[fields$field %in% input$Fields, 1])
    toPlot <- as.data.frame(t(helper))
    
    p <- toPlot%>%
      ggplot(aes(x =  reorder(input$Fields,-toPlot[,1]) ,y = toPlot[,1],text = paste("chosen field:" ,reorder(input$Fields,-toPlot[,1]),"<br>number of graduates:",toPlot[,1])))+
      geom_col(fill = "#D369FF")+
      labs(x = "Fields",
           y = "Number of students that graduated")+
      theme_minimal()+
      scale_y_continuous(limits = c(0, 100000))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      theme(panel.grid.major.x = element_blank() ,
            panel.grid = element_line(color = "white",
                                      size = 0.75),
            plot.background = element_rect(fill = "#FFDFF8"),
            panel.background = element_rect(fill = "#FFDFF8",color = "white"))
    #editing hover because it is awful and annoys me 
    ggplotly(p, tooltip="text") %>% config(displayModeBar = F)
    #oops, for theological sciences it's too little number to be even visible on the plot. 
    
    
  })
  
  
  
  output$myPlot2 <- renderPlotly({
    fields <- fields%>%mutate(name = c("PL_WWB_GRUP_P1_D1",
                                       "PL_WWB_GRUP_P1_D2",
                                       "PL_WWB_GRUP_P1_D3",
                                       "PL_WWB_GRUP_P1_D4",
                                       "PL_WWB_GRUP_P1_D5",
                                       "PL_WWB_GRUP_P1_D6",
                                       "PL_WWB_GRUP_P1_D7",
                                       "PL_WWB_GRUP_P1_D8",
                                       "PL_WWB"))
    
    helper <- df%>%
      filter(PL_ROKDYP == input$Year2)%>%
      select(fields[fields$field %in% c(input$Field, 'wskaznik'),1])
    colnames(helper) <- c("chosen field","general")
    helper[["chosen field"]] <- as.numeric(gsub(",", ".", helper[["chosen field"]]))
    helper$general <- as.numeric(gsub(",", ".", helper$general))
    helper<-as.data.frame(t(helper))
    p <- helper%>%
      ggplot(aes(x =  rownames(helper),y =helper[,1] ,text = paste("Unemployment Rate:",helper[,1])))+
      geom_col(fill = "#D369FF")+
      labs(x = input$Field,
           y = "Relative Unemployment Rate" )+
      theme_minimal()+
      theme(panel.grid.major.x = element_blank() ,
            panel.grid = element_line(color = "white",
                                      size = 0.75),
            plot.background = element_rect(fill = "#FFDFF8"),
            panel.background = element_rect(fill = "#FFDFF8",color = "white"))+
      scale_y_continuous(limits = c(0, 1))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      geom_hline(yintercept = helper[,1][2],linetype="dashed",color = 'black')
    
    
    ggplotly(p, tooltip="text") %>% config(displayModeBar = F)
    
  })
}

# Run the application 
shinyApp(ui = app_ui, server = server)
