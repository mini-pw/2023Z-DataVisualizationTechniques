library(dplyr)
library(ggplot2)
library(shiny)
library(plotly)
library(shinythemes)
Sys.setlocale(category="LC_ALL", locale = "polish")

df<-read.csv("graduates-major-data.csv",header=TRUE,sep=";",encoding="UTF-8",stringsAsFactors=FALSE)
head(df)

to_numeric<-function(x){
  as.numeric(gsub(",", ".", x))
}
df<-df %>% 
  select(P_KIERUNEK_NAZWA,P_DZIEDZINA,P_WOJ,P_CZAS_PRACA,P_CZAS_PRACA_NDOSW,P_CZAS_PRACA_DOSW,P_ROKDYP,
         P_E_ZAR_DOSW,
         P_E_ZAR_NDOSW,
         P_E_ZAR_DOSW_P1,
         P_E_ZAR_NDOSW_P1,
         P_E_ZAR_DOSW_P2,
         P_E_ZAR_NDOSW_P2,
         P_E_ZAR_DOSW_P3,
         P_E_ZAR_NDOSW_P3,
         P_E_ZAR_DOSW_P4,
         P_E_ZAR_NDOSW_P4,
         P_E_ZAR_DOSW_P5,
         P_E_ZAR_NDOSW_P5
  ) 
  
df[c("P_CZAS_PRACA","P_CZAS_PRACA_NDOSW","P_CZAS_PRACA_DOSW",
     "P_E_ZAR_DOSW",
     "P_E_ZAR_NDOSW",
     "P_E_ZAR_DOSW_P1",
     "P_E_ZAR_NDOSW_P1",
     "P_E_ZAR_DOSW_P2",
     "P_E_ZAR_NDOSW_P2",
     "P_E_ZAR_DOSW_P3",
     "P_E_ZAR_NDOSW_P3",
     "P_E_ZAR_DOSW_P4",
     "P_E_ZAR_NDOSW_P4",
     "P_E_ZAR_DOSW_P5",
     "P_E_ZAR_NDOSW_P5")]<-sapply(df[c("P_CZAS_PRACA","P_CZAS_PRACA_NDOSW","P_CZAS_PRACA_DOSW",
                                       "P_E_ZAR_DOSW",
                                       "P_E_ZAR_NDOSW",
                                       "P_E_ZAR_DOSW_P1",
                                       "P_E_ZAR_NDOSW_P1",
                                       "P_E_ZAR_DOSW_P2",
                                       "P_E_ZAR_NDOSW_P2",
                                       "P_E_ZAR_DOSW_P3",
                                       "P_E_ZAR_NDOSW_P3",
                                       "P_E_ZAR_DOSW_P4",
                                       "P_E_ZAR_NDOSW_P4",
                                       "P_E_ZAR_DOSW_P5",
                                       "P_E_ZAR_NDOSW_P5")],to_numeric)


df1<-df %>% 
  group_by(P_DZIEDZINA) %>% 
  summarise(P_CZAS_PRACA=mean(P_CZAS_PRACA,na.rm=TRUE),P_CZAS_PRACA_NDOSW=mean(P_CZAS_PRACA_NDOSW,na.rm=TRUE),P_CZAS_PRACA_DOSW=mean(P_CZAS_PRACA_DOSW,na.rm=TRUE)) %>% 
  filter(P_DZIEDZINA!="")

df2<-df %>% 
  mutate(P_DZIEDZINA=substr(P_DZIEDZINA,start=11, stop=nchar(P_DZIEDZINA))) %>% 
  filter(P_DZIEDZINA!="")


ui <- fluidPage(
  theme = shinytheme("cyborg"),
  navbarPage(
    theme = "flatly",
    "Analiza danych o absolwentach szkół wyższych w Polsce",
    tabPanel("Średnie zarobki według wybranych dziedzin",
             fluidRow(column(width=12,
                             checkboxGroupInput("dziedziny",
                                                "Wybierz dziedziny (max 3)",
                                                unique(df2$P_DZIEDZINA),
                                                selected="sztuki",
                                                ),
                             h4(textOutput("tytuł2")),
                             actionButton("doswiadczeni", "doswiadczeni"),
                             actionButton("niedoswiadczeni", "niedoświadczeni"),
                             
                             plotlyOutput('violinPlot')
             )),
    ),
    tabPanel("Czas potrzebny do znalezienia pracy",
             sidebarLayout( 
               
               sidebarPanel(
                 selectInput(inputId = "ColumnPlot",
                             label = "Wybierz rodzaj absolwentów",
                             choices = c("ogółem","osoby bez doświadczenia w trakcie studiów", "osoby z doświadczeniem w trakcie studiów"),
                             selected = "ogółem",
                             multiple = FALSE),
                 radioButtons(
                   "kolor1",
                   "Kolor wykresu:",
                   choices = c("złoty"="#d2ac47",
                               "srebrny"="#C0C0C0",
                               "niebieski"="blue"))
               ),
               mainPanel(
                 h4(textOutput("tytuł1")),
                 plotlyOutput("columnPlot")
               )
               ),
    ))
  
)

server <- function(input, output, session) {
  jakieDaneColumnPlot <- reactive({
    switch(input$ColumnPlot,
           "ogółem" = "P_CZAS_PRACA",
           "osoby bez doświadczenia w trakcie studiów" = "P_CZAS_PRACA_NDOSW",
           "osoby z doświadczeniem w trakcie studiów" = "P_CZAS_PRACA_DOSW",

    )
  })
  
  observe({
    if(length(input$dziedziny) > 3){
      updateCheckboxGroupInput(session,"dziedziny", selected = tail(input$dziedziny,3))
    }
    if(length(input$dziedziny) < 1){
      updateCheckboxGroupInput(session,"dziedziny", selected= "sztuki")
    }
  })
  
  v <- reactiveValues(data = "P_E_ZAR_DOSW")
  
  observeEvent(input$doswiadczeni, {
    v$data <- "P_E_ZAR_DOSW"
  })
  
  observeEvent(input$niedoswiadczeni, {
    v$data <- "P_E_ZAR_NDOSW" 
  })  

  output$columnPlot<-renderPlotly({
    ggplotly(
      ggplot(data=df1,aes(na.rm=TRUE))+
        geom_col(aes_string(x="P_DZIEDZINA", y=jakieDaneColumnPlot()),fill=input$kolor1)+
        labs(x="Dziedzina", y = "miesiące")+
        ggtitle(paste("Czas potrzebny do znalezienia zatrudnienia w miesiącach ", input$ColumnPlot))+
        coord_flip()+
        scale_y_continuous(limits = c(0, 14))+
        theme(panel.background = element_rect(fill = 'black', color='black'))+
        theme(plot.background = element_rect(fill = 'black', color='black'))+
        theme(axis.text.y = element_text(color='white'))+
        theme(axis.text.x = element_text(color='white'))+
        theme(axis.title.x = element_text(color='white'))
      )
  })
  
  output$tytuł1<-renderText(paste("Czas potrzebny do znalezienia zatrudnienia w miesiącach ", input$ColumnPlot))

  output$tytuł2<-renderText(paste("Rozłożenie średnich zarobków w zależności od dziedziny i doświadczenia (pracy zawodowej) w trakcie studiów"))
  
  output$violinPlot<-renderPlotly({
    ggplotly(
      ggplot(df2[df2$P_DZIEDZINA %in% input$dziedziny,],aes(na.rm=TRUE))+
        geom_violin(aes_string(x="P_DZIEDZINA", y=v$data),color="red4",fill="red4")+
        labs(x="Dziedzina", y = "Zarobki w zł")+
        scale_y_continuous(limits=c(0,20000))+
        theme(panel.background = element_rect(fill = 'black', color='black'))+
        theme(plot.background = element_rect(fill = 'black', color='black'))+
        theme(axis.text.y = element_text(color='white'))+
        theme(axis.text.x = element_text(color='white'))+
        theme(axis.title.x = element_text(color='white'))+
        theme(axis.title.y = element_text(color='white'))
    )
  })
  
  
  }

shinyApp(ui = ui, server = server)

