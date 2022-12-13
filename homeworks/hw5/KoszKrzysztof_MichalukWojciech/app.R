
library(shiny)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(plotly)
library(bslib)

data <- read.csv2("doctors-national-discipline-data.csv")
#View(data)

selecData <- data %>% select(PLDZ_DZIEDZINA_EXE, PLDZ_ROKDYP, PLDZ_PROC_MIES_PRACA, PLDZ_PROC_MIES_ETAT, PLDZ_PROC_MIES_SAMOZ) 

selecData <- selecData %>% replace(is.na(.), 0)

selecData <- mutate(selecData, PLDZ_DZIEDZINA_EXE=case_when(
  PLDZ_DZIEDZINA_EXE=="Dziedzina nauk inĹĽynieryjno-technicznych"~"Dziedzina nauk inżynieryjno-technicznych",
  PLDZ_DZIEDZINA_EXE=="Dziedzina nauk spoĹ‚ecznych"~"Dziedzina nauk społecznych",
  PLDZ_DZIEDZINA_EXE=="Dziedzina nauk Ĺ›cisĹ‚ych i przyrodniczych"~"Dziedzina nauk ścisłych i przyrodniczych",
  TRUE~PLDZ_DZIEDZINA_EXE
))
df<-read.csv2("doctors-national-discipline-data.csv")

df<-df[!startsWith(colnames(df), "PLDZ_E_ZAR_ETAT")]

colnames(df)[1]<-"PLDZ_E_ZAR_ROKDYP"
colnames(df)[2]<-"PLDZ_E_ZAR_DZIEDZINA_EXE"

df<-df%>%select(starts_with("PLDZ_E_ZAR"))

colnames(df)[1]<-"PLDZ_ROKDYP"
colnames(df)[2]<-"Dziedzina_nauk"

df<-mutate(df, Dziedzina_nauk=case_when(
  Dziedzina_nauk=="Dziedzina nauk inĹĽynieryjno-technicznych"~"Dziedzina nauk inżynieryjno-technicznych",
  Dziedzina_nauk=="Dziedzina nauk spoĹ‚ecznych"~"Dziedzina nauk społecznych",
  Dziedzina_nauk=="Dziedzina nauk Ĺ›cisĹ‚ych i przyrodniczych"~"Dziedzina nauk ścisłych i przyrodniczych",
  TRUE~Dziedzina_nauk
))



uiK <- fluidPage(

    titlePanel("Średnie wynagrodzenie doktorów po otrzymaniu dyplomu na przestrzeni lat w zależności od dziedziny [w PLN]"),
 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("dziedziny",
                               "Dziedziny, które mają wyświetlić się na wykresie", 
                               choices=unique(df$Dziedzina_nauk),
                               selected="Dziedzina nauk ścisłych i przyrodniczych"),
            selectInput("typ",
                        "Typ średniego wynagrodzenia",
                        choices=c("Ze wszystkich źródeł w okresach pracy na uczelni",
                                  "Z pracy na uczelni",
                                  "Ze wszystkich źródeł w okresach niepracowania na uczelni",
                                  "Ze wszystkich źródeł"))
        ),

        mainPanel(
           plotOutput("linePlot")
        )
    )
)
uiW <- fluidPage(
  titlePanel("Procent zatrudnionych osób po ukończeniu doktoratu"),
  sidebarLayout(
    sidebarPanel(
      selectInput("forma",
                  "Forma zatrudnienia",
                  choices=c("Umowa o pracę",
                            "Samozatrudnienie",
                            "Jakakolwiek forma zatrudnienia")),
      selectInput("dziedzina",
                  "Dziedzina uzyskania doktoratu",
                  choices=unique(selecData$PLDZ_DZIEDZINA_EXE))
    ),
    mainPanel(
      plotOutput("geomPlot")
    )
  )
  
)


server <- function(input, output) {
  output$linePlot<-renderPlot({
    dfplot<-df%>%filter(Dziedzina_nauk %in% input$dziedziny)
    
    ggplot(dfplot, aes_string(x=dfplot$PLDZ_ROKDYP, y=case_when(
      input$typ=="Ze wszystkich źródeł w okresach pracy na uczelni"~dfplot$PLDZ_E_ZAR_UCZ,
      input$typ=="Z pracy na uczelni"~dfplot$PLDZ_E_ZAR_PRACAUCZ,
      input$typ=="Ze wszystkich źródeł w okresach niepracowania na uczelni"~dfplot$PLDZ_E_ZAR_NUCZ,
      input$typ=="Ze wszystkich źródeł"~dfplot$PLDZ_E_ZAR
    ), color='Dziedzina_nauk'))+
      geom_line()+
      labs(x="rok",
           y="Średnie wynagrodzenie w PLN")+
      theme_bw()
  })
  
  output$geomPlot<-renderPlot({
    dfPlot <- selecData%>%filter(PLDZ_DZIEDZINA_EXE %in% input$dziedzina)
    
    
    yData <- case_when(
      input$forma=="Umowa o pracę"~dfPlot$PLDZ_PROC_MIES_ETAT,
      input$forma=="Samozatrudnienie"~dfPlot$PLDZ_PROC_MIES_SAMOZ,
      input$forma=="Jakakolwiek forma zatrudnienia"~dfPlot$PLDZ_PROC_MIES_PRACA)
    
    
    ggplot(dfPlot, aes(x=PLDZ_ROKDYP, y=yData))+
      geom_bar(stat='identity', position = "fill", width = 0.7)+
      geom_col(fill="lightblue") +
      geom_text(aes(label = yData)) +
      theme_minimal() +
      coord_cartesian(ylim=c(0,100))+
      labs(x ="Rok uzyskania dyplomu", y="Procent zatrudnionych") +
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
  })
    
}


app_ui <- navbarPage(
  title = "Analiza sytuacji doktorów na rynku pracy",
  tabPanel("Porównanie zarobków względem dziedzin nauki", uiK),
  tabPanel("Porównanie typu zatrudnienia względem dziedzin nauki", uiW),
  theme = bslib::bs_theme(bootswatch = "cosmo")
)

shinyApp(app_ui, server)