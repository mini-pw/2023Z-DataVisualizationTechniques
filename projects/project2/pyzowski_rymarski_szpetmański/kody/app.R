#Załączenie bibliotek

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(bslib)
library(dplyr)
library(rsconnect)
library(packrat)
library(lubridate)
library(rvest)
library(tm)
library(jsonlite)
library(tidyverse)
library(data.table)
library(magrittr)
library(shinycssloaders)
library(tidytext)


#####
#
# Rafał part (nie odpowiadam za redundancję w niższych częściach kodu)
#
#####
{android_act_rafal <- as.data.frame(fromJSON("Moja_aktywnosc_rafal.json"))
android_act_maciek <- as.data.frame(fromJSON("Moja_aktywnosc_maciek.json"))

android_act_rafal %>% 
  select(c("header", "time")) -> android_act_rafal
android_act_maciek %>% 
  select(c("header", "time")) %>% 
  filter(header != "com.wssyncmldm") %>% 
  filter(header != "Samsung One UI Home") -> android_act_maciek

android_act_rafal[android_act_rafal == "com.android.deskclock"] <- "Zegar"
android_act_rafal[android_act_rafal == "com.android.gallery3d"] <- "Galeria"
android_act_rafal[android_act_rafal == "com.huawe.camera"] <- "Aparat"
android_act_rafal[android_act_rafal == "com.android.contacts"] <- "Kontakty"
android_act_rafal[android_act_rafal == "com.example.android.notepad"] <- "Notatnik"
android_act_rafal[android_act_rafal == "com.huawei.parentcontrol"] <- "Kontrola rodzicielska"
android_act_rafal[android_act_rafal == "com.android.mediacenter"] <- "Centrum multimediów"

android_act_maciek[android_act_maciek == "com.samsung.android.mcfds"] <- "Samsung Continuity Service"
android_act_maciek[android_act_maciek == "com.samsung.android.app.dressroom"] <- "Dressing Room"
android_act_maciek[android_act_maciek == "com.sec.android.app.camera"] <- "Camera"


df_r <- android_act_rafal %>% 
  mutate(date = substr(time, 1, 10))
df_m <- android_act_maciek %>% 
  mutate(date = substr(time, 1, 10))}

ui_rafal <- fluidPage(
  titlePanel("Najpopularniejsze aplikacje"),
  
  fluidRow(
    column(2,
           fluidRow(
             selectInput("student_r",
                         "Którego studenta wybierasz?",
                         choices=c("Rafał"="Rafal", "Maciek"="Maciek"))
           ),
           fluidRow(
             sliderInput(
               inputId = "androidSlider",
               label = "podaj zakres dat",
               min = as.Date(min(df_r$date),"%Y-%m-%d"),
               max = as.Date(max(df_r$date),"%Y-%m-%d"),
               value=as.Date(c(min(df_r$date), max(df_r$date))),
               timeFormat="%Y-%m-%d"
             )
           ),
           fluidRow(
             numericInput(
               inputId = "android_n",
               label = "wybierz liczbę aplikacji",
               value = 5,
               min = 3, 
               max = 10,
               step = 1
             )
           )
    ),
    column(10,
           fluidRow(
             shinycssloaders::withSpinner(plotOutput("top_n_apps", height = "700px"),
                                          type = getOption("spinner.type", default = 6),
                                          color = getOption("spinner.color", default = "#0275D8"),
                                          size = getOption("spinner.size", default = 1))
           )  
    )
  )
  
)





#####
#
# Kuba part
#
#####

{
  resK <- jsonlite::read_json("BrowserHistory_Kuba.json")
  resR <- jsonlite::read_json("BrowserHistory_rafal.json")
  resM <- jsonlite::read_json("BrowserHistory_maciej.json")
  
  dfK <- as.data.frame(rbindlist(resK[[1]], fill = TRUE))
  dfR <- as.data.frame(rbindlist(resR[[1]], fill = TRUE))
  dfM <- as.data.frame(rbindlist(resM[[1]], fill = TRUE))
  
  df2K<-dfK %>% select("url", "time_usec") %>% mutate(data=as.POSIXct(time_usec / 1e6, origin="1970-01-01")) %>% 
    select("url", "data")
  df2R<-dfR %>% select("url", "time_usec") %>% mutate(data=as.POSIXct(time_usec / 1e6, origin="1970-01-01")) %>% 
    select("url", "data")
  
  monatK<-df2K %>% mutate(miesiąc=month(data))
  monatR<-df2R %>% mutate(miesiąc=month(data))
  
  monat2K<-monatK %>% group_by(miesiąc) %>% 
    summarise(total_count=n(),
              .groups="drop")
  monat2R<-monatR %>% group_by(miesiąc) %>% 
    summarise(total_count=n(),
              .groups="drop")
  monat2KR=merge(x=monat2R, y=monat2K, by = "miesiąc", all.x = TRUE)
  colnames(monat2KR)=c("miesiąc" ,"rafal", "kuba")
  
  godzK<-df2K %>% mutate(godzina=hour(data))
  godzR<-df2R %>% mutate(godzina=hour(data))
  
  godz2K<-godzK %>% group_by(godzina) %>% 
    summarise(total_count=n(),
              .groups="drop")
  
  godz2R<-godzR %>% group_by(godzina) %>% 
    summarise(total_count=n(),
              .groups="drop")
  
  godz2KR=merge(x=godz2R, y=godz2K, by = "godzina", all.x = TRUE)
  colnames(godz2KR)=c("godzina" ,"rafal", "kuba")
  
  nazwy_mies=c("sty", "lut", "mar", "kwi", "maj", "lip", "sie", "wrz", "paź", "lis", "gru")
  
  # Tworzenie  aplikacji
  
  
}

ui_kuba <- fluidPage(
  titlePanel("Kiedy wyszukiwaliśmy? (dane z 2022 roku)"),
  
  fluidRow(
    column(2,
           fluidRow(
             radioGroupButtons(
               inputId = "change_plot",
               label = "Co Cię interesuje?",
               choices = c(
                 "miesiące" = "mies",
                 "godziny" = "godz"
               )
             )
           ),
           fluidRow(
             selectInput("student",
                         "Którego studenta wybierasz?",
                         choices=c("Kuba"="kuba", "Rafał"="rafal"))
           ),
           fluidRow(
             sliderInput("zakres",
                         "Miesiąc",
                         value = c(1, 12),
                         min = 1,
                         max = 12,
                         step = 1)
           )  
    ),
    column(10,
           shinycssloaders::withSpinner(plotOutput("histPlot", height = "700px"),
                                        type = getOption("spinner.type", default = 6),
                                        color = getOption("spinner.color", default = "#0275D8"),
                                        size = getOption("spinner.size", default = 1))
    )
  ) 
)




#####
#
# Maciek part
#
#####

{
  BSK <- jsonlite::read_json("BrowserHistory_Kuba.json")
  BSR <- jsonlite::read_json("BrowserHistory_rafal.json")
  
  dfK_m <- as.data.frame(rbindlist(BSK[[1]], fill = TRUE))
  dfR_m <- as.data.frame(rbindlist(BSR[[1]], fill = TRUE))
  
  dfK_m[dfK_m == "AUTO_BOOKMARK"] <- "Zakładka"
  dfK_m[dfK_m == "AUTO_TOPLEVEL"] <- "Strona startowa"
  dfK_m[dfK_m == "FORM_SUBMIT"] <- "Zatwierdzenie formularza"
  dfK_m[dfK_m == "GENERATED"] <- "Wygenerowany"
  dfK_m[dfK_m == "KEYWORD"] <- "Słowo kluczowe"
  dfK_m[dfK_m == "LINK"] <- "Link"
  dfK_m[dfK_m == "RELOAD"] <- "Odświeżenie strony"
  dfK_m[dfK_m == "TYPED"] <- "Wpisany"
  
  dfR_m[dfR_m == "AUTO_BOOKMARK"] <- "Zakładka"
  dfR_m[dfR_m == "AUTO_TOPLEVEL"] <- "Strona startowa"
  dfR_m[dfR_m == "FORM_SUBMIT"] <- "Zatwierdzenie formularza"
  dfR_m[dfR_m == "GENERATED"] <- "Wygenerowany"
  dfR_m[dfR_m == "KEYWORD"] <- "Słowo kluczowe"
  dfR_m[dfR_m == "LINK"] <- "Link"
  dfR_m[dfR_m == "RELOAD"] <- "Odświeżenie strony"
  dfR_m[dfR_m == "TYPED"] <- "Wpisany"
  
  dfK_m<-dfK_m %>% select("page_transition", "time_usec") %>% mutate(data=as.POSIXct(time_usec / 1e6, origin="1970-01-01")) %>% 
    select("page_transition", "data")
  
  dfR_m<-dfR_m %>% select("page_transition", "time_usec") %>% mutate(data=as.POSIXct(time_usec / 1e6, origin="1970-01-01")) %>% 
    select("page_transition", "data")
  
  miesiacK<-dfK_m %>% mutate(miesiac=month(data))
  miesiacR<-dfR_m %>% mutate(miesiac=month(data))
  
  miesiacK<-miesiacK %>% group_by(page_transition,miesiac) %>% 
    summarise(total_count=n(),
              .groups="drop")
  miesiacR<-miesiacR %>% group_by(page_transition,miesiac) %>% 
    summarise(total_count=n(),
              .groups="drop")
  
  tabela=merge(x=miesiacR, y=miesiacK, by = "miesiac", all.x = TRUE)
  colnames(tabela)=c("miesiac" ,"Rafal_wysz","Rafal","Kuba_wysz", "Kuba")
  
  godzinaK<-dfK_m %>% mutate(godzina=hour(data))
  godzinaR<-dfR_m %>% mutate(godzina=hour(data))
  
  godzinaK<-godzinaK %>% group_by(page_transition,godzina) %>% 
    summarise(total_count=n(),
              .groups="drop")
  godzinaR<-godzinaR %>% group_by(page_transition,godzina) %>% 
    summarise(total_count=n(),
              .groups="drop")
  
  tabela_godz=merge(x=godzinaR, y=godzinaK, by = "godzina", all.x = TRUE)
  colnames(tabela_godz)=c("godzina" ,"Rafal_wysz","Rafal","Kuba_wysz", "Kuba")
  
  
}

ui_maciek <- fluidPage(
  titlePanel("W jaki sposób wyszukiwaliśmy? (dane z 2022 roku)"),
  
  fluidRow(
    column(2,
           fluidRow(
             radioGroupButtons(
               inputId = "change_plot_m",
               label = "Co Cię interesuje?",
               choices = c(
                 "miesiące" = "mies",
                 "godziny" = "godz"
               )
             )
           ),
           fluidRow(
             selectInput("student_m",
                         "Którego studenta wybierasz?",
                         choices=c("Kuba"="Kuba", "Rafał"="Rafal"))
           ),
           fluidRow(
             sliderInput("zakres_m",
                         "Miesiąc",
                         value = c(1, 12),
                         min = 1,
                         max = 12,
                         step = 1)
           )
    ),
    column(10,
           shinycssloaders::withSpinner(plotOutput("histPlot_m", height = "700px"),
                                        type = getOption("spinner.type", default = 6),
                                        color = getOption("spinner.color", default = "#0275D8"),
                                        size = getOption("spinner.size", default = 1))
    )
  )
)




server <- function(input, output, session) {
  
  
  output$top_n_apps <- renderPlot({
    
    if(input$student_r %in% "Maciek") {
      df <- df_m
    } else {
      df <- df_r
    }
    
    df %>% 
      filter(header != "com.huawei.android.launcher") %>% 
      filter(date >=  input$androidSlider[1] & date <= input$androidSlider[2]) %>% 
      group_by(header) %>% 
      summarise(n = n()) %>% 
      arrange(desc(n)) %>% 
      head(input$android_n) %>% 
      ggplot(aes(y = reorder(header, n), x = n)) +
      geom_bar(stat = "identity", fill = "#3DDC84") +
      theme(rect = element_blank(),
            legend.position = "none",
            axis.line.y = element_line(color = "black"),
            plot.title = element_text(hjust = 0.5, size=25),
            axis.text.x = element_text(size=15, color="black"),
            axis.text.y = element_text(size=15, color="black"),
            axis.title.x = element_text(size=15, color="black"),
            axis.title.y = element_text(size=15, color="black"),
            panel.grid.major.x = element_line(color="lightgrey")) +
      labs(title = "",
           x = "Liczba sesji aplikacji",
           y = "") 
    
    
  })
  
  output$histPlot <- renderPlot({
    if (input$change_plot %in% "mies"){
      updateSliderInput(session, 
                        "zakres",
                        "Miesiące",
                        value = c(input$zakres[1], input$zakres[2]),
                        min = 1,
                        max = 12,
                        step = 1)
      ggplot(monat2KR %>% filter(miesiąc >=input$zakres[1], miesiąc <=input$zakres[2]) %>% 
               rename(student = input$student), 
             aes(x=miesiąc, y=student, fill=student))+
        scale_fill_gradient(low="yellow",high="red")+
        geom_col()+
        scale_y_continuous(expand = expansion(mult =c(0, 0.1)))+
        theme(rect = element_blank(),
              legend.position = "none",
              axis.line.x = element_line(color = "black"),
              plot.title = element_text(hjust = 0.5, size=25),
              axis.text.x = element_text(size=15, color="black"),
              axis.text.y = element_text(size=15, color="black"),
              axis.title.x = element_text(size=15, color="black"),
              axis.title.y = element_text(size=15, color="black"),
              panel.grid.major.y = element_line(color="lightgrey"))+
        labs(
          x = "miesiąc",
          y = "liczba wyszukiwań",
          title = "Liczba wyszukiwań względem miesiąca"
        )+
        scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
    }else{
      updateSliderInput(session, 
                        "zakres",
                        "Godziny",
                        value = c(input$zakres[1], input$zakres[2]),
                        min = 0,
                        max = 23,
                        step = 1)
      ggplot(godz2KR %>% filter(godzina >=input$zakres[1], godzina <=input$zakres[2]) %>%
               rename(student = input$student),
             aes(x=godzina, y=student, fill=student))+
        scale_fill_gradient(low="yellow",high="red")+
        geom_col()+
        scale_y_continuous(expand = expansion(mult =c(0, 0.1)))+
        theme(rect = element_blank(),
              legend.position = "none",
              axis.line.x = element_line(color = "black"),
              plot.title = element_text(hjust = 0.5, size=25),
              axis.text.x = element_text(size=15, color="black"),
              axis.text.y = element_text(size=15, color="black"),
              axis.title.x = element_text(size=15, color="black"),
              axis.title.y = element_text(size=15, color="black"),
              panel.grid.major.y = element_line(color="lightgrey"))+
        labs(
          x = "godzina",
          y = "liczba wyszukiwań",
          title = "Liczba wyszukiwań względem godziny"
        )+
        scale_x_continuous(breaks=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23))
    }
  })
  
  output$histPlot_m <- renderPlot({
    if(input$student_m %in% "Kuba"){
      if (input$change_plot_m %in% "mies"){
        updateSliderInput(session, 
                          "zakres_m",
                          "Miesiące",
                          value = c(input$zakres_m[1], input$zakres_m[2]),
                          min = 1,
                          max = 12,
                          step = 1)
        ggplot(tabela %>% filter(miesiac >=input$zakres_m[1], miesiac <=input$zakres_m[2]) %>% 
                 rename(student_m = "Kuba"), aes(miesiac, student_m, fill = Kuba_wysz)) +
          geom_bar(stat="identity", position = "dodge")+
          labs(title = "Sposoby wyszukiwania w poszczególnych miesiącach", x = "Miesiąc", y = "Liczba wyszukiwań", fill = "Kuba") +
          theme(rect = element_blank(),
                axis.line.x = element_line(color = "black"),
                plot.title = element_text(hjust = 0.5, size=25),
                axis.text.x = element_text(size=15, color="black"),
                axis.text.y = element_text(size=15, color="black"),
                axis.title.x = element_text(size=15, color="black"),
                axis.title.y = element_text(size=15, color="black"),
                panel.grid.major.y = element_line(color="lightgrey"),
                legend.key.size = unit(2, "cm"),
                legend.title = element_text(size = 18),
                legend.text = element_text(size = 14)) +
          guides(fill = guide_legend(title="Typ przejścia")) +
          scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
      }else{
        updateSliderInput(session, 
                          "zakres_m",
                          "Godziny",
                          value = c(input$zakres_m[1], input$zakres_m[2]),
                          min = 0,
                          max = 23,
                          step = 1)
        ggplot(tabela_godz %>% filter(godzina >=input$zakres_m[1], godzina <=input$zakres_m[2]) %>% 
                 rename(student_m = "Kuba"), aes(godzina, student_m, fill = Kuba_wysz)) +
          geom_bar(stat="identity", position = "dodge") +
          labs(title = "Sposoby wyszukiwania w poszczególnych godzinach", x = "Godzina", y = "Liczba wyszukiwań", fill = "Kuba") +
          theme(rect = element_blank(),
                axis.line.x = element_line(color = "black"),
                plot.title = element_text(hjust = 0.5, size=25),
                axis.text.x = element_text(size=15, color="black"),
                axis.text.y = element_text(size=15, color="black"),
                axis.title.x = element_text(size=15, color="black"),
                axis.title.y = element_text(size=15, color="black"),
                panel.grid.major.y = element_line(color="lightgrey"),
                legend.key.size = unit(2, "cm"),
                legend.title = element_text(size = 18),
                legend.text = element_text(size = 14)) +
          guides(fill = guide_legend(title="Typ przejścia")) +
          scale_x_continuous(breaks=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23))
      }
    }else{if (input$change_plot_m %in% "mies"){
      updateSliderInput(session, 
                        "zakres_m",
                        "Miesiące",
                        value = c(input$zakres_m[1], input$zakres_m[2]),
                        min = 1,
                        max = 12,
                        step = 1)
      ggplot(tabela %>% filter(miesiac >=input$zakres_m[1], miesiac <=input$zakres_m[2]) %>% 
               rename(student_m = "Rafal"), aes(miesiac, student_m, fill = Rafal_wysz)) +
        geom_bar(stat="identity", position = "dodge")+
        labs(title = "Sposoby wyszukiwania w poszczególnych miesiącach", x = "Miesiąc", y = "Liczba wyszukiwań", fill = "Rafal") +
        theme(rect = element_blank(),
              axis.line.x = element_line(color = "black"),
              plot.title = element_text(hjust = 0.5, size=25),
              axis.text.x = element_text(size=15, color="black"),
              axis.text.y = element_text(size=15, color="black"),
              axis.title.x = element_text(size=15, color="black"),
              axis.title.y = element_text(size=15, color="black"),
              panel.grid.major.y = element_line(color="lightgrey"),
              legend.key.size = unit(2, "cm"),
              legend.title = element_text(size = 18),
              legend.text = element_text(size = 14)) +
        guides(fill = guide_legend(title="Typ przejścia")) +
        scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
    }else{
      updateSliderInput(session, 
                        "zakres_m",
                        "Godziny",
                        value = c(input$zakres_m[1], input$zakres_m[2]),
                        min = 0,
                        max = 23,
                        step = 1)
      ggplot(tabela_godz %>% filter(godzina >=input$zakres_m[1], godzina <=input$zakres_m[2]) %>% 
               rename(student_m = "Rafal"), aes(godzina, student_m, fill = Rafal_wysz)) +
        geom_bar(stat="identity", position = "dodge") +
        labs(title = "Sposoby wyszukiwania w poszczególnych godzinach", x = "Godzina", y = "Liczba wyszukiwań", fill = "Rafal") +
        theme(rect = element_blank(),
              axis.line.x = element_line(color = "black"),
              plot.title = element_text(hjust = 0.5, size=25),
              axis.text.x = element_text(size=15, color="black"),
              axis.text.y = element_text(size=15, color="black"),
              axis.title.x = element_text(size=15, color="black"),
              axis.title.y = element_text(size=15, color="black"),
              panel.grid.major.y = element_line(color="lightgrey"),
              legend.key.size = unit(2, "cm"),
              legend.title = element_text(size = 18),
              legend.text = element_text(size = 14)) +
        guides(fill = guide_legend(title="Typ przejścia")) +
        scale_x_continuous(breaks=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23))
    }
    }})
  
}

app_ui <- navbarPage(
  title = "Co wie o nas Google?",
  tabPanel("Kiedy wyszukujemy?", ui_kuba),
  tabPanel("Jak wyszukujemy?", ui_maciek),
  tabPanel("Android", ui_rafal),
  theme = bs_theme(bootswatch = "lumen"),
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  © 2023 Copyright:
                  <a class='text-dark' href='https://github.com/pyzololo'>Rafał Pyzowski, </a>
                  <a class='text-dark' href='https://github.com/kubarrr'>Jakub Rymarski, </a>
                <a class='text-dark' href='https://github.com/MaciejSzpetmanski'>Maciej Szpetmański</a>
                </p>
                </footer>
                "),
)


shinyApp(ui = app_ui, server = server)

