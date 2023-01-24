library(shiny)
library(bslib)
library(wordcloud2)
library(dplyr)

source("wykres_jezyki.R", encoding = "UTF-8")
source("wykres_wordcloud.R", encoding = "UTF-8")
source("wykresy-wiktoria.R", encoding = "UTF-8")
source("wykresy_statystyki.R", encoding = "UTF-8")

df_lang_list <- read_our_data()

ac <- read.csv("ac.csv", encoding = "UTF-8")
mk <- read.csv("mk.csv", encoding = "UTF-8")
wk <- read.csv("wk.csv", encoding = "UTF-8")

cat_ac <- read.csv("category_ac.csv", encoding = "UTF-8")
cat_mk <- read.csv("category_mk.csv", encoding = "UTF-8")
cat_wk <- read.csv("category_wk.csv", encoding = "UTF-8")

custom_theme <- bs_theme(
  bg = "#100c0c",
  fg = "white",
  primary = "#fb0404",
  secondary = "#FF374B",
  base_font = font_google("Roboto"),
  code_font = font_google("Roboto")
)

ui_activity <- fluidPage(
  theme = custom_theme,
  
  titlePanel("W jakich godzinach oglądamy filmy na YT"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      radioButtons(
        "person_heatMap",
        label = "Która osoba Cię interesuje?",
        choices = c("Alicja", "Martyna", "Wiktoria"),
        selected = "Alicja"
      ),
      dateRangeInput(
        "date_heatMap",
        "Zakres czasu:",
        start = "2015-01-01",
        end = "2022-12-31",
        language = "pl"
      ),
      checkboxInput("isClock", "Zegary")
    ),
    mainPanel(
      width = 10,
      plotOutput("activity") %>%
        shinycssloaders::withSpinner(
          type = 3,
          color = "#727372",
          color.background = "#100c0c"
        )
    )
  )
  
)

ui_word <- fluidPage(
  tags$head(tags$style(HTML(
    'div#wcLabel {display: none;}'
  ))),
  
  theme = custom_theme,
  
  titlePanel("Wordcloud"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      radioButtons(
        "person_word",
        label = "Która osoba Cię interesuje?",
        choices = c("Alicja", "Martyna", "Wiktoria")
      ),
      sliderInput(
        "freq",
        "Minimalna liczba obejrzanych filmów danego twórcy:",
        min = 0,
        max = 150,
        value = 75
      ),
      dateRangeInput(
        "date_word",
        "Zakres czasu:",
        start = "2015-01-01",
        end = "2022-12-31",
        language = "pl"
      ),
      
    ),
    mainPanel(
      width = 10,
      wordcloud2Output("wordPlot") %>%
        shinycssloaders::withSpinner(
          type = 3,
          color = "#727372",
          color.background = "#100c0c"
        )
    )
  )
)

ui_lang <- fluidPage(
  theme = custom_theme,
  
  titlePanel("Stosunek języków, w jakich oglądamy filmy na YT"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      checkboxGroupInput(
        "person_lang",
        label = "Które osoby Cię interesują?",
        choices = c("Alicja", "Martyna", "Wiktoria"),
        selected = c("Alicja", "Martyna", "Wiktoria")
      ),
      br(),
      dateRangeInput(
        "date_lang",
        label = "Zakres czasu:",
        start = "2015-01-01",
        end = "2022-12-31",
        language = "pl"
      )
    ),
    mainPanel(
      width = 10,
      fluidRow(
        plotlyOutput("langPlot") %>%
          shinycssloaders::withSpinner(
            type = 3,
            color = "#727372",
            color.background = "#100c0c"
          )
      ),
      fluidRow(br()),
      fluidRow(
        titlePanel("TOP 5 twórców w języku angielskim"),
        column(width = 5,
               titlePanel("Alicja"),
               verbatimTextOutput("top5AlicjaEN")),
        tags$head(tags$style(HTML(
          "
                            #top5AlicjaEN {
                              font-size: 20px;
                            }
                            "
        ))),
        column(width = 5,
               titlePanel("Martyna"),
               verbatimTextOutput("top5MartynaEN")),
        tags$head(tags$style(HTML(
          "
                            #top5MartynaEN {
                              font-size: 20px;
                            }
                            "
        ))),
        column(width = 5,
               titlePanel("Wiktoria"),
               verbatimTextOutput("top5WiktoriaEN")),
        tags$head(tags$style(HTML(
          "
                            #top5WiktoriaEN {
                              font-size: 20px;
                            }
                            "
        ))),
      ),
      fluidRow(br()),
      fluidRow(
        titlePanel("TOP 5 twórców w języku polskim"),
        column(
          width = 5,
          titlePanel("Alicja"),
          verbatimTextOutput("top5AlicjaPL")
        ),
        tags$head(tags$style(HTML(
          "
                            #top5AlicjaPL {
                              font-size: 20px;
                            }
                            "
        ))),
        column(
          width = 5,
          titlePanel("Martyna"),
          verbatimTextOutput("top5MartynaPL")
        ),
        tags$head(tags$style(HTML(
          "
                            #top5MartynaPL {
                              font-size: 20px;
                            }
                            "
        ))),
        column(
          width = 5,
          titlePanel("Wiktoria"),
          verbatimTextOutput("top5WiktoriaPL")
        ),
        tags$head(tags$style(HTML(
          "
                            #top5WiktoriaPL {
                              font-size: 20px;
                            }
                            "
        ))),
      )
    )
  )
)

ui_stat <- fluidPage(theme = custom_theme,
                     
                     titlePanel("Podsumowanie"),
                     
                     sidebarLayout(
                       sidebarPanel(
                         width = 2,
                         radioButtons(
                           "person_stat",
                           label = "Która osoba Cię interesuje?",
                           choices = c("Alicja", "Martyna", "Wiktoria")
                         )
                       ),
                       mainPanel(
                         width = 10,
                         fluidRow(
                           plotlyOutput("catPlot") %>%
                             shinycssloaders::withSpinner(
                               type = 3,
                               color = "#727372",
                               color.background = "#100c0c"
                             )
                         ),
                         fluidRow(br()),
                         fluidRow(br()),
                         fluidRow(
                           column(
                             width = 6,
                             titlePanel("Liczba obejrzanych wszystkich filmów:"),
                             verbatimTextOutput("texta")
                           ),
                           tags$head(tags$style(HTML(
                             "
                            #texta {
                              font-size: 20px;
                            }
                            "
                           ))),
                           column(
                             width = 6,
                             titlePanel("Liczba obejrzanych unikalnych filmów:"),
                             verbatimTextOutput("textb")
                           ),
                           tags$head(tags$style(HTML(
                             "
                            #textb {
                              font-size: 20px;
                            }
                            "
                           )))
                         ),
                         fluidRow(
                           titlePanel("Tytuł pierwszego obejrzanego filmu:"),
                           column(width = 12,
                                  verbatimTextOutput("textc")),
                           tags$head(tags$style(HTML(
                             "
                            #textc {
                              font-size: 20px;
                            }
                            "
                           )))
                         ),
                         fluidRow(
                           column(
                             width = 8,
                             titlePanel("Najczęściej oglądany film:"),
                             verbatimTextOutput("textd")
                           ),
                           tags$head(tags$style(HTML(
                             "
                            #textd {
                              font-size:20px

                            }
                            "
                           ))),
                           column(
                             width = 4,
                             titlePanel("Liczba jego obejrzeń:"),
                             verbatimTextOutput("texte")
                           ),
                           tags$head(tags$style(HTML(
                             "
                            #texte {
                              font-size:20px

                            }
                            "
                           )))
                         )
                       )
                     ))


server <- function(input, output) {
  output$catPlot <- renderPlotly({
    df <- switch(
      input$person_stat,
      Alicja = cat_ac,
      Martyna = cat_mk,
      Wiktoria = cat_wk
    )
    plot_cat(df)
  }) %>%
    bindCache(input$person_stat)
  
  output$texta <- renderText({
    df <- switch(
      input$person_stat,
      Alicja = ac,
      Martyna = mk,
      Wiktoria = wk
    )
    nrow(df)
  }) %>%
    bindCache(input$person_stat)
  
  output$textb <- renderText({
    df <- switch(
      input$person_stat,
      Alicja = ac,
      Martyna = mk,
      Wiktoria = wk
    )
    length(unique(df$titleId))
  }) %>%
    bindCache(input$person_stat)
  
  output$textc <- renderText({
    df <- switch(
      input$person_stat,
      Alicja = ac,
      Martyna = mk,
      Wiktoria = wk
    )
    df %>% filter(channelName != "") %>%
      slice(n()) %>% select(title) %>% toString()
  }) %>%
    bindCache(input$person_stat)
  
  output$textd <- renderText({
    df <- switch(
      input$person_stat,
      Alicja = ac,
      Martyna = mk,
      Wiktoria = wk
    )
    df %>%
      filter(title != "ilm, który został usunięty") %>%
      group_by(title) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      top_n(1) -> df
    
    df %>% select("title") %>% toString()
    
  }) %>%
    bindCache(input$person_stat)
  
  output$texte <- renderText({
    df <- switch(
      input$person_stat,
      Alicja = ac,
      Martyna = mk,
      Wiktoria = wk
    )
    
    df %>%
      filter(title != "ilm, który został usunięty") %>%
      group_by(title) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      top_n(1) -> df
    
    df %>% select("count") %>% toString()
  }) %>%
    bindCache(input$person_stat)
  
  output$wordPlot <- renderWordcloud2({
    df <- switch(
      input$person_word,
      Alicja = ac,
      Martyna = mk,
      Wiktoria = wk
    )
    plot_word(df, input$date_word[1], input$date_word[2], input$freq)
  }) %>%
    bindCache(input$person_word, input$date_word, input$freq)
  
  output$langPlot <- renderPlotly({
    start <- input$date_lang[1]
    end <- input$date_lang[2]
    
    df_a <- df_lang_list$a
    df_m <- df_lang_list$m
    df_w <- df_lang_list$w
    
    
    plot_lang(input$person_lang, df_a, df_m, df_w, start, end)
  }) %>%
    bindCache(input$date_lang, input$person_lang)
  
  output$top5AlicjaPL <- renderPrint({
    df <-
      chooseTOP5(ac,
                 df_lang_list$a,
                 FALSE,
                 input$date_lang[1],
                 input$date_lang[2])
    TOP5toString(df)
  }) %>%
    bindCache(input$date_lang, input$person_lang)
  
  output$top5MartynaPL <- renderPrint({
    df <-
      chooseTOP5(mk,
                 df_lang_list$m,
                 FALSE,
                 input$date_lang[1],
                 input$date_lang[2])
    TOP5toString(df)
  }) %>%
    bindCache(input$date_lang, input$person_lang)
  
  output$top5WiktoriaPL <- renderPrint({
    df <-
      chooseTOP5(wk,
                 df_lang_list$w,
                 FALSE,
                 input$date_lang[1],
                 input$date_lang[2])
    TOP5toString(df)
  }) %>%
    bindCache(input$date_lang, input$person_lang)
  
  output$top5AlicjaEN <- renderPrint({
    df <-
      chooseTOP5(ac,
                 df_lang_list$a,
                 TRUE,
                 input$date_lang[1],
                 input$date_lang[2])
    TOP5toString(df)
  }) %>%
    bindCache(input$date_lang, input$person_lang)
  
  output$top5MartynaEN <- renderPrint({
    df <-
      chooseTOP5(mk,
                 df_lang_list$m,
                 TRUE,
                 input$date_lang[1],
                 input$date_lang[2])
    TOP5toString(df)
  }) %>%
    bindCache(input$date_lang, input$person_lang)
  
  output$top5WiktoriaEN <- renderPrint({
    df <-
      chooseTOP5(wk,
                 df_lang_list$w,
                 TRUE,
                 input$date_lang[1],
                 input$date_lang[2])
    TOP5toString(df)
  }) %>%
    bindCache(input$date_lang, input$person_lang)
  
  output$activity <- renderPlot({
    df <- switch(
      input$person_heatMap,
      Alicja = ac,
      Martyna = mk,
      Wiktoria = wk
    )
    if (input$isClock) {
      clocks.plot(df,
                  as.POSIXlt(input$date_heatMap[1]),
                  as.POSIXlt(input$date_heatMap[2]))
    } else {
      activity.heatmap(df,
                       as.POSIXlt(input$date_heatMap[1]),
                       as.POSIXlt(input$date_heatMap[2]))
    }
  })
  
  
}

app_ui <- navbarPage(
  tags$style(".fa-youtube {color:#fb0404}"),
  title = div(icon("youtube"), "MyYoutube"),
  tabPanel("Podsumowanie", ui_stat, icon = icon("chart-bar")),
  tabPanel("Aktywność", ui_activity, icon = icon("calendar")),
  tabPanel("Wordcloud", ui_word, icon = icon("cloud")),
  tabPanel("Języki", ui_lang, icon = icon("book")),
  theme = custom_theme
)


shinyApp(ui = app_ui, server = server)
