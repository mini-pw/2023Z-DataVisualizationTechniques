library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(shinycssloaders)

# options(semantic.themes = TRUE)
# options(shiny.custom.semantic = "www/")

source("server.R")

# HEADER
header <- dashboardHeader(
  includeCSS("./www/header.css"),
  class = "dsHeader",
  logo_path = "logo.png",
  logo_align = "center",
  title = "Chess ExploRer",
  right = div(
    id = "creator-container",
    textOutput("creator")
  )
)

# SIDEBAR
sidebar <- dashboardSidebar(
  size = "thin",
  color = "brown",
  sidebarMenu(
    includeCSS("./www/sidebar.css"),
    menuItem(text = "Home",
      tabName = "home",
      icon = icon("home")),
    menuItem(text = "Competition",
      tabName = "comp",
      icon = icon("trophy")),
    menuItem(text = "Map",
      tabName = "map",
      icon = icon("map")),
    menuItem(text = "Games",
      tabName = "games",
      icon = icon("chess board",
        lib = "font-awesome")),
    menuItem(text = "Reporitory",
      href = "https://github.com/KacWNK/TWD-ChessProject",
      icon = icon("github"))
  )
)

###### HOME TAB ###### TODO

### FAQ
faq <- list(
  list(
    title = "Why was this project created?",
    content = p(class = "faq-content",
      "There once was a assigment worth 1/4 of your grade so we decided to make awesome"
    )
  ),
  list(
    title = "Where did we get the data?",
    content = p("Data was collected or scarped from www.chess.com and www.kaggle.com")
  ),
  list(
    title = "What was our inspiration?",
    content = p("To create a progress tracker for our chess journey")
  ),
  list(
    title = "How long it took?",
    content = p("About 30 work hours spend on research and app development")
  )
)

homeTab <- semanticPage(
  title = "Start page",
  div(
    class = "ui grid full-span",
    div(class = "row h-70",
      div(class = "four wide column",
        div(class = "ui statistics",
          div(class = "statistic",
            div(class = "value text-orange", 2),
            div(class = "label text-white", "Player game")
          ),
          div(class = "statistic",
            div(class = "value text-orange", 20),
            div(class = "label text-white", "Starting moves")
          ),
          div(class = "statistic",
            div(class = "value text-orange", 32),
            div(class = "label text-white", "Chess pieces")
          ),
          div(class = "statistic",
            div(class = "value text-orange", "10 Milion Milion"),
            div(class = "label text-white", "Games Played")
          ),
          div(class = "statistic",
            div(class = "value text-orange", span(
              "Around 10", tags$sup("111")
            )),
            div(class = "label text-white", "Possible Chess Positions")
          )
        )
      ),
      div(class = "twelve wide column center",
        shinycssloaders::withSpinner(
          plotlyOutput(outputId = "mapFIDE"),
          type = 4,
          color = "#f9a03f"
        )
      ),
    ),
    div(class = "row h-30",
      div(class = "faq",
        accordion(faq,
          fluid = TRUE,
          styled = FALSE
        )
      )
    )
  )
)


###### MAP TAB ######
mapTab <- semanticPage(
  title = "Map",
  div(class = "ui grid full-span",
    div(class = "row",
      multiple_radio(
        "fill_var", "Select type: ",
        choices = c("Win Ratio", "Average Accuracy"),
        choices_value = c("WinP", "Accuracy"),
        position = "inline"
      )
    ),
    div(class = "two column row",
      div(
        class = "column",
        shinycssloaders::withSpinner(
          plotlyOutput(outputId = "mapKacper"),
          type = 4,
          color = "#f9a03f"
        )
      ),
      div(class = "column",
        shinycssloaders::withSpinner(
          plotlyOutput(outputId = "mapKrzysiek"),
          type = 4,
          color = "#f9a03f"
        ),
      )
    )
  )
)

###### GAMES TAB #######
gamesTab <- semanticPage(
  tilte = "Games",
  div(class = "ui grid",
      div(class = "row",
      selectInput(
        "gif", "Select a gif:",
        choices = c(
          "Best game- Kacper(white)" = "./resources/KW_immortalGame.gif",
          "First game- Kacper(white)" = "./resources/KacperPierwszaPartia.gif",
          "Best game- Krzysiek(white)" = "./resources/Kristof_Immortal.gif",
          "First game- Krzysiek(white)" = "./resources/Krzysiu_pierwszaPartia.gif"
        ))
      ),
      div(class = "row",
        shinycssloaders::withSpinner(
          imageOutput("selected_gif"),
          type = 4,
          color = "#f9a03f"
        )
      )
  )
)

###### COMPETITION TAB #######
panel_style <- "20px; border: solid 2px #dfdede; border-radius: 10px;"
### MAIN PLOT 2 - MOVE QUALITY
compTabRow2 <- div(
  class = "row clear-bg",
  div(class = "ten wide column",
    sidebar_layout(
      sidebar_panel(
        multiple_radio(
          "colorMoveQuality",
          "Choose color of pieces",
          selected = unique(dfMoveQuality$Color)[1],
          choices = c("White", "Black", "White and black"),
          choices_value = unique(dfMoveQuality$Color)
        ),
        width = 2
      ),
      main_panel(
        shinycssloaders::withSpinner(
          plotlyOutput("moveQualityPlot"),
          type = 4,
          color = "#f9a03f"
        ),
        width = 10
      ),
      container_style = panel_style
    )
  ),
  div(class = "four wide column",
    div(id = "winRate",
      style = panel_style,
      shinycssloaders::withSpinner(
        plotlyOutput("winRatePlot"),
        type = 4,
        color = "#f9a03f"
      )
    )
  )
)

### MAIN PLOT 3 - ELO
compTabRow3 <- div(
  class = "row clear-bg",
  sidebar_layout(
    sidebar_panel(
      uiOutput("timeLagElo"),
      width = 2
    ),
    main_panel(
      shinycssloaders::withSpinner(
        plotOutput("eloPlot"),
        type = 4,
        color = "#f9a03f"
      ),
      width = 10
    ),
    container_style = panel_style
  )
)

### MAIN PLOTS COMBINED
compTab <- semanticPage(
  title = "Competition page",
  div(class = "ui grid",
    div(class = "row",
      div(class = "four wide column",
        multiple_radio(
          "playerComp",
          "Select a player:",
          selected = unique(dfMoveQuality$Player)[1],
          choices = unique(dfMoveQuality$Player),
          position = "inline"
        ),
      ),
      div(class = "four wide column",
        multiple_radio(
          "timeControlComp",
          "Choose time control:",
          selected = unique(dfMoveQuality$Type)[1],
          choices = c("Bullet", "Blitz", "Rapid"),
          choices_value = unique(dfMoveQuality$Type),
          position = "inline"
        )
      )
    ),
    compTabRow3,
    compTabRow2,
    div(class = "row",
      style = panel_style,
      plotOutput("densPlot")
    )
  )
)

# BODY
body <- dashboardBody(class = "dsBody", tabItems(
  includeCSS("./www/body.css"),
  tabItem(tabName = "home", homeTab),
  tabItem(tabName = "comp", compTab),
  tabItem(tabName = "map", mapTab),
  tabItem(tabName = "games", gamesTab)
))

# RUN APP
shinyUI(dashboardPage(
  title = "ChessExploRer",
  header, sidebar, body,
  theme = "slate",
  class = "dsBodyOuter",
  sidebar_and_body_container_class = "dsPage"
))
