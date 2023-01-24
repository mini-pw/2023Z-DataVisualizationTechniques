
library(shiny)
library(bslib)
source("ui_strony.R", local = T)


ui <- navbarPage(
  title = "Projekt JA",
  tabPanel("Szachowe rozgrywki", ui2),
  
  )
