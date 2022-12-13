library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(packrat)
library(rsconnect)
library(bslib)

source("ui8.R", local = TRUE)
ui <- navbarPage(
  title = "Ekonomiczne losy studentów",
  tabPanel("Nauka po studiach", ui2),
  tabPanel("Praca po studiach", ui3),
  theme = bs_theme(bootswatch = "flatly")
)