library(shinycssloaders)
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(plotly)
library(vistime)
library(lubridate)
library(stringr)
library(packrat)
library(rsconnect)
library(bslib)
library(shinyWidgets)

source("ui8.R", local = TRUE)

ui <- navbarPage(
  title = "Projekt JA",
  tabPanel("O projekcie", ui0),
  tabPanel("Uśredniony dzień", ui1),
  tabPanel("Top aplikacje", ui2),
  tabPanel("Przeglądarka dni", ui3),
  tabPanel("Słuchanie muzyki", ui4),
  theme = bs_theme(bootswatch = "flatly")
)
