#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(packrat)
library(rsconnect)
library(bslib)
#devtools::install_github('rstudio/rsconnect')


source('ui.R', local = TRUE)
source('server.R', local = TRUE)




# Run the application 
shinyApp(ui, server)




