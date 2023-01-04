library(rsconnect)
library(packrat)
library(tidyverse, quietly = T)
library(ggplot2)
library(ggstar)
library(shiny)
library(colourpicker)

t1 = data.frame(x=c(0.5,3,5.5), y=c(0.5,5.5,0.5))
t2 = data.frame(x=c(1,3,5), y=c(2.5,6.5,2.5))
t3 = data.frame(x=c(1.5,3,4.5), y=c(4.5,7.5,4.5))
t4 = data.frame(x=c(2,3,4), y=c(6.5,8.5,6.5))
t5 = data.frame(x=c(2.6,2.6,3.4,3.4), y=c(0,0.5,0.5,0))

p1 = data.frame(x=c(3,2.75,4.8), y=c(7.9,3.5,1.1))
p2 = data.frame(x=c(2.5,2), y=c(1.5,4.8))
p3 = data.frame(x=c(3.5,1.25,1.75), y=c(7,1,3))
p4 = data.frame(x=c(2.6,3.1,3.85), y=c(6.9,5.5,2))
p5 = data.frame(x=c(2.75,3.9), y=c(2.2,4))

ui = fluidPage(
  column(4,
      
      colourInput(
        "col6", NULL, "#D9B63CF7",
        allowTransparent = TRUE,
        closeOnClick = TRUE),
      colourInput(
        "background", NULL, "darkblue",
        allowTransparent = TRUE,
        closeOnClick = TRUE),
      colourInput(
        "col2", NULL, "#FFD900F5",
        allowTransparent = TRUE,
        closeOnClick = TRUE),
      colourInput(
        "col5", NULL, "#CCFF00F5",
        allowTransparent = TRUE,
        closeOnClick = TRUE),
      colourInput(
        "col3", NULL, "#0011FFC7",
        allowTransparent = TRUE,
        closeOnClick = TRUE),
      colourInput(
        "col4", NULL, "#7B00FFF7",
        allowTransparent = TRUE,
        closeOnClick = TRUE),
      colourInput(
        "col1", NULL, "#FF0026F7",
        allowTransparent = TRUE,
        closeOnClick = TRUE)),
  column(6,mainPanel(plotOutput("plot"))

    )
)
server = function(input, output) {
  output$plot <- renderPlot({
    ggplot(t1,aes(x=x, y=y)) +
      geom_polygon(fill="#107E32") +
      geom_polygon(data=t2, fill = "#107E32") +
      geom_polygon(data=t3, fill = "#107E32") +
      geom_polygon(data=t4, fill = "#107E32") +
      geom_polygon(data=t5, fill = "#584C36") +
      theme(aspect.ratio=5/2) +
      geom_star(aes(x=3,y=8.5, fill=TRUE), show.legend = FALSE,fill = input$col6,colour = input$col6, size = 15)+
      geom_point(data = p1,aes(x=x,y=y), show.legend = NA,colour = input$col1, size = 8)+
      geom_point(data = p2,aes(x=x,y=y), show.legend = NA,colour = input$col2, size = 8)+
      geom_point(data = p3,aes(x=x,y=y), show.legend = NA,colour = input$col3, size = 8)+
      geom_point(data = p4,aes(x=x,y=y), show.legend = NA,colour = input$col4, size = 8)+
      geom_point(data = p5,aes(x=x,y=y), show.legend = NA,colour = input$col5, size = 8)+
      theme_void() + theme(plot.background = element_rect(fill = input$background)) +
      scale_y_continuous(limits = c(0,9.5), expand = c(0, 0))
    
  })
}

shinyApp(ui = ui, server = server)
