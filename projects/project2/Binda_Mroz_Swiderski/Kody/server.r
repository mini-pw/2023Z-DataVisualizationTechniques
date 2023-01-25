library(DT)
library(supercaliheatmapwidget)
library(hash)

function(input, output, session) {
  
  topCreatorsP <- reactive({
    if (input$rankingCriteria == "byWatchtime"){
      if (input$subscriberCountFilter == "all"){
        topCreatorsP <- DT::datatable(head(watch_time_per_channelP[,c(1,2,3)],10), 
                                      selection = 'none',
                                      options = list(
                                        dom = 't',
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'color': 'red'});",
                                          "}")
                                      ),
                                      rownames = FALSE
                                      
        ) %>% formatStyle(columns = c("channel","subscribers","watchtime"), color = "white")
      }
      else if (input$subscriberCountFilter == "underMilion"){
        topCreatorsP <- DT::datatable(head(filter(watch_time_per_channelP,watch_time_per_channelP$subscribers < 1000000)[,c(1,2,3)],10), 
                                      selection = 'none',
                                      options = list(
                                        dom = 't',
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'color': 'red'});",
                                          "}")
                                      ), rownames = FALSE
        ) %>% formatStyle(columns = c("channel","subscribers","watchtime"), color = "white")
      }
      else {
        topCreatorsP <- DT::datatable(head(filter(watch_time_per_channelP,watch_time_per_channelP$subscribers < 100000)[,c(1,2,3)],10), 
                                      selection = 'none',
                                      options = list(
                                        dom = 't',
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'color': 'red'});",
                                          "}")
                                      ), rownames = FALSE
        ) %>% formatStyle(columns = c("channel","subscribers","watchtime"), color = "white")
      }
    }
    else {
      if (input$subscriberCountFilter == "all"){
        topCreatorsP <- DT::datatable(head(videos_per_channelP[,c(1,2,3)],10), 
                                      selection = 'none',
                                      options = list(
                                        dom = 't',
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'color': 'red'});",
                                          "}")
                                      ), rownames = FALSE
        ) %>% formatStyle(columns = c("channel","subscribers","videos watched"), color = "white")
      }
      else if (input$subscriberCountFilter == "underMilion"){
        topCreatorsP <- DT::datatable(head(filter(videos_per_channelP,videos_per_channelP$subscribers < 1000000)[,c(1,2,3)],10), 
                                      selection = 'none',                 
                                      options = list(
                                        dom = 't',
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'color': 'red'});",
                                          "}")
                                      ), rownames = FALSE
        ) %>% formatStyle(columns = c("channel","subscribers","videos watched"), color = "white")
      }
      else {
        topCreatorsP <- DT::datatable(head(filter(videos_per_channelP,videos_per_channelP$subscribers < 100000)[,c(1,2,3)],10), 
                                      selection = 'none',                
                                      options = list(
                                        dom = 't',
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'color': 'red'});",
                                          "}")
                                      ), rownames = FALSE
        ) %>% formatStyle(columns = c("channel","subscribers","videos watched"), color = "white")
      }
    }
    return (topCreatorsP)
  })
  
  topCreatorsM <- reactive({
    if (input$rankingCriteria == "byWatchtime"){
      if (input$subscriberCountFilter == "all"){
        topCreatorsM <- DT::datatable(head(watch_time_per_channelM[,c(1,2,3)],10), 
                                      selection = 'none',
                                      options = list(
                                        dom = 't',
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'color': 'red'});",
                                          "}")
                                      ), rownames = FALSE
        ) %>% formatStyle(columns = c("channel","subscribers","watchtime"), color = "white")
      }
      else if (input$subscriberCountFilter == "underMilion"){
        topCreatorsM <- DT::datatable(head(filter(watch_time_per_channelM,watch_time_per_channelM$subscribers < 1000000)[,c(1,2,3)],10), 
                                      selection = 'none',               
                                      options = list(
                                        dom = 't',
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'color': 'red'});",
                                          "}")
                                      ), rownames = FALSE
        ) %>% formatStyle(columns = c("channel","subscribers","watchtime"), color = "white")
      }
      else {
        topCreatorsM <- DT::datatable(head(filter(watch_time_per_channelM,watch_time_per_channelM$subscribers < 100000)[,c(1,2,3)],10), 
                                      selection = 'none',                
                                      options = list(
                                        dom = 't',
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'color': 'red'});",
                                          "}")
                                      ), rownames = FALSE
        ) %>% formatStyle(columns = c("channel","subscribers","watchtime"), color = "white")
      }
    }
    else {
      if (input$subscriberCountFilter == "all"){
        topCreatorsM <- DT::datatable(head(videos_per_channelM[,c(1,2,3)],10), 
                                      selection = 'none',
                                      options = list(
                                        dom = 't',
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'color': 'red'});",
                                          "}")
                                      ), rownames = FALSE
        ) %>% formatStyle(columns = c("channel","subscribers","videos watched"), color = "white")
      }
      else if (input$subscriberCountFilter == "underMilion"){
        topCreatorsM <- DT::datatable(head(filter(videos_per_channelM,videos_per_channelM$subscribers < 1000000)[,c(1,2,3)],10), 
                                      selection = 'none',              
                                      options = list(
                                        dom = 't',
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'color': 'red'});",
                                          "}")
                                      ), rownames = FALSE
        ) %>% formatStyle(columns = c("channel","subscribers","videos watched"), color = "white")
      }
      else {
        topCreatorsM <- DT::datatable(head(filter(videos_per_channelM,videos_per_channelM$subscribers < 100000)[,c(1,2,3)],10), 
                                      selection = 'none',              
                                      options = list(
                                        dom = 't',
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'color': 'red'});",
                                          "}")
                                      ), rownames = FALSE
        ) %>% formatStyle(columns = c("channel","subscribers","videos watched"), color = "white")
      }
    }
    return (topCreatorsM)
  })
  
  topCreatorsB <- reactive({
    if (input$rankingCriteria == "byWatchtime"){
      if (input$subscriberCountFilter == "all"){
        topCreatorsB <- DT::datatable(head(watch_time_per_channelB[,c(1,2,3)],10), 
                                      selection = 'none',
                                      options = list(
                                        dom = 't',
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'color': 'red'});",
                                          "}")
                                      ), rownames = FALSE
        ) %>% formatStyle(columns = c("channel","subscribers","watchtime"), color = "white")
      }
      else if (input$subscriberCountFilter == "underMilion"){
        topCreatorsB <- DT::datatable(head(filter(watch_time_per_channelB,watch_time_per_channelB$subscribers < 1000000)[,c(1,2,3)],10), 
                                      selection = 'none',               
                                      options = list(
                                        dom = 't',
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'color': 'red'});",
                                          "}")
                                      ), rownames = FALSE
        ) %>% formatStyle(columns = c("channel","subscribers","watchtime"), color = "white")
      }
      else {
        topCreatorsB <- DT::datatable(head(filter(watch_time_per_channelB,watch_time_per_channelB$subscribers < 100000)[,c(1,2,3)],10), 
                                      selection = 'none',               
                                      options = list(
                                        dom = 't',
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'color': 'red'});",
                                          "}")
                                      ), rownames = FALSE
        ) %>% formatStyle(columns = c("channel","subscribers","watchtime"), color = "white")
      }
    }
    else {
      if (input$subscriberCountFilter == "all"){
        topCreatorsB <- DT::datatable(head(videos_per_channelB[,c(1,2,3)],10), 
                                      selection = 'none',
                                      options = list(
                                        dom = 't',
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'color': 'red'});",
                                          "}")
                                      ), rownames = FALSE
        ) %>% formatStyle(columns = c("channel","subscribers","videos watched"), color = "white")
      }
      else if (input$subscriberCountFilter == "underMilion"){
        topCreatorsB <- DT::datatable(head(filter(videos_per_channelB,videos_per_channelB$subscribers < 1000000)[,c(1,2,3)],10), 
                                      selection = 'none',               
                                      options = list(
                                        dom = 't',
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'color': 'red'});",
                                          "}")
                                      ), rownames = FALSE
        ) %>% formatStyle(columns = c("channel","subscribers","videos watched"), color = "white")
      }
      else {
        topCreatorsB <- DT::datatable(head(filter(videos_per_channelB,videos_per_channelB$subscribers < 100000)[,c(1,2,3)],10), 
                                      selection = 'none',                
                                      options = list(
                                        dom = 't',
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'color': 'red'});",
                                          "}")
                                      ), rownames = FALSE
        ) %>% formatStyle(columns = c("channel","subscribers","videos watched"), color = "white")
      }
    }
    return (topCreatorsB)
  })
  
    
  output$topCreatorsP = DT::renderDataTable(topCreatorsP())
  output$topCreatorsM = DT::renderDataTable(topCreatorsM())
  output$topCreatorsB = DT::renderDataTable(topCreatorsB())
  
  
  hourPlot <- reactive({
    if("pawel" %in% input$hourPlotFilter){
      if("mikolaj" %in% input$hourPlotFilter){
        if("michal" %in% input$hourPlotFilter){
          hourPlot <- hourPlotAll
        } else {
        hourPlot <- hourPlotPM
        }
      }
      else if("michal" %in% input$hourPlotFilter){
        hourPlot <- hourPlotPB
      } else {
        hourPlot <- hourPlotP
      }
    }
    else if("mikolaj" %in% input$hourPlotFilter){
      if("michal" %in% input$hourPlotFilter){
        hourPlot <- hourPlotMB
      } else {
        hourPlot <- hourPlotM
      }
    }
    else{
      hourPlot <- hourPlotB
    }
   
    return (hourPlot)
  })
  
  output$hourPlot = renderPlot(hourPlot(), bg="transparent")
  
  output$weekdayPlot = renderPlot({weekdayPlot}, bg="transparent")
  
  PutChartOnTop <- function(This, ChartList) {
    c(This, ChartList[ChartList != This])
  }
  
  ChartOrder1 <- reactiveVal(list("streamedCalendarP1","streamedCalendarM1","streamedCalendarB1"))
  
  output$ListOfCharts1 <- renderUI({
    Order <- ChartOrder1()
    
    ui <- supercaliheatmapwidgetOutput(Order[1], height = "210px")
    class(ui) <- c("shiny.tag.list", "list")
    return(ui)
  })
  
  observeEvent(
    input$whoseCalendar,
    {
      if (input$whoseCalendar == "dfP") {
        ChartOrder1(PutChartOnTop("streamedCalendarP1", ChartOrder1())) # add plot on top
      } else {
        ChartOrder1(ChartOrder1()[ChartOrder1() != "streamedCalendarP1"]) # filter out plot 3
      }
    })
  
  observeEvent(
    input$whoseCalendar,
    {
      if (input$whoseCalendar == "dfM") {
        ChartOrder1(PutChartOnTop("streamedCalendarM1", ChartOrder1())) # add plot on top
      } else {
        ChartOrder1(ChartOrder1()[ChartOrder1() != "streamedCalendarM1"]) # filter out plot 3
      }
    })
  
  observeEvent(
    input$whoseCalendar,
    {
      if (input$whoseCalendar == "dfB") {
        ChartOrder1(PutChartOnTop("streamedCalendarB1", ChartOrder1())) # add plot on top
      } else {
        ChartOrder1(ChartOrder1()[ChartOrder1() != "streamedCalendarB1"]) # filter out plot 3
      }
    })
  
  ChartOrder2 <- reactiveVal(list("streamedCalendarP2","streamedCalendarM2","streamedCalendarB2"))
  
  output$ListOfCharts2 <- renderUI({
    Order <- ChartOrder2()
    
    ui <- supercaliheatmapwidgetOutput(Order[1], height = "210px")
    class(ui) <- c("shiny.tag.list", "list")
    return(ui)
  })
  
  observeEvent(
    input$whoseCalendar,
    {
      if (input$whoseCalendar == "dfP") {
        ChartOrder2(PutChartOnTop("streamedCalendarP2", ChartOrder2())) # add plot on top
      } else {
        ChartOrder2(ChartOrder2()[ChartOrder2() != "streamedCalendarP2"]) # filter out plot 3
      }
    })
  
  observeEvent(
    input$whoseCalendar,
    {
      if (input$whoseCalendar == "dfM") {
        ChartOrder2(PutChartOnTop("streamedCalendarM2", ChartOrder2())) # add plot on top
      } else {
        ChartOrder2(ChartOrder2()[ChartOrder2() != "streamedCalendarM2"]) # filter out plot 3
      }
    })
  
  observeEvent(
    input$whoseCalendar,
    {
      if (input$whoseCalendar == "dfB") {
        ChartOrder2(PutChartOnTop("streamedCalendarB2", ChartOrder2())) # add plot on top
      } else {
        ChartOrder2(ChartOrder2()[ChartOrder2() != "streamedCalendarB2"]) # filter out plot 3
      }
    })
  
  ChartOrder3 <- reactiveVal(list("streamedCalendarP3","streamedCalendarM3","streamedCalendarB3"))
  
  output$ListOfCharts3 <- renderUI({
    Order <- ChartOrder3()

    
    ui <- supercaliheatmapwidgetOutput(Order[1], height = "210px")
    class(ui) <- c("shiny.tag.list", "list")
    return(ui)
  })
  
  observeEvent(
    input$whoseCalendar,
    {
      if (input$whoseCalendar == "dfP") {
        ChartOrder3(PutChartOnTop("streamedCalendarP3", ChartOrder3())) # add plot on top
      } else {
        ChartOrder3(ChartOrder3()[ChartOrder3() != "streamedCalendarP3"]) # filter out plot 3
      }
    })
  
  observeEvent(
    input$whoseCalendar,
    {
      if (input$whoseCalendar == "dfM") {
        ChartOrder3(PutChartOnTop("streamedCalendarM3", ChartOrder3())) # add plot on top
      } else {
        ChartOrder3(ChartOrder3()[ChartOrder3() != "streamedCalendarM3"]) # filter out plot 3
      }
    })
  
  observeEvent(
    input$whoseCalendar,
    {
      if (input$whoseCalendar == "dfB") {
        ChartOrder3(PutChartOnTop("streamedCalendarB3", ChartOrder3())) # add plot on top
      } else {
        ChartOrder3(ChartOrder3()[ChartOrder3() != "streamedCalendarB3"]) # filter out plot 3
      }
    })
  
  
  
  
  output$streamedCalendarP1 <- renderSupercaliheatmapwidget({
    supercal(
      calendarP, 
      datetime_col = date, value_col = n,
      label = cal_label("top", "center", height = 20),
      cell_size = 20,
      cell_padding = 2, 
      tooltip_item_name = cal_names("watched video"),
      col_limit = 3,
      #row_limit = 3,
      range=4,
      start='2022-01-01',
      orientation='vertical',
      height="100%"
    ) %>% 
      cal_legend(show=FALSE, colors = cal_colors(min = "FFCCCC", max = "#ff0000"))
    
  })
  
  output$streamedCalendarP2 <- renderSupercaliheatmapwidget({
    supercal(
      calendarP,
      datetime_col = date, value_col = n,
      label = cal_label("top", "center", height = 20),
      cell_size = 20,
      cell_padding = 2,
      tooltip_item_name = cal_names("watched video"),
      col_limit = 1,
      range=4,
      start='2022-05-01',
      orientation='vertical',
      height="100%"
    )%>%
      cal_legend(show=FALSE, colors = cal_colors(min = "FFCCCC", max = "#FF0000"))
  })
  
  output$streamedCalendarP3 <- renderSupercaliheatmapwidget({
    supercal(
      calendarP,
      datetime_col = date, value_col = n,
      label = cal_label("top", "center", height = 20),
      cell_size = 20,
      cell_padding = 2,
      tooltip_item_name = cal_names("watched video"),
      col_limit = 1,
      range=4,
      start='2022-09-01',
      orientation='vertical',
      height="100%"
    )%>%
      cal_legend(show=FALSE, colors = cal_colors(min = "FFCCCC", max = "#FF0000"))
  })
  
  output$streamedCalendarM1 <- renderSupercaliheatmapwidget({
    supercal(
      calendarM, 
      datetime_col = date, value_col = n,
      label = cal_label("top", "center", height = 20),
      cell_size = 20,
      cell_padding = 2, 
      tooltip_item_name = cal_names("watched video"),
      col_limit = 3,
      range=4,
      orientation='vertical',
      height="100%"
    ) %>% 
      cal_legend(show=FALSE, colors = cal_colors(min = "FFCCCC", max = "#FF0000"))
    
  })
  
  output$streamedCalendarM2 <- renderSupercaliheatmapwidget({
    supercal(
      calendarM,
      datetime_col = date, value_col = n,
      label = cal_label("top", "center", height = 20),
      cell_size = 20,
      cell_padding = 2,
      tooltip_item_name = cal_names("watched video"),
      col_limit = 1,
      range=4,
      start='2022-05-01',
      orientation='vertical',
      height="100%"
    )%>%
      cal_legend(show=FALSE, colors = cal_colors(min = "FFCCCC", max = "#FF0000"))
  })
  
  output$streamedCalendarM3 <- renderSupercaliheatmapwidget({
    supercal(
      calendarM,
      datetime_col = date, value_col = n,
      label = cal_label("top", "center", height = 20),
      cell_size = 20,
      cell_padding = 2,
      tooltip_item_name = cal_names("watched video"),
      col_limit = 1,
      range=4,
      start='2022-09-01',
      orientation='vertical',
      height="100%"
    )%>%
      cal_legend(show=FALSE, colors = cal_colors(min = "FFCCCC", max = "#FF0000"))
  })
  output$streamedCalendarB1 <- renderSupercaliheatmapwidget({
    supercal(
      calendarB, 
      datetime_col = date, value_col = n,
      label = cal_label("top", "center", height = 20),
      cell_size = 20,
      cell_padding = 2, 
      tooltip_item_name = cal_names("watched video"),
      col_limit = 3,
      range=4,
      start='2022-01-01',
      orientation='vertical',
      height="100%"
    ) %>% 
      cal_legend(show=FALSE, colors = cal_colors(min = "FFCCCC", max = "#FF0000"))
    
  })
  
  output$streamedCalendarB2 <- renderSupercaliheatmapwidget({
    supercal(
      calendarB,
      datetime_col = date, value_col = n,
      label = cal_label("top", "center", height = 20),
      cell_size = 20,
      cell_padding = 2,
      tooltip_item_name = cal_names("watched video"),
      col_limit = 1,
      range=4,
      start='2022-05-01',
      orientation='vertical',
      height="100%"
    )%>%
      cal_legend(show=FALSE, colors = cal_colors(min = "FFCCCC", max = "#FF0000"))
  })
  
  output$streamedCalendarB3 <- renderSupercaliheatmapwidget({
    supercal(
      calendarB,
      datetime_col = date, value_col = n,
      label = cal_label("top", "center", height = 20),
      cell_size = 20,
      cell_padding = 2,
      tooltip_item_name = cal_names("watched video"),
      col_limit = 1,
      range=4,
      start='2022-09-01',
      orientation='vertical',
      height="100%"
    )%>%
      cal_legend(show=FALSE, colors = cal_colors(min = "FFCCCC", max = "#FF0000"))
  })
  
  likePlot <- reactive({
    names <- hash()
    names[["dfP"]] <- "Paweł"
    names[["dfM"]] <- "Mikołaj"
    names[["dfB"]] <- "Michał"
    colors <- hash()
    colors[["dfB"]] <- '#124e78' 
    colors[["dfM"]] <- '#d74e09'
    colors[["dfP"]] <- '#6e0e0a'
    if (length(input$likeViewPlotFilter) == 3){
      likePlot <- ggplot(df, aes(x = likes)) +
        geom_density(data = get(input$likeViewPlotFilter[1]), aes(fill = names[[input$likeViewPlotFilter[1]]]),
                     alpha = 0.5)+
        geom_density(data = get(input$likeViewPlotFilter[2]), aes(fill = names[[input$likeViewPlotFilter[2]]]),
                     alpha = 0.5)+
        geom_density(data = get(input$likeViewPlotFilter[3]), aes(fill = names[[input$likeViewPlotFilter[3]]]),
                     alpha = 0.5) +
        scale_x_continuous(limits=input$likeRange, expand = c(0,0)) +
        scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
        scale_fill_manual("",labels = paste("<span style='color:",
                                            c(colors[[input$likeViewPlotFilter[3]]],colors[[input$likeViewPlotFilter[2]]],colors[[input$likeViewPlotFilter[1]]]),
                                            "'>",
                                            c(names[[input$likeViewPlotFilter[3]]],names[[input$likeViewPlotFilter[2]]],names[[input$likeViewPlotFilter[1]]]),
                                            "</span>"),
                          values=c(colors[[input$likeViewPlotFilter[3]]],colors[[input$likeViewPlotFilter[2]]],colors[[input$likeViewPlotFilter[1]]]))+
        theme_classic() +
        theme(plot.margin = margin(1,1,1,1, unit = 'cm'),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_text(size = 35, color="#ff9933",margin=margin(15,0,0,0)),
              axis.text.x = element_text(size = 22, face='bold', margin=margin(5,0,0,0)),
              axis.ticks.x = element_line(linewidth = 1.4),
              legend.spacing.y = unit(1,'cm'),
              legend.title = element_text(family='Roboto_condensed',size = 28, color='#d74e09'),
              legend.text = element_markdown(family='Roboto_condensed',size = 25),
              legend.title.align = 0.5,
              panel.background = element_blank(),
              plot.background = element_blank(),
              legend.background = element_rect(fill='transparent')) +
        guides(fill = guide_legend(byrow = TRUE))
    }
    else if (length(input$likeViewPlotFilter) == 2){
      likePlot <- ggplot(df, aes(x = likes)) +
        geom_density(data = get(input$likeViewPlotFilter[1]), aes(fill = names[[input$likeViewPlotFilter[1]]]),
                     alpha = 0.5)+
        geom_density(data = get(input$likeViewPlotFilter[2]), aes(fill = names[[input$likeViewPlotFilter[2]]]),
                     alpha = 0.5)+
        scale_x_continuous(limits=input$likeRange, expand = c(0,0)) +
        scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
        scale_fill_manual("",labels = paste("<span style='color:",
                                            c(colors[[input$likeViewPlotFilter[2]]], colors[[input$likeViewPlotFilter[1]]]),
                                            "'>",
                                            c(names[[input$likeViewPlotFilter[2]]], names[[input$likeViewPlotFilter[1]]]),
                                            "</span>"),
                          values=c(colors[[input$likeViewPlotFilter[2]]], colors[[input$likeViewPlotFilter[1]]]))+
        theme_classic() +
        theme(plot.margin = margin(1,1,1,1, unit = 'cm'),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_text(size = 35, color="#ff9933",margin=margin(15,0,0,0)),
              axis.text.x = element_text(size = 22, face='bold', margin=margin(5,0,0,0)),
              axis.ticks.x = element_line(linewidth = 1.4),
              legend.spacing.y = unit(1,'cm'),
              legend.title = element_text(family='Roboto_condensed',size = 28, color='#d74e09'),
              legend.text = element_markdown(family='Roboto_condensed',size = 25),
              legend.title.align = 0.5,
              panel.background = element_blank(),
              plot.background = element_blank(),
              legend.background = element_rect(fill='transparent')) +
        guides(fill = guide_legend(byrow = TRUE))
    }
    else if (length(input$likeViewPlotFilter) == 1){
      likePlot <- ggplot(df, aes(x = likes)) +
        geom_density(data = get(input$likeViewPlotFilter[1]), aes(fill = names[[input$likeViewPlotFilter[1]]]),
                     alpha = 0.5)+
        scale_x_continuous(limits=input$likeRange, expand = c(0,0)) +
        scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
        scale_fill_manual("",labels = paste("<span style='color:",
                                            c(colors[[input$likeViewPlotFilter[1]]]),
                                            "'>",
                                            c(names[[input$likeViewPlotFilter[1]]]),
                                            "</span>"),
                          values=c(colors[[input$likeViewPlotFilter[1]]]))+
        theme_classic() +
        theme(plot.margin = margin(1,1,1,1, unit = 'cm'),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_text(size = 35, color="#ff9933",margin=margin(15,0,0,0)),
              axis.text.x = element_text(size = 22, face='bold', margin=margin(5,0,0,0)),
              axis.ticks.x = element_line(linewidth = 1.4),
              legend.spacing.y = unit(1,'cm'),
              legend.title = element_text(family='Roboto_condensed',size = 28, color='#d74e09'),
              legend.text = element_markdown(family='Roboto_condensed',size = 25),
              legend.title.align = 0.5,
              panel.background = element_blank(),
              plot.background = element_blank(),
              legend.background = element_rect(fill='transparent')) +
        guides(fill = guide_legend(byrow = TRUE))
    }
    
    return (likePlot)
  })
  output$likePlot = renderPlot(likePlot(), bg="transparent")

  
  
  viewPlot <- reactive({
    names <- hash()
    names[["dfP"]] <- "Paweł"
    names[["dfM"]] <- "Mikołaj"
    names[["dfB"]] <- "Michał"
    colors <- hash()
    colors[["dfB"]] <- '#124e78' 
    colors[["dfM"]] <- '#d74e09'
    colors[["dfP"]] <- '#6e0e0a'
    if (length(input$likeViewPlotFilter) == 3){
      viewPlot <- ggplot(df, aes(x = views)) +
        geom_density(data = get(input$likeViewPlotFilter[1]), aes(fill = names[[input$likeViewPlotFilter[1]]]),
                     alpha = 0.5)+
        geom_density(data = get(input$likeViewPlotFilter[2]), aes(fill = names[[input$likeViewPlotFilter[2]]]),
                     alpha = 0.5)+
        geom_density(data = get(input$likeViewPlotFilter[3]), aes(fill = names[[input$likeViewPlotFilter[3]]]),
                     alpha = 0.5) +
        scale_x_continuous(limits=input$viewRange, expand = c(0,0)) +
        scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
        scale_fill_manual("",labels = paste("<span style='color:",
                                            c(colors[[input$likeViewPlotFilter[3]]],colors[[input$likeViewPlotFilter[2]]],colors[[input$likeViewPlotFilter[1]]]),
                                            "'>",
                                            c(names[[input$likeViewPlotFilter[3]]],names[[input$likeViewPlotFilter[2]]],names[[input$likeViewPlotFilter[1]]]),
                                            "</span>"),
                          values=c(colors[[input$likeViewPlotFilter[3]]],colors[[input$likeViewPlotFilter[2]]],colors[[input$likeViewPlotFilter[1]]]))+
        theme_classic() +
        theme(plot.margin = margin(1,1,1,1, unit = 'cm'),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_text(size = 35, color="#ff9933",margin=margin(15,0,0,0)),
              axis.text.x = element_text(size = 22, face='bold', margin=margin(5,0,0,0)),
              axis.ticks.x = element_line(linewidth = 1.4),
              legend.spacing.y = unit(1,'cm'),
              legend.title = element_text(family='Roboto_condensed',size = 28, color='#FF0000'),
              legend.text = element_markdown(family='Roboto_condensed',size = 25),
              legend.title.align = 0.5,
              panel.background = element_blank(),
              plot.background = element_blank(),
              legend.background = element_rect(fill='transparent')) +
        guides(fill = guide_legend(byrow = TRUE))
    }
    else if (length(input$likeViewPlotFilter) == 2){
      viewPlot <- ggplot(df, aes(x = views)) +
        geom_density(data = get(input$likeViewPlotFilter[1]), aes(fill = names[[input$likeViewPlotFilter[1]]]),
                     alpha = 0.5)+
        geom_density(data = get(input$likeViewPlotFilter[2]), aes(fill = names[[input$likeViewPlotFilter[2]]]),
                     alpha = 0.5)+
        scale_x_continuous(limits=input$viewRange, expand = c(0,0)) +
        scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
        scale_fill_manual("",labels = paste("<span style='color:",
                                            c(colors[[input$likeViewPlotFilter[2]]], colors[[input$likeViewPlotFilter[1]]]),
                                            "'>",
                                            c(names[[input$likeViewPlotFilter[2]]], names[[input$likeViewPlotFilter[1]]]),
                                            "</span>"),
                          values=c(colors[[input$likeViewPlotFilter[2]]], colors[[input$likeViewPlotFilter[1]]]))+
        theme_classic() +
        theme(plot.margin = margin(1,1,1,1, unit = 'cm'),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_text(size = 35, color="#ff9933",margin=margin(15,0,0,0)),
              axis.text.x = element_text(size = 22, face='bold', margin=margin(5,0,0,0)),
              axis.ticks.x = element_line(linewidth = 1.4),
              legend.spacing.y = unit(1,'cm'),
              legend.title = element_text(family='Roboto_condensed',size = 28, color='#FF0000'),
              legend.text = element_markdown(family='Roboto_condensed',size = 25),
              legend.title.align = 0.5,
              panel.background = element_blank(),
              plot.background = element_blank(),
              legend.background = element_rect(fill='transparent')) +
        guides(fill = guide_legend(byrow = TRUE))
    }
    else if (length(input$likeViewPlotFilter) == 1){
      viewPlot <- ggplot(df, aes(x = views)) +
        geom_density(data = get(input$likeViewPlotFilter[1]), aes(fill = names[[input$likeViewPlotFilter[1]]]),
                     alpha = 0.5)+
        scale_x_continuous(limits=input$viewRange, expand = c(0,0)) +
        scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
        scale_fill_manual("",labels = paste("<span style='color:",
                                            c(colors[[input$likeViewPlotFilter[1]]]),
                                            "'>",
                                            c(names[[input$likeViewPlotFilter[1]]]),
                                            "</span>"),
                          values=c(colors[[input$likeViewPlotFilter[1]]]))+
        theme_classic() +
        theme(plot.margin = margin(1,1,1,1, unit = 'cm'),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_text(size = 35, color="#ff9933",margin=margin(15,0,0,0)),
              axis.text.x = element_text(size = 22, face='bold', margin=margin(5,0,0,0)),
              axis.ticks.x = element_line(linewidth = 1.4),
              legend.spacing.y = unit(1,'cm'),
              legend.title = element_text(family='Roboto_condensed',size = 28, color='#FF0000'),
              legend.text = element_markdown(family='Roboto_condensed',size = 25),
              legend.title.align = 0.5,
              panel.background = element_blank(),
              plot.background = element_blank(),
              legend.background = element_rect(fill='transparent')) +
        guides(fill = guide_legend(byrow = TRUE))
    }
    
    return (viewPlot)
  })
  output$viewPlot = renderPlot(viewPlot(), bg="transparent")
}

