library(dplyr)
library(plotly)
library(forcats)
library(gganimate)

read_our_data <- function() {
  df_a <- read.csv("languages_ac.csv", encoding = "UTF-8")
  df_m <- read.csv("languages_mk.csv", encoding = "UTF-8")
  df_w <- read.csv("languages_wk.csv", encoding = "UTF-8")
  list(
    "a" = df_a,
    "m" = df_m,
    "w" = df_w)
}

plot_lang <- function(x, df_a, df_m, df_w, start.time, end.time) {
  df_list <- list(
    "alicja" = df_a,
    "martyna" = df_m,
    "wiktoria" = df_w)  
  if(length(x) == 0) {
    ggplot() +
      geom_blank() +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "#100c0c"),
            plot.background = element_rect(fill = "#100c0c")) -> p
  } else if(length(x) == 1) {

    df <- df_list[[tolower(x)]]

    df <- specify_time(df, start.time, end.time)
    
    df %>% 
      filter(language %in% c("pl", "en")) %>% 
      count(language) %>%
      mutate(percentage = 100*n/sum(n)) %>% 
      select(language, percentage) %>% 
      arrange(desc(percentage)) -> df
      
      
    ggplot(df, aes(x = language, y = percentage, text = paste0(round(percentage, digits = 1), sep = "%"))) +
      geom_col(fill = "#fb0404") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "#100c0c"),
            plot.background = element_rect(fill = "#100c0c"),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "gray34"),
            axis.text = element_text(color = "white")) +
      scale_x_discrete(labels = c("en" = "Angielski", "pl" = "Polski")) -> p
    
    ggplotly(p, tooltip = "text") %>% 
      layout(hoverlabel = list(bgcolor = "#2e2d2d"),
             yaxis = list(title = "Procent wszystkich filmów [%]", color = "white")) -> p
  } else {
    for (i in names(df_list)){
        df_list[[i]] %>% 
            mutate(include = i %in% tolower(x),
                   owner = substr(i, 1, 1)) -> df_list[[i]]
    }
    rbind(df_list[[1]], df_list[[2]], df_list[[3]]) -> df

    df %>% 
      filter(include == TRUE) -> df
    
    df <- specify_time(df, start.time, end.time)
    
    df %>% 
      filter(language %in% c("pl", "en")) %>%
      mutate(language = if_else(language == "pl",
                                "Polski",
                                "Angielski")) %>% 
      count(language, owner) %>%
      group_by(owner) %>% 
      summarise(percentage = 100*n/sum(n), language) -> df
    
    df$owner <- factor(df$owner, levels = rev(unique(df$owner)), ordered = TRUE)
    
    ggplot(df) +
      geom_col(aes(x = owner, y = percentage, fill = reorder(language, percentage), text = paste0(round(percentage, digits = 1), sep = "%")), position = "stack") +
      theme_minimal() + 
      theme(panel.background = element_rect(fill = "#100c0c"),
            plot.background = element_rect(fill = "#100c0c"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_line(color = "gray34"),
            panel.grid.minor.x = element_blank(),
            axis.text = element_text(color = "white"),
            legend.title = element_text(color = "white"),
            legend.text = element_text(color = "white")) +
      scale_x_discrete(labels = c("a" = "Alicja", "m" = "Martyna", "w" = "Wiktoria")) +
      labs(x = "") +
      scale_fill_manual(name = "Język:",
                        values = c("#fb0404", "#2e2d2d"),
                        labels = c("Polski", "Angielski")) +
      coord_flip() -> p
    
    ggplotly(p, tooltip = "text") %>% 
      layout(hoverlabel = list(bgcolor = "#2e2d2d"),
             xaxis = list(title = "Procent wszystkich filmów [%]", color = "white",
                          titlefont = list(size = 20), tickfont = list(size = 15)),
             yaxis = list(tickfont = list(size = 15)),
             legend = list(font = list(size = 15))) -> p
  }
  p
}
  
  
specify_time <- function(df, start.time, end.time) {
  df %>% 
    mutate(time = substr(as.character(time), 1, 10)) %>%
    filter(as.Date(time) > start.time & as.Date(time) < end.time) -> df
  df
}

chooseTOP5 <- function(df, df_lang, isenglish, start.time, end.time) {
  df %>%
    left_join(df_lang, by = c("title", "time")) %>%
    select(language, time, channelName) %>%
    specify_time(start.time, end.time) %>%
    filter(language == if_else(isenglish,
                              "en",
                              "pl")) %>% 
    count(channelName) %>% 
    filter(channelName != "") %>% 
    arrange(desc(n)) %>%
    select(channelName) %>% 
    head(5) -> df
  df
}

TOP5toString <- function(df) {
  res <- paste0(paste0(1:5, ". "), df$channelName, sep = "\n")
  res <- sub("1.", " 1.", res)
  cat(res)
} 
  
