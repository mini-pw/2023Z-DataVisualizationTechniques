plot_word <- function(df, start.time, end.time, freq){
  validate(
    need(end.time > start.time, "Data początkowa jest późniejsza niż końcowa"
    )
  )
  
  df <- df %>%
    mutate(time = substr(as.character(time),1,10)) %>%
    filter(channelName != "") %>%
    filter(as.Date(time) > start.time & as.Date(time) < end.time) %>%
    count(channelName) %>%
    filter(n>freq)
  wordcloud2(df,
             backgroundColor = "#100c0c",
             color=rep(c("#FF0000", "#F05555", "#EE7777", "EE3333"), length.out=nrow(df)),
             size = 0.8)
}

