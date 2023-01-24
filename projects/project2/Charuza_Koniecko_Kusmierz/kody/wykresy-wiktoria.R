library(dplyr)
library(ggplot2)
library(grid)
library(gridBase)


activity.heatmap <- function(df, start.time, end.time){
    # df - Data.Frame
    # start.time, end.time - POSIXlt
    date.vec <- as.POSIXlt(df$time, format = "%FT%T")
    watchtime.df <- data.frame(
        weekday = weekdays(date.vec),
        hour = unclass(date.vec)$hour,
        date = date.vec
        )
    watchtime.df$weekday <- factor(watchtime.df$weekday,
                               levels = c("Monday", "Tuesday", "Wednesday",
                                          "Thursday", "Friday", "Saturday", "Sunday"))
    watchtime.df %>% 
        filter(date > start.time, date < end.time) %>% 
        count(weekday, hour) %>% 
        ggplot(aes(x=weekday, y=hour, fill=n)) +
        geom_tile() +
        scale_y_reverse(n.breaks = 23, expand = c(0, 0), name = "godzina") +
        scale_x_discrete(expand = c(0, 0), name = "dzień tygodnia", position = "top", labels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela")) +
        scale_fill_gradientn(
            colours = c("#500000", "red4", "red2", "red", "indianred2", "lightsalmon2", "white")) +
        theme(
            line = element_blank(),
            text = element_text(colour = "white"),
            axis.title = element_text(colour = "white"),
            axis.text = element_text(colour = "white"),
            axis.ticks = element_blank(),
            panel.background = element_rect(colour = NA, fill = "#100c0c"),
            plot.background = element_rect(colour = NA, fill = "#100c0c"),
            legend.background = element_rect(fill = "#100c0c")
        ) +
        labs(title = "Aktywność na Youtube", fill="Liczba obejrzanych\nfilmów")
}

drawClock <- function(hour, minute) {
    # dodaje zegar do istniejącego wykresu kołowego
    t <- seq(0, 2*pi, length=13)[-13]
    x <- cos(t)
    y <- sin(t)
    
    vps <- baseViewports()
    pushViewport(vps$inner, vps$figure, vps$plot)
    # ticks
    grid.segments(x, y, x*.9, y*.9, default="native", gp=gpar(lex=2, col="#EAEAEA"))
    # Hour hand
    hourAngle <- pi/2 - (hour + minute/60)/12*2*pi
    grid.segments(0, 0, 
                  .6*cos(hourAngle), .6*sin(hourAngle), 
                  default="native", gp=gpar(lex=4, col="#EAEAEA"))
    # Minute hand
    minuteAngle <- pi/2 - (minute)/60*2*pi
    grid.segments(0, 0, 
                  .8*cos(minuteAngle), .8*sin(minuteAngle), 
                  default="native", gp=gpar(lex=2, col="#EAEAEA"))   
    grid.circle(0,0,1.02,default.units = "native", gp=gpar(fill=NA, lex=10, col="#282424"))
    popViewport(3)
}

clock.pie <- function(watchtime.df, start.time, end.time, scale, AM=TRUE){
    # rysuje jeden zegar, przed lub po południu
    # start.time, end.time - POSIXlt
    pie.colours <- c("#100c0c", "#330000", "#770000", "#AA0000", "#CC0000", "#FF0000", "#FF4444")
    watchtime.df %>% 
        filter(time < end.time & time > start.time) -> watchtime.df
    if(AM) watchtime.df %>% filter(hour<12) -> watchtime.df
    else watchtime.df %>% filter(hour>=12) %>% mutate(hour = hour-12) -> watchtime.df
    if (nrow(watchtime.df)==0) return()
    watchtime.df %>%
        count(hour) %>% 
        mutate(colour = (length(pie.colours)-1)*n/scale) %>% 
        add_row(
            hour = 0:11,
            n = rep(0, 12),
            colour = rep(1, 12)) %>% 
        group_by(hour) %>%
        summarise(n = sum(n), colour = sum(colour)) -> hours.df
    par(bg="#100c0c", col="white", col.main="white", mar=c(7,1,7,1))
    pie(
        main = ifelse(AM, "AM", "PM"),
        x = rep(1, 12),
        clockwise = TRUE,
        radius = 1,
        init.angle = 90,
        labels = "",
        col = pie.colours[hours.df$colour],
        border = NA,
        cex.main=2
    )
    date.vec <- as.POSIXlt(watchtime.df$time)
    date.vec <- strptime(
        paste(unclass(date.vec)[["hour"]], unclass(date.vec)[["min"]], sep = ":"),
        format = "%H:%M")
    mean.time <- as.POSIXlt(mean(date.vec, na.rm = TRUE))
    drawClock(mean.time$hour, mean.time$min)
    par(bg="white", col="black", col.main="black", mar=c(1,1,1,1))
}

clocks.plot <- function(df, start.time, end.time){
    # rysuje dwa zegary dla danego przedziału czasowego
    # start.time, end.time - POSIXlt
    date.vec <- as.POSIXlt(df$time, format = "%FT%T")
    watchtime.df <- data.frame(
        hour = unclass(date.vec)$hour,
        time = date.vec)
    scale <- max((watchtime.df %>%
                      filter(time < end.time, time > start.time) %>% 
                      count(hour))$n)
    par(mfrow=c(1,3), mar=c(1,1,1,1))
    clock.pie(watchtime.df,start.time, end.time, scale, TRUE)
    clock.pie(watchtime.df, start.time, end.time, scale, FALSE)
    labs <- paste(floor(0:6*scale/7), floor(1:7*scale/7), sep=" - ")
    par(mfrow=c(1,1),mar=c(5,1,5,1))
    legend("right",
           legend = labs,
           title = "Liczba obejrzanych\nfilmów",
           bty = "n",
           text.col = "white",
           fill = c("#100c0c", "#330000", "#770000", "#AA0000", "#CC0000", "#FF0000", "#FF4444"),
           border = "white")
}

