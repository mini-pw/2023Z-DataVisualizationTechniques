library("ggtext")
library("rjson")
library("reticulate")
library("feather")
library(dplyr)
library("zoo")
library("ggplot2")
library(xts)
library(lubridate)
library(chron)
library(parsedate)
library(scales)
library(circular)
library(gridExtra)
library(ggpubr)
library(extrafont)
library(showtext)
library(ggchicklet)

showtext_auto()

options(scipen = 100)

result <- fromJSON(file = "historia_oglądania.json")

title = list('python')
titleUrl = list('python')
channel = list('python')
channelUrl = list('python')
time = list('python')
 
for (p in result) {
  if (names(p[4]) == "subtitles") {
    title <- append(title, p[[2]])
    titleUrl <- append(titleUrl, p[[3]])
    channel <- append(channel, p[[4]][[1]][[1]])
    channelUrl <- append(channelUrl, p[[4]][[1]][[2]])
    time <- append(time, p[[5]])
  }
}

df <- data.frame(unlist(title),unlist(titleUrl), unlist(channel), unlist(channelUrl), unlist(time))
df <- df[-1,]
rownames(df) <- NULL
names(df) = c("title","titleUrl","channel","channelUrl", "time")

df <- mutate(df, title = substr(df$title,12, nchar(df$title)))

write_feather(df, "foobar.feather")

dfP <- read.csv("Youtube_full_stats.csv")
dfP2 <- read.csv("Youtube_full_stats2.csv", sep=";")
dfP3 <- read.csv("Youtube_full_stats4000-8000_pawel.csv", sep=";")
colnames(dfP2)[6] <- 'id'
dfP <- rbind(dfP,dfP2,dfP3)
dfP <- data.frame(append(dfP, c(who='P')))

dfM <- read.csv("Youtube_full_stats_frosty.csv")
dfM2 <- read.csv("Youtube_full_stats_frosty_2.csv")
dfM3 <- read.csv("Youtube_full_stats_frosty_3.csv")
dfM <- dfM[,-1]
dfM2 <- dfM2[,-1]
dfM3 <- dfM3[,-1]
dfM <- rbind(dfM,dfM2,dfM3)
dfM <- data.frame(append(dfM, c(who='M')))

dfB <- read.csv("Youtube_full_stats_bindus.csv", sep=";")
dfB2 <- read.csv("Youtube_full_stats_michal2.csv", sep=";")
dfB3 <- read.csv("Youtube_full_stats3.csv", sep=";")
dfB <- rbind(dfB,dfB2,dfB3)
dfB <- data.frame(append(dfB, c(who='B')))


df <- rbind(dfP,dfM,dfB)

df <- na.omit(df)

df <- mutate(df, duration = substr(df$duration,8,15))
df <- mutate(df, duration = lubridate::hms(df$duration))
df <- mutate(df, duration = period_to_seconds(df$duration))

df <- mutate(df, time = parse_iso_8601(df$time))
df <- df[,-10]

df <- df %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())

df <- mutate(df, weekday = weekdays(df$time))



videos_per_channel <- df %>% 
  group_by(channel, who) %>% 
  mutate(number_of_videos = n()) %>% 
  select(number_of_videos)
videos_per_channel <- merge(videos_per_channel, df[,c(4,11)], by = 'channel', all.x = TRUE)
videos_per_channel <- videos_per_channel[!duplicated(videos_per_channel[c(1,2)]),]
videos_per_channel <- videos_per_channel[order(df[,'channel'],-df[,'channelSubscribers']),]
videos_per_channel <- arrange(videos_per_channel, desc(videos_per_channel$number_of_videos))


watch_time_per_channel <- aggregate(df$duration, by=list(Category=df$channel), FUN=sum)
names(watch_time_per_channel) = c("channel", "watchtime")
watch_time_per_channel <- merge(watch_time_per_channel, df[,c(4,11,12)], by = 'channel', all.x = TRUE) 
watch_time_per_channel <- watch_time_per_channel[!duplicated(watch_time_per_channel[c(1,2)]),]
watch_time_per_channel <- watch_time_per_channel[order(df[,'channel'],-df[,'channelSubscribers']),]
watch_time_per_channel <- arrange(watch_time_per_channel, desc(watch_time_per_channel$watchtime))

watch_time_per_channelP <- filter(watch_time_per_channel, watch_time_per_channel$who =="P")
watch_time_per_channelM <- filter(watch_time_per_channel, watch_time_per_channel$who =="M")
watch_time_per_channelB <- filter(watch_time_per_channel, watch_time_per_channel$who =="B")

videos_per_channelP <- filter(videos_per_channel, videos_per_channel$who =="P")
videos_per_channelM <- filter(videos_per_channel, videos_per_channel$who =="M")
videos_per_channelB <- filter(videos_per_channel, videos_per_channel$who =="B")


videos_per_weekday <- df %>% 
  group_by(weekday, who) %>% 
  mutate(number_of_videos = n()) %>% 
  arrange(desc(number_of_videos)) %>% 
  select(number_of_videos)
videos_per_weekday <- unique(videos_per_weekday, by = 'weekday')
videos_per_weekday$weekday <- factor(videos_per_weekday$weekday, levels =
                                           c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday",
                                             "Tuesday", "Monday")) 


weekdayPlot <- ggplot(videos_per_weekday, aes(x=weekday, y=number_of_videos, fill=who)) +
  geom_chicklet(position = "dodge")
videos_per_weekday$who <- factor(videos_per_weekday$who, levels =
                                           c("B","M","P")) 

fill <- c('#124e78', '#d74e09','#6e0e0a') #'#2e86ab', Mikołaj='#c73e1d', Paweł='#f18f01')
weekdayPlot <- ggplot(videos_per_weekday, aes(x=weekday, y=number_of_videos, fill=who), alpha = 0.5) +
  geom_chicklet(position = "dodge") +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() + 
  theme_classic() +
  labs(y = 'watched videos') +
  scale_fill_manual("",labels = paste("<span style='color:",
                                      fill,
                                      "'>",
                                      c("Michał", "Mikołaj", "Paweł"),
                                      "</span>"),
                    values=fill)+
  theme(plot.margin = margin(1,1,1,1, unit = 'cm'),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 30, colour = '#ff9933', margin=margin(15,0,0,0)),
        axis.text.x = element_text(size = 23, face='bold', margin=margin(5,0,0,0)),
        axis.text.y = element_text(size = 25, colour = '#ff9933', face='bold', margin=margin(0,5,0,0)),
        legend.spacing.y = unit(1,'cm'),
        legend.title = element_blank(),
        legend.text = element_markdown(size = 30),
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_rect(fill='transparent'))+
  guides(fill = guide_legend(byrow = TRUE))
weekdayPlot

watch_time_per_weekday <- aggregate(df$duration, by=list(Category=df$weekday), FUN=sum)
names(watch_time_per_weekday) = c("weekday", "watchtime")
watch_time_per_weekday <- arrange(watch_time_per_weekday, desc(watch_time_per_weekday$watchtime))
watch_time_per_weekday$weekday <- factor(watch_time_per_weekday$weekday, levels =
                                           c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday",
                                             "Tuesday", "Monday")) 



dfP <- filter(df, df$who == 'P')
dfM <- filter(df, df$who == 'M')
dfB <- filter(df, df$who == 'B')

datetimesP = dfP$time%>%
  lubridate::parse_date_time("%Y/%m/%d %h:%M:%S")
times_in_decimalP = lubridate::hour(datetimesP) + lubridate::minute(datetimesP) / 60
times_in_radiansP = 2 * pi * (times_in_decimalP / 24)

# Doing this just for bandwidth estimation:
basic_densP = density(times_in_radiansP, from = 0, to = 2 * pi)

resP = circular::density.circular(circular::circular(times_in_radiansP,
                                                    type = "angle",
                                                    units = "radians",
                                                    rotation = "clock"),
                                 kernel = "wrappednormal",
                                 bw = basic_densP$bw)

time_pdfP = data.frame(time = as.numeric(24 * (2 * pi + resP$x) / (2 * pi)), # Convert from radians back to 24h clock
                      likelihood = resP$y)

colors <- c(Paweł='#6e0e0a')
hourPlotP <- ggplot(time_pdfP) +
  geom_area(aes(x = time, y = likelihood, fill = "Paweł"), alpha = 0.5,
                                            color = 1,    # Line color
                                            linewidth = 0.5,    # Line width
                                            linetype = 1) +
  scale_x_continuous("Hour", labels = 0:24, breaks = 0:24, expand = c(0,0)) +
  scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
  scale_fill_manual("",labels = paste("<span style='color:",
                                            colors,
                                            "'>",
                                            c("Paweł"),
                                            "</span>"),
                    values=colors)+
  theme_classic()+
  theme(plot.margin = margin(1,1,1,1, unit = 'cm'),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 35, color="#ff9933",margin=margin(15,0,0,0)),
        axis.text.x = element_text(size = 22, face='bold', margin=margin(5,0,0,0)),
        axis.ticks.x = element_line(linewidth = 1.4),
        legend.spacing.y = unit(1,'cm'),
        legend.title = element_text(size = 28, color='#d74e09'),
        legend.text = element_markdown(size = 25),
        legend.title.align = 0.5,
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_rect(fill='transparent')) +
  guides(fill = guide_legend(byrow = TRUE))
  
hourPlotP

datetimesM = dfM$time%>%
  lubridate::parse_date_time("%Y/%m/%d %h:%M:%S")
times_in_decimalM = lubridate::hour(datetimesM) + lubridate::minute(datetimesM) / 60
times_in_radiansM = 2 * pi * (times_in_decimalM / 24)

# Doing this just for bandwidth estimation:
basic_densM = density(times_in_radiansM, from = 0, to = 2 * pi)

resM = circular::density.circular(circular::circular(times_in_radiansM,
                                                    type = "angle",
                                                    units = "radians",
                                                    rotation = "clock"),
                                 kernel = "wrappednormal",
                                 bw = basic_densM$bw)

time_pdfM = data.frame(time = as.numeric(24 * (2 * pi + resM$x) / (2 * pi)), # Convert from radians back to 24h clock
                      likelihood = resM$y)

colors <- c(Mikołaj='#d74e09')
hourPlotM <- ggplot(time_pdfM) +
  geom_area(aes(x = time, y = likelihood, fill = "Mikołaj"), alpha = 0.5,
            color = 1,    # Line color
            linewidth = 0.5,    # Line width
            linetype = 1) +
  scale_x_continuous("Hour", labels = 0:24, breaks = 0:24, expand = c(0,0)) +
  scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
  scale_fill_manual("",labels = paste("<span style='color:",
                                            colors,
                                            "'>",
                                            c("Mikołaj"),
                                            "</span>"),
                    values=colors)+
  theme_classic()+
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

hourPlotM

datetimesB = dfB$time%>%
  lubridate::parse_date_time("%Y/%m/%d %h:%M:%S")
times_in_decimalB = lubridate::hour(datetimesB) + lubridate::minute(datetimesB) / 60
times_in_radiansB = 2 * pi * (times_in_decimalB / 24)

# Doing this just for bandwidth estimation:
basic_densB = density(times_in_radiansB, from = 0, to = 2 * pi)

resB = circular::density.circular(circular::circular(times_in_radiansB,
                                                     type = "angle",
                                                     units = "radians",
                                                     rotation = "clock"),
                                  kernel = "wrappednormal",
                                  bw = basic_densB$bw)

time_pdfB = data.frame(time = as.numeric(24 * (2 * pi + resB$x) / (2 * pi)), # Convert from radians back to 24h clock
                       likelihood = resB$y)

colors <- c(Michał='#124e78')
hourPlotB <- ggplot(time_pdfB) +
  geom_area(aes(x = time, y = likelihood, fill = "Michał"), alpha = 0.5,
            color = 1,    # Line color
            linewidth = 0.5,    # Line width
            linetype = 1) +
  scale_x_continuous("Hour", labels = 0:24, breaks = 0:24, expand = c(0,0)) +
  scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
  scale_fill_manual("",labels = paste("<span style='color:",
                                            colors,
                                            "'>",
                                            c("Michał"),
                                            "</span>"),
                    values=colors)+
  theme_classic()+
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

hourPlotB

colors <- c(Mikołaj='#d74e09', Paweł='#6e0e0a')
hourPlotPM <- ggplot(NULL,aes(x = time, y = likelihood))+
  geom_area(data=time_pdfP, aes(fill = "Paweł"), alpha = 0.5,
            color = 1,    # Line color
            linewidth = 0.5,    # Line width
            linetype = 1) +
  geom_area(data=time_pdfM, aes(fill = "Mikołaj"), alpha = 0.5,
            color = 1,    # Line color
            linewidth = 0.5,    # Line width
            linetype = 1) +
  scale_x_continuous("Hour", labels = 0:24, breaks = 0:24, expand = c(0,0)) +
  scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
  scale_fill_manual("",labels = paste("<span style='color:",
                                            colors,
                                            "'>",
                                            c("Mikołaj","Paweł"),
                                            "</span>"),
                    values=colors)+
  theme_classic()+
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
hourPlotPM

colors <- c(Michał='#124e78', Paweł='#6e0e0a')
hourPlotPB <- ggplot(NULL,aes(x = time, y = likelihood))+
  geom_area(data=time_pdfP, aes(fill = "Paweł"), alpha = 0.5,
            color = 1,    # Line color
            linewidth = 0.5,    # Line width
            linetype = 1) +
  geom_area(data=time_pdfB, aes(fill = "Michał"), alpha = 0.5,
            color = 1,    # Line color
            linewidth = 0.5,    # Line width
            linetype = 1) +
  scale_x_continuous("Hour", labels = 0:24, breaks = 0:24, expand = c(0,0)) +
  scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
  scale_fill_manual("",labels = paste("<span style='color:",
                                            colors,
                                            "'>",
                                            c("Michał","Paweł"),
                                            "</span>"),
                    values=colors)+
  theme_classic()+
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
hourPlotPB

colors <- c(Michał='#124e78', Mikołaj='#d74e09')
hourPlotMB <- ggplot(NULL,aes(x = time, y = likelihood))+
  geom_area(data=time_pdfM, aes(fill = "Mikołaj"), alpha = 0.5,
            color = 1,    # Line color
            linewidth = 0.5,    # Line width
            linetype = 1) +
  geom_area(data=time_pdfB, aes(fill = "Michał"), alpha = 0.5,
            color = 1,    # Line color
            linewidth = 0.5,    # Line width
            linetype = 1) +
  scale_x_continuous("Hour", labels = 0:24, breaks = 0:24, expand = c(0,0)) +
  scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
  scale_fill_manual("",labels = paste("<span style='color:",
                                            colors,
                                            "'>",
                                            c("Michał","Mikołaj"),
                                            "</span>"),
                    values=colors)+
  theme_classic()+
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
hourPlotMB

colors <- c(Michał='#124e78', Mikołaj='#d74e09', Paweł='#6e0e0a')
hourPlotAll <- ggplot(NULL,aes(x = time, y = likelihood))+
  geom_area(data=time_pdfP, aes(fill='Paweł'),alpha = 0.5,
            color = 1,    # Line color
            linewidth = 0.5,    # Line width
            linetype = 1) +
  geom_area(data=time_pdfM, aes(fill='Mikołaj'),alpha = 0.5,
            color = 1,    # Line color
            linewidth = 0.5,    # Line width
            linetype = 1) +
  geom_area(data=time_pdfB, aes(fill='Michał'), alpha = 0.5,
            color = 1,    # Line color
            linewidth = 0.5,    # Line width
            linetype = 1) +
  scale_x_continuous("Hour", labels = 0:24, breaks = 0:24, expand = c(0,0)) +
  scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
  scale_fill_manual("",labels = paste("<span style='color:",
                                            colors,
                                            "'>",
                                            c("Michał","Mikołaj","Paweł"),
                                            "</span>"),
                    values=colors)+
  theme_classic()+
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
hourPlotAll

calendarM <- dfM %>%
  mutate(day = substr(time,1,11)) %>% 
  group_by(day) %>%
  count() %>%
  rename(date=day)

calendarP <- dfP %>%
  mutate(day = substr(time,1,11)) %>% 
  group_by(day) %>%
  count() %>%
  rename(date=day)

calendarB <- dfB %>%
  mutate(day = substr(time,1,11)) %>% 
  group_by(day) %>%
  count() %>%
  rename(date=day)


colors <- c(Paweł='#6e0e0a')
likePlotP <- ggplot(df, aes(x = likes)) +
  geom_density(data = dfP, aes(fill = "Paweł"),
               alpha = 0.5)+
  scale_x_continuous(limits=c(0,10000), expand = c(0,0)) +
  scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
  scale_fill_manual("",labels = paste("<span style='color:",
                                      colors,
                                      "'>",
                                      c("Paweł"),
                                      "</span>"),
                    values=colors)+
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

likePlotP

colors <- c(Mikołaj='#d74e09')
likePlotM <- ggplot(df, aes(x = likes)) +
  geom_density(data = dfM, aes(fill = "Mikołaj"),
               alpha = 0.5)+
  scale_x_continuous(limits=c(0,10000), expand = c(0,0)) +
  scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
  scale_fill_manual("",labels = paste("<span style='color:",
                                      colors,
                                      "'>",
                                      c("Mikołaj"),
                                      "</span>"),
                    values=colors)+
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

likePlotM

colors <- c(Michał='#124e78')
likePlotB <- ggplot(df, aes(x = likes)) +
  geom_density(data = dfB, aes(fill = "Michał"),
               alpha = 0.5)+
  scale_x_continuous(limits=c(0,10000), expand = c(0,0)) +
  scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
  scale_fill_manual("",labels = paste("<span style='color:",
                                      colors,
                                      "'>",
                                      c("Michał"),
                                      "</span>"),
                    values=colors)+
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

likePlotB

colors <- c(Mikołaj='#d74e09', Paweł='#6e0e0a')
likePlotPM <- ggplot(df, aes(x = likes)) +
  geom_density(data = dfP, aes(fill = "Paweł"),
               alpha = 0.5)+
  geom_density(data = dfM, aes(fill = "Mikołaj"),
               alpha = 0.5)+
  scale_x_continuous(limits=c(0,10000), expand = c(0,0)) +
  scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
  scale_fill_manual("",labels = paste("<span style='color:",
                                      colors,
                                      "'>",
                                      c("Mikołaj", "Paweł"),
                                      "</span>"),
                    values=colors)+
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

likePlotPM

colors2 <- c(Michał='#124e78', Paweł='#6e0e0a')
likePlotPB <- ggplot(df, aes(x = likes)) +
  geom_density(data = dfP, aes(fill = "Paweł"),
             alpha = 0.5)+
  geom_density(data = dfB, aes(fill = "Michał"),
               alpha = 0.5)+
  scale_x_continuous(limits=c(0,20000), expand = c(0,0)) +
  scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
  scale_fill_manual("",labels = paste("<span style='color:",
                                      colors2,
                                      "'>",
                                      c("Michał", "Paweł"),
                                      "</span>"),
                    values=colors2)+
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

likePlotPB

colors2 <- c(Michał='#124e78', Mikołaj='#d74e09')
likePlotMB <- ggplot(df, aes(x = likes)) +
  geom_density(data = dfM, aes(fill = "Mikołaj"),
               alpha = 0.5)+
  geom_density(data = dfB, aes(fill = "Michał"),
               alpha = 0.5) +
  scale_x_continuous(limits=c(0,10000), expand = c(0,0)) +
  scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
  scale_fill_manual("",labels = paste("<span style='color:",
                                      colors2,
                                      "'>",
                                      c("Michał", "Mikołaj"),
                                      "</span>"),
                    values=colors2)+
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

likePlotMB

colors <- c(Michał='#124e78', Mikołaj='#d74e09', Paweł='#6e0e0a')
likePlotAll <- ggplot(df, aes(x = likes)) +
  geom_density(data = dfP, aes(fill = "Paweł"),
                alpha = 0.5)+
  geom_density(data = dfM, aes(fill = "Mikołaj"),
                alpha = 0.5)+
  geom_density(data = dfB, aes(fill = "Michał"),
                alpha = 0.5) +
  scale_x_continuous(limits=c(0,10000), expand = c(0,0)) +
  scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
  scale_fill_manual("",labels = paste("<span style='color:",
                                       colors,
                                       "'>",
                                       c("Michał", "Mikołaj", "Paweł"),
                                       "</span>"),
                     values=colors)+
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

likePlotAll

colors <- c(Paweł='#6e0e0a')
viewPlotP <- ggplot(df, aes(x = views)) +
  geom_density(data = dfP, aes(fill = "Paweł"),
               alpha = 0.5)+
  scale_x_continuous(limits=c(0,500000), expand = c(0,0)) +
  scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
  scale_fill_manual("",labels = paste("<span style='color:",
                                      colors,
                                      "'>",
                                      c("Paweł"),
                                      "</span>"),
                    values=colors)+
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

viewPlotP

colors <- c(Mikołaj='#d74e09')
viewPlotM <- ggplot(df, aes(x = views)) +
  geom_density(data = dfM, aes(fill = "Mikołaj"),
               alpha = 0.5)+
  scale_x_continuous(limits=c(0,500000), expand = c(0,0)) +
  scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
  scale_fill_manual("",labels = paste("<span style='color:",
                                      colors,
                                      "'>",
                                      c("Mikołaj"),
                                      "</span>"),
                    values=colors)+
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

viewPlotM

colors <- c(Michał='#124e78')
viewPlotB <- ggplot(df, aes(x = views)) +
  geom_density(data = dfB, aes(fill = "Michał"),
               alpha = 0.5)+
  scale_x_continuous(limits=c(0,500000), expand = c(0,0)) +
  scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
  scale_fill_manual("",labels = paste("<span style='color:",
                                      colors,
                                      "'>",
                                      c("Michał"),
                                      "</span>"),
                    values=colors)+
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

viewPlotB

colors <- c(Mikołaj='#d74e09', Paweł='#6e0e0a')
viewPlotPM <- ggplot(df, aes(x = views)) +
  geom_density(data = dfP, aes(fill = "Paweł"),
               alpha = 0.5)+
  geom_density(data = dfM, aes(fill = "Mikołaj"),
               alpha = 0.5)+
  scale_x_continuous(limits=c(0,500000), expand = c(0,0)) +
  scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
  scale_fill_manual("",labels = paste("<span style='color:",
                                      colors,
                                      "'>",
                                      c("Mikołaj", "Paweł"),
                                      "</span>"),
                    values=colors)+
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

viewPlotPM

colors <- c(Michał='#124e78', Paweł='#6e0e0a')
viewPlotPB <- ggplot(df, aes(x = views)) +
  geom_density(data = dfP, aes(fill = "Paweł"),
               alpha = 0.5)+
  geom_density(data = dfB, aes(fill = "Michał"),
               alpha = 0.5)+
  scale_x_continuous(limits=c(0,500000), expand = c(0,0)) +
  scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
  scale_fill_manual("",labels = paste("<span style='color:",
                                      colors,
                                      "'>",
                                      c("Michał", "Paweł"),
                                      "</span>"),
                    values=colors)+
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

viewPlotPB

colors <- c(Michał='#124e78', Mikołaj='#d74e09')
viewPlotMB <- ggplot(df, aes(x = views)) +
  geom_density(data = dfM, aes(fill = "Mikołaj"),
               alpha = 0.5)+
  geom_density(data = dfB, aes(fill = "Michał"),
               alpha = 0.5) +
  scale_x_continuous(limits=c(0,500000), expand = c(0,0)) +
  scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
  scale_fill_manual("",labels = paste("<span style='color:",
                                      colors,
                                      "'>",
                                      c("Michał", "Mikołaj"),
                                      "</span>"),
                    values=colors)+
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

viewPlotMB

colors2 <- c(Michał='#124e78', Mikołaj='#d74e09', Paweł='#6e0e0a')
viewPlotAll <- ggplot(df, aes(x = views)) +
  geom_density(data = dfP, aes(fill = "Paweł"),
               alpha = 0.5)+
  geom_density(data = dfM, aes(fill = "Mikołaj"),
               alpha = 0.5)+
  geom_density(data = dfB, aes(fill = "Michał"),
               alpha = 0.5) +
  scale_x_continuous(limits=c(0,800000), expand = c(0,0)) +
  scale_y_continuous("Likelihood of Data", expand = c(0,0)) +
  scale_fill_manual("",labels = paste("<span style='color:",
                                      colors2,
                                      "'>",
                                      c("Michał", "Mikołaj", "Paweł"),
                                      "</span>"),
                    values=colors2)+
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

viewPlotAll

