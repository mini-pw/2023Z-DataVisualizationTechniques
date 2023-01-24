
library(ggplot2)
library(dplyr)
library(ggrepel)
library(shinycssloaders)
library(reshape2)
library(tidyquant)
library(gridExtra)
library(grid)
source("wykres_ratingu.R")
source("rozklad_ruchow.R")

czas_mat_poddanie_size = 15
y_ticks_label_size = 12
procent_wygranych_przegranych_size = 18
background_color = "#202124"
text_color = "white"

#Odpowiada za tytuł wykresu
tytul = 30
#Odpowiada za podtytuł i nazwy osi
podtytul = 24
#Odpowiada za wartości na osiach
osie = 18


source('ui_strony.R')
server <- function(input, output, session) {
  
  output$rating <- renderPlot({ratings <- read.csv("daily_data.csv")
  colors <- c("nizwant" = "#8a0404","wiktorw123" = "#024f1a","LauPaSat" = "#070982")
  

  
  ratings %>% 
    mutate(date = as.Date(ratings$date,"%Y-%m-%d")) %>% 
    filter(player_name %in% input$uzytkownik,
           date >= input$date[1], date <= input$date[2],
           mode == input$typ_gry) %>% 
    ggplot(aes(x = date, y = rating, group = player_name,color = player_name))+
    geom_line(linewidth = 2.5)+
    geom_hline(yintercept=1500, colour = "#00c2cb", linetype = "dashed", linewidth = 2)+
    annotate("text",
             label = "Początkowy rating",
             x=as.Date("2023-01-14","%Y-%m-%d"),
             y=1450,
             colour = "white",
             size = 6)+
    theme_bw()+
    labs(title = "Rating w czasie")+
    ylab("Rating")+
    ylim(500,NA)+
    scale_y_continuous(expand = c(0,150))+
    scale_color_manual(values = colors)+
    scale_x_date(name = "Data", date_breaks = "4 days", date_labels = "%d-%m", expand = c(0,0.2), limits = c(as.Date("2022-12-07","%Y-%m-%d"),as.Date("2023-01-21","%Y-%m-%d")))+
    theme(axis.text=element_text(size=osie, colour = text_color),
          plot.title = element_text(size=tytul, colour = text_color, hjust = 0.5),
          panel.border = element_blank(),
          legend.position="none",
          panel.grid = element_line(colour = "#4e5c68"),
          axis.title = element_text(size = podtytul, color = text_color),
          panel.background = element_rect(fill=background_color),
          plot.background = element_rect(fill=background_color, color=NA))
  
  
    
  
    })
  
  output$rozklad <- renderPlot({data <- read.csv("data.csv")
  colors <- c("nizwant" = "#8a0404","wiktorw123" = "#024f1a","LauPaSat" = "#070982")
  LauPaSat_median <- median(filter(data, player_name == "LauPaSat")$move_count)
  nizwant_median <- median(filter(data, player_name == "nizwant")$move_count)
  wiktorw123_median <- median(filter(data, player_name == "wiktorw123")$move_count)
  
  
  
  data$date <- strptime(as.character(data$date), '%Y-%d-%m')
  data$date <- format(data$date, '%Y-%m-%d')
  
  data %>% 
    filter(player_name %in% input$uzytkownik,
                  date >= input$date[1], date <= input$date[2],
           game_type == input$typ_gry, 
           played_as %in% input$kolor) %>% 
    ggplot(aes(x = move_count, fill = player_name, color = player_name))+
    geom_density(linewidth = 2,alpha = 0.2)+
    theme_bw()+
    labs(title = "Rozkład liczby ruchów w partii")+
    xlab("Liczba ruchów")+
    ylab("Gęstość")+
    ylim(0,NA)+
    scale_x_continuous(expand = c(0,0.3), limits = c(0,100))+
    scale_y_continuous(expand = c(0,0.002))+
    scale_color_manual(values = colors)+
    scale_fill_manual(values = colors)+
    theme(axis.text=element_text(size=osie, colour = text_color),
          plot.title = element_text(size=tytul, colour = text_color, hjust = 0.5),
          panel.border = element_blank(),
          legend.position="none",
          panel.grid = element_line(colour = "#4e5c68"),
          axis.title = element_text(size = podtytul, color = text_color),
          panel.background = element_rect(fill=background_color),
          plot.background = element_rect(fill=background_color, color=NA)) 

                                   
  })
  
  
  output$procenty <- renderPlot({
    df <- read.csv("data.csv")
    
    
    colors <- c("nizwant" = "#8a0404","wiktorw123" = "#024f1a","LauPaSat" = "#070982")
    czas_mat_poddanie_size = 15
    y_ticks_label_size = 12
    procent_wygranych_przegranych_size = 18
    background_color = "#202124"
    text_color = "white"
    
    df$date <- strptime(as.character(df$date), '%Y-%d-%m')
    df$date <- format(df$date, '%Y-%m-%d')
    
    # przygotowanie danych dla wygranych
    wygrane <- df %>% filter( was_win == "True",
                              player_name %in% input$uzytkownik,
                             date >= input$date[1], date <= input$date[2],
                             game_type == input$typ_gry, 
                             played_as %in% input$kolor)
    
    
    przegrane <- df %>% filter( was_win == "False",
                                player_name %in% input$uzytkownik,
                                date >= input$date[1], date <= input$date[2],
                                game_type == input$typ_gry, 
                                played_as %in% input$kolor)
    
    
    if(dim(wygrane)[1] > 0 & dim(przegrane)[1] > 0){
    wygrane_ilosci <- wygrane %>%
      count(player_name) %>%
      rename(ilosc_wygranych = n)
    
    wygrane_czas_ilosci <- wygrane %>%
      filter(how_ended == "outoftime") %>%
      count(player_name) %>%
      rename(wygrane_czas = n)
    
    wygrane_czas_mat <- wygrane %>%
      filter(how_ended == "mate") %>%
      count(player_name) %>%
      rename(wygrane_mat = n)
    
    wygrane_czas_poddanie <- wygrane %>%
      filter(how_ended == "resign") %>%
      count(player_name) %>%
      rename(wygrane_poddanie = n)
    
    
    wygrane_wszystkie <- wygrane_ilosci %>%
      left_join(wygrane_czas_ilosci, by="player_name") %>%
      left_join(wygrane_czas_mat, by="player_name") %>%
      left_join(wygrane_czas_poddanie, by="player_name") %>%
      replace(is.na(.), 0)
    
    wygrane_wszystkie <- wygrane_wszystkie %>%
      mutate(wygrane_czas_procent = round(wygrane_czas/ilosc_wygranych,2)) %>%
      mutate(wygrane_mat_procent = round(wygrane_mat/ilosc_wygranych,2)) %>%
      mutate(wygrane_poddanie_procent = round(wygrane_poddanie/ilosc_wygranych,2))
    
    
    # analogiczne przygotowanie danych dla przegranych
    
    
    przegrane_ilosci <- przegrane %>%
      count(player_name) %>%
      rename(ilosc_przegrane = n)
    
    przegrane_czas_ilosci <- przegrane %>%
      filter(how_ended == "outoftime") %>%
      count(player_name) %>%
      rename(przegrane_czas = n)
    
    przegrane_czas_mat <- przegrane %>%
      filter(how_ended == "mate") %>%
      count(player_name) %>%
      rename(przegrane_mat = n)
    
    przegrane_czas_poddanie <- przegrane %>%
      filter(how_ended == "resign") %>%
      count(player_name) %>%
      rename(przegrane_poddanie = n)
    
    
    przegrane_wszystkie <- przegrane_ilosci %>%
      left_join(przegrane_czas_ilosci, by="player_name") %>%
      left_join(przegrane_czas_mat, by="player_name") %>%
      left_join(przegrane_czas_poddanie, by="player_name") %>%
      replace(is.na(.), 0)
    
    przegrane_wszystkie <- przegrane_wszystkie %>%
      mutate(przegrane_czas_procent = round(przegrane_czas/ilosc_przegrane,2)) %>%
      mutate(przegrane_mat_procent = round(przegrane_mat/ilosc_przegrane,2)) %>%
      mutate(przegrane_poddanie_procent = round(przegrane_poddanie/ilosc_przegrane,2))
    
    
    # dane do wykresów i wykresy dla wygranych
    
    wygrane_max <- max(wygrane_wszystkie %>%
                         select(wygrane_czas_procent, wygrane_mat_procent, wygrane_poddanie_procent))
    
    
    wykres_wygrane_czas <- ggplot(data = wygrane_wszystkie, aes(x=player_name, y=wygrane_czas_procent, fill=player_name)) +
      geom_col() +
      xlab("") + 
      ylab("") +
      ylim(0,wygrane_max) +
      ggtitle("czas") +
      theme_bw()+
      scale_fill_manual(values = colors) +
      scale_x_discrete(breaks = NULL)+
      scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.2), labels = c("0","20","40","60","80","100"))+
      theme(axis.text=element_text(size=osie, colour = text_color),
            plot.title = element_text(size=podtytul, colour = text_color, hjust = 0.5),
            legend.position="none",
            panel.border = element_blank(),
            panel.grid = element_line(colour = "#4e5c68"),
            panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA))
    
    
    wykres_wygrane_mat <- ggplot(data = wygrane_wszystkie, aes(x=player_name, y=wygrane_mat_procent, fill=player_name)) +
      geom_col() +
      xlab("") + 
      ylab("") +
      ylim(0,wygrane_max) +
      ggtitle("mat") +
      theme_bw() +
      scale_fill_manual(values = colors) +
      theme(legend.position="none") +
      scale_x_discrete(breaks = NULL)+
      scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.2), labels = c("0","20","40","60","80","100"))+
      theme(axis.text=element_text(size=osie, colour = text_color),
            plot.title = element_text(size=podtytul, colour = text_color, hjust = 0.5),
            panel.border = element_blank(),
            panel.grid = element_line(colour = "#4e5c68"),
            panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA))
    
    wykres_wygrane_poddanie <- ggplot(data = wygrane_wszystkie, aes(x=player_name, y=wygrane_poddanie_procent, fill=player_name)) +
      geom_col() +
      xlab("") + 
      ylab("") +
      ylim(0,wygrane_max) +
      ggtitle("poddanie") +
      theme_bw() +
      scale_fill_manual(values = colors) +
      theme(legend.position="none") +
      scale_x_discrete(breaks = NULL)+
      scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.2), labels = c("0","20","40","60","80","100"))+
      theme(axis.text=element_text(size=osie, colour = text_color),
            plot.title = element_text(size=podtytul, colour = text_color, hjust = 0.5),
            panel.border = element_blank(),
            panel.grid = element_line(colour = "#4e5c68"),
            panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA))
    
    
    win <- grid.arrange(wykres_wygrane_czas, wykres_wygrane_mat, wykres_wygrane_poddanie, ncol=3, top = grid::textGrob("Procent wygranych ze względu na:\n", x = 0.049, hjust = 0, gp = gpar(col = text_color, fontsize = tytul)))
    
    
    # dane do wykresów i wykresy dla przegranych
    
    przegrane_max <- max(przegrane_wszystkie %>%
                           select(przegrane_czas_procent, przegrane_mat_procent, przegrane_poddanie_procent))
    
    wykres_przegrane_czas <- ggplot(data = przegrane_wszystkie, aes(x=player_name, y=przegrane_czas_procent, fill=player_name)) +
      geom_col() +
      xlab("") + 
      ylab("") +
      ylim(0,przegrane_max) +
      ggtitle("czas") +
      theme_bw() +
      scale_fill_manual(values = colors) +
      theme(legend.position="none") +
      scale_x_discrete(breaks = NULL) +
      scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.2), labels = c("0","20","40","60","80","100"))+
      theme(axis.text=element_text(size=osie, colour = text_color),
            plot.title = element_text(size=podtytul, colour = text_color, hjust = 0.5),
            panel.border = element_blank(),
            panel.grid = element_line(colour = "#4e5c68"),
            panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA))
    
    wykres_przegrane_mat <- ggplot(data = przegrane_wszystkie, aes(x=player_name, y=przegrane_mat_procent, fill=player_name)) +
      geom_col() +
      xlab("") + 
      ylab("") +
      ylim(0,przegrane_max) +
      ggtitle("mat") +
      theme_bw() +
      scale_fill_manual(values = colors) +
      theme(legend.position="none") +
      scale_x_discrete(breaks = NULL)+
      scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.2), labels = c("0","20","40","60","80","100"))+
      theme(axis.text=element_text(size=osie, colour = text_color),
            plot.title = element_text(size=podtytul, colour = text_color, hjust = 0.5),
            panel.border = element_blank(),
            panel.grid = element_line(colour = "#4e5c68"),
            panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA))
    
    wykres_przegrane_poddanie <- ggplot(data = przegrane_wszystkie, aes(x=player_name, y=przegrane_poddanie_procent, fill=player_name)) +
      geom_col() +
      xlab("") + 
      ylab("") +
      ylim(0,przegrane_max) +
      ggtitle("poddanie") +
      theme_bw() +
      scale_fill_manual(values = colors) +
      theme(legend.position="none") +
      scale_x_discrete(breaks = NULL)+
      scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.2), labels = c("0","20","40","60","80","100"))+
      theme(axis.text=element_text(size=osie, colour = text_color),
            plot.title = element_text(size=podtytul, colour = text_color, hjust = 0.5),
            panel.border = element_blank(),
            panel.grid = element_line(colour = "#4e5c68"),
            panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA))
    
    
    lose <- grid.arrange(wykres_przegrane_czas, wykres_przegrane_mat, wykres_przegrane_poddanie, ncol=3, top = grid::textGrob("Procent przegranych ze względu na:\n", x = 0.049, hjust = 0, gp = gpar(col = text_color, fontsize = tytul)))
    
    g <- grid.arrange(win, lose, widths = c(4, 1, 4), layout_matrix = rbind(c(1, NA, 3)))
    
    
    cowplot::ggdraw(g) + 
      # same plot.background should be in the theme of p1 and p2 as mentioned above
      theme(plot.background = element_rect(fill=background_color, color = NA)) 
    }
    else{
       c<- plot(1, type = "n", xlab = "",
           ylab = "", xlim = c(0, 5), 
           ylim = c(0, 5))
       z <- plot(1, type = "n", xlab = "",
                 ylab = "", xlim = c(0, 5), 
                 ylim = c(0, 5), main = "poddanie")
       v <- plot(1, type = "n", xlab = "",
                 ylab = "", xlim = c(0, 5), 
                 ylim = c(0, 5))
       
       
       lose <- grid.arrange(c, z, v, ncol=3, top = grid::textGrob("Procent przegranych ze względu na:\n", x = 0.049, hjust = 0, gp = gpar(col = text_color, fontsize = tytul)))
       win <- grid.arrange(c, z, v, ncol=3, top = grid::textGrob("Procent wygranych ze względu na:\n", x = 0.049, hjust = 0, gp = gpar(col = text_color, fontsize = tytul)))
       g <- grid.arrange(win, lose, widths = c(4, 1, 4), layout_matrix = rbind(c(1, NA, 3)))
       cowplot::ggdraw(g) + 
         # same plot.background should be in the theme of p1 and p2 as mentioned above
         theme(plot.background = element_rect(fill=background_color, color = NA))
      }
    
    
    
  }
    
    
                                  
  )
  output$dzienne <- renderPlot({daily_data <- read.csv("daily_data.csv")
                               
                               colors <- c("nizwant" = "#8a0404","wiktorw123" = "#024f1a","LauPaSat" = "#070982")
                               
                              
                               
                               daily_data$date <- as.Date(daily_data$date, '%Y-%m-%d')
                               
                               daily_data %>% 

                                filter(player_name %in% input$uzytkownik,
                                       date >= input$date[1], date <= input$date[2],
                                       mode == input$typ_gry) %>% 
                                 ggplot(aes(x = date, y = games_per_day, group = player_name, color = player_name))+
                                 geom_line(linewidth = 2.5)+
                                 theme_bw()+
                                 theme(legend.position = "none",
                                       axis.text=element_text(size=axes.size),
                                       axis.title=element_text(size=label.size,face="bold"),
                                       title = element_text(size = title.size, face = "bold"))+
                                 labs(title = "Liczba gier dziennie")+
                                 ylab("Liczba gier")+
                                 scale_color_manual(values = colors)+
                                scale_y_continuous(limits = c(0,12), expand = c(0,0))+
                                 scale_x_date(name = "Data", date_breaks = "4 days", date_labels = "%d-%m", expand = c(0,0), limits = c(as.Date("2022-12-07","%Y-%m-%d"),as.Date("2023-01-21","%Y-%m-%d")))+
                                 theme(axis.text=element_text(size=osie, colour = text_color),
                                       plot.title = element_text(size=tytul, colour = text_color, hjust = 0.5),
                                       panel.border = element_blank(),
                                       legend.position="none",
                                       panel.grid = element_line(colour = "#4e5c68"),
                                       axis.title = element_text(size = podtytul, color = text_color, hjust = 0.5),
                                       panel.background = element_rect(fill=background_color),
                                       plot.background = element_rect(fill=background_color, color=NA))
                               
                               
                                  
  })

    

}

