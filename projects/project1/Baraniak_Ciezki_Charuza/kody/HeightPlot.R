library(dplyr)
library(ggplot2)
library(forcats)

athlete_events %>%
  filter(!is.na(Height) & !is.na(Medal) & Sex == "M") %>% 
  mutate(Height_interval = case_when(
    Height >= 127 & Height < 140 ~ "127-140",
    Height >= 140 & Height < 150 ~ "140-150",
    Height >= 150 & Height < 160 ~ "150-160",
    Height >= 160 & Height < 170 ~ "160-170",
    Height >= 170 & Height < 180 ~ "170-180",
    Height >= 180 & Height < 190 ~ "180-190",
    Height >= 190 & Height < 200 ~ "190-200",
    Height >= 200 & Height < 210 ~ "200-210",
    Height >= 210 & Height < 220 ~ "210-220",
    Height >= 220 & Height < Inf ~ "220-227",
    TRUE ~ "NA"
    ),
      Medal = case_when(
        Medal == "Bronze" ~ "Brązowy",
        Medal == "Silver" ~ "Srebrny",
        Medal == "Gold" ~ "Złoty"
    )) %>%
  group_by(Height_interval, Medal) %>% 
  summarise(Number = n()) -> df_men

athlete_events %>%
  filter(!is.na(Height) & !is.na(Medal) & Sex == "F") %>% 
  mutate(
    Height_interval = case_when(
    Height >= 127 & Height < 140 ~ "127-140",
    Height >= 140 & Height < 150 ~ "140-150",
    Height >= 150 & Height < 160 ~ "150-160",
    Height >= 160 & Height < 170 ~ "160-170",
    Height >= 170 & Height < 180 ~ "170-180",
    Height >= 180 & Height < 190 ~ "180-190",
    Height >= 190 & Height < 200 ~ "190-200",
    Height >= 200 & Height < 210 ~ "200-210",
    Height >= 210 & Height < 220 ~ "210-220",
    Height >= 220 & Height < Inf ~ "220-227",
    TRUE ~ "NA"
  ),
    Medal = case_when(
    Medal == "Bronze" ~ "Brązowy",
    Medal == "Silver" ~ "Srebrny",
    Medal == "Gold" ~ "Złoty"
  )) %>% 
  group_by(Height_interval, Medal) %>% 
  summarise(Number = n()) -> df_women
  

  df_men %>% 
  ggplot(aes(x = Height_interval, y = Number, 
             fill = factor(Medal, levels = c("Brązowy", "Srebrny", "Złoty")),
             label = ifelse(Number > 200, Number, ""))) +
  geom_col() + 
  labs(title = "Liczba zdobytych medali dla poszczególnych grup 
       wzrostowych wśród mężczyzn",
       x = "Przedział wzrostowy [cm]",
       y = "Uzyskane medale") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  theme_minimal() +
  theme(text = element_text(
    size = 12,
    family = "emblemaone")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  guides(fill = guide_legend(title = "Zdobyty Medal")) +
  scale_fill_manual(values = c("#cd7f32", "#d6d6d6", "#ffdb1a"))
  
  
  
  df_women %>% 
    ggplot(aes(x = Height_interval, y = Number, 
               fill = factor(Medal, levels = c("Brązowy", "Srebrny", "Złoty")),
               label = ifelse(Number > 200, Number, ""))) +
    geom_col() + 
    labs(title = "Liczba zdobytych medali dla poszczególnych grup 
         wzrostowych wśród kobiet",
         x = "Przedział wzrostowy [cm]",
         y = "Uzyskane medale") +
    geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    theme_minimal() +
    theme(plot.title = element_text()) +
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(fill = guide_legend(title = "Zdobyty Medal")) +
    scale_fill_manual(values = c("#cd7f32", "#d6d6d6", "#ffdb1a"))

################################ library #######################################
library(tidyverse)
library(viridis)

# Create dataset
data <- data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value1=sample( seq(10,100), 60, replace=T),
  value2=sample( seq(10,100), 60, replace=T),
  value3=sample( seq(10,100), 60, replace=T)
)

# Transform data in a tidy format (long format)
data <- datas %>% gather(key = "observation", value="value", -c(1,2)) 


data <- df_men
# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
nObsType <- nlevels(as.factor(data$Height_interval))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$Height_interval)*nObsType, ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$Medal <- rep(levels(data$Medal), each=empty_bar*nObsType )
data <- rbind(data, to_add)
data <- data %>% arrange(Medal, Height_interval)
data$id <- rep( seq(1, nrow(data)/nObsType), each=nObsType)

# Get the name and the y position of each label
label_data <- data %>% group_by(id, Height_interval) %>% summarize(tot=sum(Number))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(Height_interval) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p <- ggplot(data) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=Number, fill=Medal), stat="identity", alpha=0.5) +
  scale_fill_viridis(discrete=TRUE) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 150, xend = start, yend = 150), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data$id),5), y = c(0, 50, 100, 150, 200), label = c("0", "50", "100", "150", "200") , color="grey", size=6 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-150,max(label_data$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+10, label=Height_interval, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=Height_interval), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)


# Save at png
#ggsave(p, file="output.png", width=10, height=10)
p
