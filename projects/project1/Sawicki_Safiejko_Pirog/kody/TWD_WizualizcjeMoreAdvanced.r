library(dplyr)
library(ggplot2)
source("themes.r")
df <- read.csv("alldane.csv", sep = ";", encoding = "UTF-8")

top_10 <- c("DeAnna PRICE", "Betty HEIDLER", "Brooke ANDERSEN",
            "Tatyana LYSENKO", "Janee' KASSANAVOID", "Gwen BERRY", 
            "Zheng WANG", "Camryn ROGERS", "Wenxiu ZHANG",
            "Oksana MIANKOVA", "Gulfiya KHANAFEYEVA")

df$Competitor <- ifelse(df$Competitor == "Anita WŁODARCZYK", "Anita Włodarczyk", ifelse(df$Competitor %in% top_10, "Top 10 contestants", "All contestants"))

ggplot(df, aes(x = Mark, y = factor(Competitor, level = rev(c("Anita Włodarczyk", "Top 10 contestants", "All contestants"))), fill = Competitor)) +
  geom_violin()+
  geom_boxplot(width = .2) +
  labs(title = "Women Hammer Throw results") +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(60, 85))+
  scale_y_discrete(expand = c(0,0), guide = guide_axis(title = element_blank()),
                   labels = c("Anita Włodarczyk" = "Anita\nWłodarczyk",
                              "Top 10 contestants" = "Top 10\ncontestants",
                              "All contestants" = "All\ncontetants"))+
  theme(
        axis.text.x = element_text(size = 14),
        
        panel.grid.major.y = element_blank(),
        panel.background =element_rect(fill="white"),
        panel.grid.major = element_line(colour = "lightgray"))+
  scale_fill_manual(values = rev(c("#fae399","#f5c713","#b2a0a0")))+
  theme_dark_blue() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 30, face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5, size = 28,colour ="#f8b31a"),
        axis.text.x= element_text(size = 20, face = "bold"),
        axis.text=element_text(size=14, colour = "white"),
        axis.title.x = element_text(size = 26))
