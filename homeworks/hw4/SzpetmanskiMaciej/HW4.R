library("plotly")
library("ggplot2")
library("dplyr")

#Wczytujemy nasz plik
homes <- read.csv("Properties_philly_Kraggle_v2.csv")

a <- as.data.frame(homes$taxAssessment)
a$`homes$taxAssessment` <-gsub("\\..*","",as.character(a$`homes$taxAssessment`))

homes$taxAssessment <- as.numeric(gsub(",","",a$`homes$taxAssessment`))

#Tworzymy wykres
my_plot <- plot_ly(
  data = homes,
  x = ~taxAssessment,
  frame = ~PropType,
  type = "box",
  marker=list(color="black")
  
) 

my_plot <- my_plot %>%
            layout(title = "Average tax assessment depending on the type of property",
                   xaxis = list(title = "tax assessment"),
                   updatemenus = list(
                     list(
                       x = 1, y = 0.9,
                       buttons = list(
                         list(method = "restyle",
                              args = list("marker.color","black"),
                              label = "Black"),
                         list(method = "restyle",
                              args = list("marker.color","green"),
                              label = "Red"),
                         list(method = "restyle",
                              args = list("marker.color","purple"),
                              label = "Purple"),
                         list(method = "restyle",
                              args = list("marker.color","red"),
                              label = "Blue"))),
                     list(
                       x = 1, y = 1,
                       buttons = list(
                         list(method = "restyle",
                              args = list("type", "box"),
                              label = "Boxplot"),
                         list(method = "restyle",
                              args = list("type", "violin"),
                              label = "Violinplot"))
                     )
                   )
            ) %>% animation_opts(1000) %>%
  animation_button(x = 0.00, y = 0.05, label = "Animation") %>%
            animation_slider(currentvalue = list(prefix = "Property type"))


#Odpalamy wykres
my_plot
