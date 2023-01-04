library(ggplot2)
library(dplyr)
library(rayrender)

punkty <- seq(from = 0, to = 2, by = 0.01)
wartosci <- rep(1, length(punkty))
klucz <- rep("drzewo", length(punkty))
tabela <- cbind(punkty,wartosci, klucz)
tabela <- as.data.frame(tabela)



for (i in seq(from = 0.01, to = 1, by = 0.01)) {
  punkty <- seq(from = i, to = 2 - i, by = 0.01)
  wartosci <- rep(1 + i, length(punkty))
  klucz <- rep("drzewo", length(punkty))
  tabela2 <- as.data.frame(cbind(punkty,wartosci, klucz))
  tabela <- rbind(tabela, tabela2)
  
}
for(i in seq(from = 0.03, to = 0.25, by = 0.01)){
  punkty <- seq(from = 0.90, to = 1.1, by = 0.01)
  wartosci <- rep(1-i, length(punkty))
  klucz <- rep("korzen", length(punkty))
  tabela2 <- as.data.frame(cbind(punkty,wartosci, klucz))
  tabela <- rbind(tabela,tabela2)
}

p <- tabela %>% 
  ggplot(aes(x = punkty, y = wartosci,  color = klucz))+
  geom_point(size = 4)+
  theme(legend.text = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = '#232633'),
        panel.grid.major.y = element_line(size = 0, color = '#232633'),
        panel.grid.minor.y = element_line(size = 0, color = '#232633'),
        panel.grid.major.x = element_line(size = 0, color = '#232633'),
        panel.grid.minor.x = element_line(size = 0, color = '#232633'),
        plot.background = element_rect(fill = "#232633"),
        legend.position = "none")+
  scale_color_manual(values = c("green", "brown"))+
  labs(x = "",
       y = "")
p
obrazek = tempfile()
png(filename=obrazek,height=1600,width=1600, bg = "#232633")
p
dev.off()




obrazek = png::readPNG(obrazek)

generate_cornell() %>%
  add_object(cube(x=100,y=130/2,z=200,xwidth = 130,ywidth=130,zwidth = 130,
                  material=diffuse(checkercolor="yellow", checkerperiod = 30),angle=c(0,10,0))) %>%
  add_object(cube(x=100,y=130 + 30/2,z=200,xwidth = 30,ywidth=30,zwidth = 30,
                  material=diffuse(checkercolor="red", checkerperiod = 3),angle=c(0,10,0))) %>%
  add_object(cube(x=475,y=130/2,z=300,xwidth = 100,ywidth=130,zwidth = 75,
                  material=diffuse(checkercolor="green", checkerperiod = 20),angle=c(0,-10,0))) %>%
  add_object(cube(x=450,y=50/2,z=100,xwidth = 120,ywidth=50,zwidth = 120,
                  material=diffuse(checkercolor="blue", checkerperiod = 40),angle=c(0,-10,0))) %>%
  add_object(cube(x=270,y=170/2,z=200,xwidth = 150,ywidth=170,zwidth = 150,
                  material=diffuse(checkercolor="#eb71b9", checkerperiod = 40),angle=c(0,0,0))) %>%
  add_object(yz_rect(x=555/2,y=300,z=555-0.01,zwidth=400,ywidth=400,
                     material = diffuse(image_texture = obrazek),angle=c(90,90,0))) %>%
  render_scene(lookfrom=c(278,278,-800),lookat = c(278,278,0), aperture=0, fov=40,  samples = 1000,
               ambient_light=T, parallel=TRUE, width=800, height=800, clamp_value = 5)
