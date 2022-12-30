library(gganimate)
library(dplyr)
library(ggthemes)
library(ggimage)

#tworzymy choinke
choinka <- NULL
x <- -1
for (i in 1:16) {
  if(x == 9){
    x = -1
  }
  x <- x+1
  xowy <- c(x, x)
  y <- paste(rep(xowy, each= i), collapse = " ")
  choinka = rbind(choinka,data.frame(row = i,
                           val = y)) 
}
#ozdoby
bombki<- data.frame(x = c(0, 0.1, -0.1, -0.05, 0.05, -0.15, -0.065, 0.065, 0.15),
                y = c(-5.2, -5.2, -5.2, -3.2, -3.2, -7.2, -7.2, -7.2, -7.2),
                image = sample("zrzutblank.png"))
gwiazda <- data.frame(x = 0,
                y = 0,
                image = "wolfram-alpha-logo.png")


wektor <- rep(1:100, each = 100)
snieg_animacja  <- data.frame(sniegX= runif(10000, min=-0.25, max=0.25), 
                              sniegY = runif(10000, min=-9, max=1),
                              stan = wektor)

#statyczny snieg wykorzystany tylko do zrobienia obrazka
snieg_static <- data.frame(sniegX= runif(100, min=-0.25, max=0.25), 
                           sniegY = runif(100, min=-8, max=1))


choineczka <-ggplot(choinka)+
  geom_text(aes(label = val, y = -row*0.5 , x = 0, group = seq_along(row)),
            color = '#14A230',
            size = 6,
            fontface = 'bold')+
  ylim(-9, 1) +
  xlim(-0.25, 0.25)+
  theme_tufte()+
  geom_point(snieg_animacja, mapping=aes(x=sniegX,y=sniegY),color = '#4BD0FC', shape = 8, size =3)+
  geom_image(data = bombki, aes(x=x, y=y, image=image))+
  geom_image(data = gwiazda, aes(x=x, y=y, image=image), size =0.08)+
  theme(aspect.ratio=1,
        plot.background = element_rect(fill = '#E7F9FF'),
        axis.title = element_blank(),
        axis.text = element_blank())+
  transition_states(snieg_animacja$stan, transition_length = 0.9, state_length = 0.9)



animate(choineczka)
anim_save("choinkaG.gif", choineczka)


