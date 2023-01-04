library(plotly)
library(dplyr)
radius <- 1 
height <- 2 
n <- 100 

angles <- seq(0, 2*pi, length.out = n)
anglesxy <- c((angles+pi/2), angles, (angles+pi), (angles+3*pi/2))

htemp <- seq(2,0, length.out = n)
radiusvec <- 0.5*htemp
x1 <- radiusvec * cos(anglesxy)
y1 <- radiusvec * sin(anglesxy)
z1 <- height * angles / (2*pi)

dftree <- data.frame(x=x1,y=y1,z=z1)

anglesxy1 <- c((angles+pi/2), angles, (angles+pi), (angles+3*pi/2))

htemp <- seq(1.7,0, length.out = n)
radiusvec <- 0.4*htemp
x2 <- radiusvec * cos(anglesxy1)
y2 <- radiusvec * sin(anglesxy1)
z2 <- height * angles / (2*pi)

dftree1 <- data.frame(x=x2,y=y2,z=z2)


anglespien <- seq(0, 10*pi, length.out = 2*n)
anglespienxy <- c(anglespien, anglespien+pi)

radiuspien <- rep(0.2, 2*n)
xpien <- radiuspien * cos(anglespien)
ypien <- radiuspien * sin(anglespien)
zpien <- angles/(10*pi)

dfpien <- data.frame(x=xpien,y=ypien,z=zpien-0.3)

xsnieg <- runif(100000,-5,5)
ysnieg <- runif(100000,-5,5)
zsnieg <- runif(100000,0,0.05)
dfsnieg <- data.frame(x=xsnieg,y=ysnieg,z=zsnieg-0.3)

xsnieg2 <- runif(1000, -3,5)
ysnieg2 <- runif(1000,-5,-1)
zsnieg2 <- runif(1000,0.2,5)
dfsnieg2 <- data.frame(x=xsnieg2,y=ysnieg2,z=zsnieg2)

xsnieg3 <- runif(1000, 1.5,5)
ysnieg3 <- runif(1000,-1.3,5)
zsnieg3 <- runif(1000,0.2,5)
dfsnieg3 <- data.frame(x=xsnieg3,y=ysnieg3,z=zsnieg3)



angleslan <- seq(0, 8*pi, length.out = n*n)
anglesxylan <- angleslan+pi/4
angleslan <- seq(0, n*pi, length.out= n*n)


htemplan <- seq(2,0, length.out = n*n)
radiusveclan <- 0.5*htemplan
xlan1 <- c(radiusveclan*1.1 * cos(anglesxylan) + cos(angleslan)/20,
           radiusveclan *cos(anglesxylan) + cos(angleslan+pi)/200,
           radiusveclan*1.1 * cos(anglesxylan) + cos(angleslan)/100,
           radiusveclan*1.1 * cos(anglesxylan) + cos(angleslan)/150)
ylan1 <- radiusveclan *1.1* sin(anglesxylan)
zlan1 <- c((height-0.1) * 1.1*angleslan / ((n+1)*pi) + sin(angleslan)/20,
           (height-0.1) *1.1* angleslan / ((n+1)*pi) + sin(angleslan)/20,
           (height-0.1) * 1.1*angleslan / ((n+1)*pi) + sin(angleslan)/20,
           (height-0.1) * 1.1*angleslan / ((n+1)*pi) + sin(angleslan)/20)


dflan1 <- data.frame(x = xlan1, y = ylan1, z = zlan1)

starx <- rep(0,200)
stary1 <- runif(2*n, -0.1902111, 0.190)
stary2 <- runif(2*n, 0, 0.11755)
stary3 <- runif(2*n, -0.11755, 0.190211)
stary4 <- runif(2*n, -0.1902111, 0.11755)
stary5 <- runif(2*n, -0.11755, 0)

starz1 <- rep(0.0618, 200)
starz2 <- (stary2/0.11755)*(-0.3618)+0.2
starz3 <- (stary3 + 0.1175) * 0.2236/0.30771-0.1618
starz4 <- (stary4 + 0.190211)/0.307761*(-0.2236)+0.0618
starz5 <- (stary5 + 0.1175)*0.3618/0.1175-0.1618

stary <- c(stary1,stary2,stary3,stary4,stary5)
starz <- c(starz1+2.15,starz2+2.15,starz3+2.15,starz4+2.15,starz5+2.15)


stardf <- data.frame(x=starx, y = stary, z = starz)

fig <- plot_ly(mode="markers")
fig <- fig %>%
  add_trace(
    x = dflan1$x,
    y = dflan1$y,
    z = dflan1$z,
    marker = list(size = 1,
                  color = "#fcba03")
  )
fig <- fig %>%
  add_trace(
    x = dftree$x,
    y = dftree$y,
    z = dftree$z,
    marker = list(size = 14,
                  color = '#399e40')
  )
fig <- fig %>%
  add_trace(
    x = dftree1$x,
    y = dftree1$y,
    z = dftree1$z,
    marker = list(size = 11,
                  color = '#08541f')
  )
fig <- fig %>%
  add_trace(
    x = stardf$x,
    y = stardf$y,
    z = stardf$z,
    marker = list(size = 10,
                  color = '#f5f242')
  )
fig <- fig %>%
  add_trace(
    x = dfpien$x,
    y = dfpien$y,
    z = dfpien$z,
    marker = list(size = 4,
                  color = '#5c0017')
  )
fig <- fig %>%
  add_trace(
    x = dfsnieg$x,
    y = dfsnieg$y,
    z = dfsnieg$z,
    marker = list(size = 4,
                  color = '#ffffff')
  )
fig <- fig %>%
  add_trace(
    x = dfsnieg2$x,
    y = dfsnieg2$y,
    z = dfsnieg2$z,
    marker = list(size = 2,
                  color = '#ffffff')
  )

fig <- fig %>%
  add_trace(
    x = dfsnieg3$x,
    y = dfsnieg3$y,
    z = dfsnieg3$z,
    marker = list(size = 2,
                  color = '#ffffff')
  )
sizelist <- list(size=0, color = '#191c5c')

a <- list(showgrid =FALSE,
          showticklabels = FALSE,
          showline=FALSE,
          tickfont = sizelist)

fig <- fig %>%
  layout(plot_bgcolor='#191c5c') %>%
  layout(paper_bgcolor='#191c5c') %>%
  layout(showlegend=FALSE) %>%
  layout(axis = a)
fig
