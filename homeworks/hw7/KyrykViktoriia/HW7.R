
plot.new()
plot.window(xlim=c(-2,10), ylim=c(0,15))
polygon(x = c(0, 0, 1, 1),                           # X-Coordinates of polygon
        y = c(1, 2, 2, 1),                             # Y-Coordinates of polygon
        col = "saddlebrown")  
                                       # Set seed for reproducibilit
## Draw a polygon filled with lines
xx = c(-2,-0.5,1.5,3)
yy = c(2,4,4,2)
## polygon is filled with 20 lines per inch, green in colour.
polygon(xx,yy, col="green", lty = 1, lwd = 1, border="#31A354")
points(x = c(-0.5,0.6,1.7),y = c(2.6,3.4,2.7),col=c("darkorchid2","cyan2","deeppink"),cex=2.5,pch=19)

x2 = c(-1.5,-0.2,1.2,2.5)
y2 = c(4,6,6,4)
polygon(x2,y2, col="green",angle=10, border="#31A354")
points(x = c(-0.5,0.5,1.5),y = c(4.6,5.2,4.6),col=c("cyan2","deeppink","darkorchid2"),cex=2.3,pch=19)

x3 = c(-1,0,0.9,2)
y3 = c(6,8,8,6)
polygon(x3,y3, col="green", border="#31A354")
points(x = c(-0.1,0.8),y = c(6.55,7.3),col=c("darkorchid2","deeppink"),cex=2,pch=19)

x4 = c(-0.7,0.2,0.5,1.5)
y4 = c(8,9.5,9.5,8)
polygon(x4,y4, col="green", border="#31A354")
points(x = c(0,0.6),y = c(8.44,8.85),col=c("cyan2","deeppink"),cex=1.7,pch=19)


points(0.35,9.7,pch=8,cex=2,col="gold",lwd=3)

