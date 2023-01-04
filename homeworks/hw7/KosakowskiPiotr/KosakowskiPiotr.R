x <- matrix(0.5, 65, 51)


n<-3
m<-0
r <- 5

x[n, 26] <- 0

for(i in 1:5) {
  
  for(j in 1:r) {
    n <- n+1
    m <- m+1
    x[n, seq(26-m,26+m)] <- 0
  }
  
  
  m <- 1
  r <- r+3
  
}

x[n, 26] <- 0



x[seq(n+1,n+5), seq(23, 29)] <- 1


x <- t(x)
x <- x[,c(65:1)]

image(x, col = hcl.colors(5, palette = "Green-Brown"))
