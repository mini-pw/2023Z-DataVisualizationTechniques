library(rgl)
library(magick)

colfunc <- colorRampPalette(c("#5ae717", "#389f0a", "#1f6200"))
# settings

LEVELS <- 1000
numPerLevel <- (1:LEVELS) 

# Create Cone 
createXs <- function(n) {
  t <- 1:n
  mul <-(2 * pi / n)
  cos(mul * t) * n
}

createYs <- function(n){
  t <- 1:n
  mul <-(2 * pi / n)
  sin(mul * t) * n
}

createZs <- function(n){
  rep(LEVELS - n, n)
}

Zs <- unlist(sapply(numPerLevel, createZs))
Xs <- unlist(sapply(numPerLevel, createXs))
Ys <- unlist(sapply(numPerLevel, createYs))
colors <- rep(colfunc(LEVELS), times = numPerLevel)


t <- seq(0.1, 10*pi, 0.01)
const <- LEVELS / (10*pi)

chain1X <- cos(t) * (10*pi - t) * const * 1.02
chain1Y <- sin(t) * (10*pi - t) * const * 1.02
chain1Z <- t * const
chain1Colors <- rep("#c0c0c0", length(t))

chain2X <- cos(-t) * (10*pi - t) * const * 1.02
chain2Y <- sin(-t) * (10*pi - t) * const * 1.02
chain2Z <- t * const
chain2Colors <- rep("#035e7b", length(t))


# Plot cone / START PLOT
plot3d(
  x = Xs,
  y = Ys,
  z = Zs,
  col = colors,
  axes = FALSE,
  xlab=NULL, ylab=NULL, zlab=NULL
)

# Black background
rgl.bg(color = "black")

# Plot chains
rgl.points(
  x = c(chain1X, chain2X),
  y = c(chain1Y, chain2Y),
  z = c(chain1Z, chain2Z),
  col = c(chain1Colors, chain2Colors),
  size = 8
)

# Create vector of samples from linear distribution
m <- length(t)
better_t <- as.integer(
  m - sqrt(1:length(t) / m) * m
)
# hist(better_t)

# Random point for bulbs
rand1 <- sample(better_t, 75)
rand2 <- sample(better_t + m, 75)
rand <- c(rand1, rand2)

# Create and plot bulbs
rgl.spheres(
  x = c(chain1X, chain2X)[rand],
  y = c(chain1Y, chain2Y)[rand],
  z = c(chain1Z, chain2Z)[rand],
  col = rainbow(150),
  r = 50
)

# Test play
play3d(spin3d(axis = c(0, 0, 1), rpm = 12), duration = 5)

# Save
movie3d(
   movie="Drzewko",
   spin3d(axis = c(0, 0, 1), rpm = 12),
   duration = 5,
   dir = getwd(),
   type = "gif",
   clean = TRUE
)
