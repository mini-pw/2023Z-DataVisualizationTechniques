####################################
###           LIBRARIES          ###
####################################

# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("gganimate")
# install.packages("Rcpp")
# install.packages("gifski")
# install.packages("av")

library(dplyr)
library(ggplot2)
library(gganimate)
library(Rcpp)
library(gifski)
library(av)

####################################
###           CONSTANTS          ###
####################################

STEP <- 0.1

ROOT_OBSERVATIONS_NUMBER <- 10

ROOT_X_DOMAIN <- seq(0, 4, STEP)

ROOT_LEFT_Y_DOMAIN <- seq(0, 1, STEP)

ROOT_RIGHT_Y_DOMAIN <- seq(0, -1, -STEP)

BRANCHES_OBSERVATIONS_NUMBER <- 20

BRANCHES_X_DOMAIN_1st <- seq(4, 8, STEP)
BRANCHES_X_DOMAIN_2nd <- seq(8, 12, STEP)
BRANCHES_X_DOMAIN_3rd <- seq(12, 16, STEP)
BRANCHES_X_DOMAIN_4th <- seq(16, 18, STEP)

# BRANCHES_Y_DOMAIN parts will be limited through corresponding functions

BRANCHES_LEFT_Y_DOMAIN_1st_UPPER_LIMIT <- function(arg) {
  return(1 / 28 * (276 - 27 * arg))
}
BRANCHES_LEFT_Y_DOMAIN_2nd_UPPER_LIMIT <- function(arg) {
  return(1 / 28 * (288 - 21 * arg))
}
BRANCHES_LEFT_Y_DOMAIN_3rd_UPPER_LIMIT <- function(arg) {
  return(9 - 15 / 28 * arg)
}
BRANCHES_LEFT_Y_DOMAIN_4th_UPPER_LIMIT <- function(arg) {
  return(1 / 7 * (54 - 3 * arg))
}

BRANCHES_RIGHT_Y_DOMAIN_1st_LOWER_LIMIT <- function(arg) {
  return(-BRANCHES_LEFT_Y_DOMAIN_1st_UPPER_LIMIT(arg))
}
BRANCHES_RIGHT_Y_DOMAIN_2nd_LOWER_LIMIT <- function(arg) {
  return(-BRANCHES_LEFT_Y_DOMAIN_2nd_UPPER_LIMIT(arg))
}
BRANCHES_RIGHT_Y_DOMAIN_3rd_LOWER_LIMIT <- function(arg) {
  return(-BRANCHES_LEFT_Y_DOMAIN_3rd_UPPER_LIMIT(arg))
}
BRANCHES_RIGHT_Y_DOMAIN_4th_LOWER_LIMIT <- function(arg) {
  return(-BRANCHES_LEFT_Y_DOMAIN_4th_UPPER_LIMIT(arg))
}

####################################
###         CALCULATIONS         ###
####################################

###             ROOT L           ###

ROOT_LEFT_MATRIX <-
  matrix(nrow = length(ROOT_X_DOMAIN), ncol = ROOT_OBSERVATIONS_NUMBER)

for (i in 1:length(ROOT_X_DOMAIN)) {
  ROOT_LEFT_MATRIX[i, ] = sample(ROOT_LEFT_Y_DOMAIN, ROOT_OBSERVATIONS_NUMBER, replace = T)
}

ROOT_LEFT_DF <-
  matrix(nrow = length(ROOT_X_DOMAIN) * ROOT_OBSERVATIONS_NUMBER,
         ncol = 2)

k = 1
for (i in 1:length(ROOT_X_DOMAIN)) {
  for (j in 1:ROOT_OBSERVATIONS_NUMBER) {
    ROOT_LEFT_DF[k , ] = c(ROOT_X_DOMAIN[i], ROOT_LEFT_MATRIX[i, j])
    k = k + 1
    
  }
}

ROOT_LEFT_DF <-
  data.frame("X" = ROOT_LEFT_DF[, 1], "Y" = ROOT_LEFT_DF[, 2])

###             ROOT R           ###

ROOT_RIGHT_MATRIX <-
  matrix(nrow = length(ROOT_X_DOMAIN), ncol = ROOT_OBSERVATIONS_NUMBER)

for (i in 1:length(ROOT_X_DOMAIN)) {
  ROOT_RIGHT_MATRIX[i, ] = sample(ROOT_RIGHT_Y_DOMAIN, ROOT_OBSERVATIONS_NUMBER, replace = T)
}

ROOT_RIGHT_DF <-
  matrix(nrow = length(ROOT_X_DOMAIN) * ROOT_OBSERVATIONS_NUMBER,
         ncol = 2)

k = 1
for (i in 1:length(ROOT_X_DOMAIN)) {
  for (j in 1:ROOT_OBSERVATIONS_NUMBER) {
    ROOT_RIGHT_DF[k , ] = c(ROOT_X_DOMAIN[i], ROOT_RIGHT_MATRIX[i, j])
    k = k + 1
    
  }
}

ROOT_RIGHT_DF <-
  data.frame("X" = ROOT_RIGHT_DF[, 1], "Y" = ROOT_RIGHT_DF[, 2])

###          BRANCHES L          ###

BRANCHES_LEFT_MATRIX <-
  matrix(
    nrow = length(BRANCHES_X_DOMAIN_1st) + length(BRANCHES_X_DOMAIN_2nd) + length(BRANCHES_X_DOMAIN_3rd) +
      length(BRANCHES_X_DOMAIN_4th),
    ncol = BRANCHES_OBSERVATIONS_NUMBER
  )

for (i in 1:length(BRANCHES_X_DOMAIN_1st)) {
  BRANCHES_LEFT_MATRIX[i, ] = sample(
    seq(
      0,
      BRANCHES_LEFT_Y_DOMAIN_1st_UPPER_LIMIT(BRANCHES_X_DOMAIN_1st[i]),
      STEP
    ),
    BRANCHES_OBSERVATIONS_NUMBER,
    replace = T
  )
}

for (i in 1:length(BRANCHES_X_DOMAIN_2nd)) {
  BRANCHES_LEFT_MATRIX[i + length(BRANCHES_X_DOMAIN_1st), ] = sample(
    seq(
      0,
      BRANCHES_LEFT_Y_DOMAIN_2nd_UPPER_LIMIT(BRANCHES_X_DOMAIN_2nd[i]),
      STEP
    ),
    BRANCHES_OBSERVATIONS_NUMBER,
    replace = T
  )
}

for (i in 1:length(BRANCHES_X_DOMAIN_3rd)) {
  BRANCHES_LEFT_MATRIX[i + length(BRANCHES_X_DOMAIN_1st) + length(BRANCHES_X_DOMAIN_2nd), ] = sample(
    seq(
      0,
      BRANCHES_LEFT_Y_DOMAIN_3rd_UPPER_LIMIT(BRANCHES_X_DOMAIN_3rd[i]),
      STEP
    ),
    BRANCHES_OBSERVATIONS_NUMBER,
    replace = T
  )
}

for (i in 1:length(BRANCHES_X_DOMAIN_4th)) {
  BRANCHES_LEFT_MATRIX[i + length(BRANCHES_X_DOMAIN_1st) + length(BRANCHES_X_DOMAIN_2nd) +
                         length(BRANCHES_X_DOMAIN_3rd), ] = sample(
                           seq(
                             0,
                             BRANCHES_LEFT_Y_DOMAIN_4th_UPPER_LIMIT(BRANCHES_X_DOMAIN_4th[i]),
                             STEP
                           ),
                           BRANCHES_OBSERVATIONS_NUMBER,
                           replace = T
                         )
}

BRANCHES_LEFT_DF <-
  matrix(
    nrow = (
      length(BRANCHES_X_DOMAIN_1st) + length(BRANCHES_X_DOMAIN_2nd) + length(BRANCHES_X_DOMAIN_3rd) +
        length(BRANCHES_X_DOMAIN_4th)
    ) * BRANCHES_OBSERVATIONS_NUMBER,
    ncol = 2
  )

k = 1
for (i in 1:length(BRANCHES_X_DOMAIN_1st)) {
  for (j in 1:BRANCHES_OBSERVATIONS_NUMBER) {
    BRANCHES_LEFT_DF[k , ] = c(BRANCHES_X_DOMAIN_1st[i], BRANCHES_LEFT_MATRIX[i, j])
    k = k + 1
  }
}

for (i in 1:length(BRANCHES_X_DOMAIN_2nd)) {
  for (j in 1:BRANCHES_OBSERVATIONS_NUMBER) {
    BRANCHES_LEFT_DF[k , ] = c(BRANCHES_X_DOMAIN_2nd[i], BRANCHES_LEFT_MATRIX[i + length(BRANCHES_X_DOMAIN_1st), j])
    k = k + 1
  }
}

for (i in 1:length(BRANCHES_X_DOMAIN_3rd)) {
  for (j in 1:BRANCHES_OBSERVATIONS_NUMBER) {
    BRANCHES_LEFT_DF[k , ] = c(BRANCHES_X_DOMAIN_3rd[i], BRANCHES_LEFT_MATRIX[i + length(BRANCHES_X_DOMAIN_1st) +
                                                                                length(BRANCHES_X_DOMAIN_2nd), j])
    k = k + 1
  }
}

for (i in 1:length(BRANCHES_X_DOMAIN_4th)) {
  for (j in 1:BRANCHES_OBSERVATIONS_NUMBER) {
    BRANCHES_LEFT_DF[k , ] = c(BRANCHES_X_DOMAIN_4th[i], BRANCHES_LEFT_MATRIX[i + length(BRANCHES_X_DOMAIN_1st) +
                                                                                length(BRANCHES_X_DOMAIN_2nd) + length(BRANCHES_X_DOMAIN_3rd), j])
    k = k + 1
  }
}

BRANCHES_LEFT_DF <-
  data.frame("X" = BRANCHES_LEFT_DF[, 1], "Y" = BRANCHES_LEFT_DF[, 2])

###          BRANCHES R          ###

BRANCHES_RIGHT_MATRIX <-
  matrix(
    nrow = length(BRANCHES_X_DOMAIN_1st) + length(BRANCHES_X_DOMAIN_2nd) + length(BRANCHES_X_DOMAIN_3rd) +
      length(BRANCHES_X_DOMAIN_4th),
    ncol = BRANCHES_OBSERVATIONS_NUMBER
  )

for (i in 1:length(BRANCHES_X_DOMAIN_1st)) {
  BRANCHES_RIGHT_MATRIX[i,] = sample(
    seq(
      BRANCHES_RIGHT_Y_DOMAIN_1st_LOWER_LIMIT(BRANCHES_X_DOMAIN_1st[i]),
      0,
      STEP
    ),
    BRANCHES_OBSERVATIONS_NUMBER,
    replace = T
  )
}

for (i in 1:length(BRANCHES_X_DOMAIN_2nd)) {
  BRANCHES_RIGHT_MATRIX[i + length(BRANCHES_X_DOMAIN_1st),] = sample(
    seq(
      BRANCHES_RIGHT_Y_DOMAIN_2nd_LOWER_LIMIT(BRANCHES_X_DOMAIN_2nd[i]),
      0,
      STEP
    ),
    BRANCHES_OBSERVATIONS_NUMBER,
    replace = T
  )
}

for (i in 1:length(BRANCHES_X_DOMAIN_3rd)) {
  BRANCHES_RIGHT_MATRIX[i + length(BRANCHES_X_DOMAIN_1st) + length(BRANCHES_X_DOMAIN_2nd),] = sample(
    seq(
      BRANCHES_RIGHT_Y_DOMAIN_3rd_LOWER_LIMIT(BRANCHES_X_DOMAIN_3rd[i]),
      0,
      STEP
    ),
    BRANCHES_OBSERVATIONS_NUMBER,
    replace = T
  )
}

for (i in 1:length(BRANCHES_X_DOMAIN_4th)) {
  BRANCHES_RIGHT_MATRIX[i + length(BRANCHES_X_DOMAIN_1st) + length(BRANCHES_X_DOMAIN_2nd) +
                          length(BRANCHES_X_DOMAIN_3rd),] = sample(
                            seq(
                              BRANCHES_RIGHT_Y_DOMAIN_4th_LOWER_LIMIT(BRANCHES_X_DOMAIN_4th[i]),
                              0,
                              STEP
                            ),
                            BRANCHES_OBSERVATIONS_NUMBER,
                            replace = T
                          )
}

BRANCHES_RIGHT_DF <-
  matrix(
    nrow = (
      length(BRANCHES_X_DOMAIN_1st) + length(BRANCHES_X_DOMAIN_2nd) + length(BRANCHES_X_DOMAIN_3rd) +
        length(BRANCHES_X_DOMAIN_4th)
    ) * BRANCHES_OBSERVATIONS_NUMBER,
    ncol = 2
  )

k = 1
for (i in 1:length(BRANCHES_X_DOMAIN_1st)) {
  for (j in 1:BRANCHES_OBSERVATIONS_NUMBER) {
    BRANCHES_RIGHT_DF[k ,] = c(BRANCHES_X_DOMAIN_1st[i], BRANCHES_RIGHT_MATRIX[i, j])
    k = k + 1
  }
}

for (i in 1:length(BRANCHES_X_DOMAIN_2nd)) {
  for (j in 1:BRANCHES_OBSERVATIONS_NUMBER) {
    BRANCHES_RIGHT_DF[k ,] = c(BRANCHES_X_DOMAIN_2nd[i], BRANCHES_RIGHT_MATRIX[i + length(BRANCHES_X_DOMAIN_1st), j])
    k = k + 1
  }
}

for (i in 1:length(BRANCHES_X_DOMAIN_3rd)) {
  for (j in 1:BRANCHES_OBSERVATIONS_NUMBER) {
    BRANCHES_RIGHT_DF[k ,] = c(BRANCHES_X_DOMAIN_3rd[i], BRANCHES_RIGHT_MATRIX[i + length(BRANCHES_X_DOMAIN_1st) +
                                                                                 length(BRANCHES_X_DOMAIN_2nd), j])
    k = k + 1
  }
}

for (i in 1:length(BRANCHES_X_DOMAIN_4th)) {
  for (j in 1:BRANCHES_OBSERVATIONS_NUMBER) {
    BRANCHES_RIGHT_DF[k ,] = c(BRANCHES_X_DOMAIN_4th[i], BRANCHES_RIGHT_MATRIX[i + length(BRANCHES_X_DOMAIN_1st) +
                                                                                 length(BRANCHES_X_DOMAIN_2nd) + length(BRANCHES_X_DOMAIN_3rd), j])
    k = k + 1
  }
}

BRANCHES_RIGHT_DF <-
  data.frame("X" = BRANCHES_RIGHT_DF[, 1], "Y" = BRANCHES_RIGHT_DF[, 2])

####################################
###      POST-ORGANISE STUFF     ###
####################################
dates_sample = 1:10
ROOT_LEFT_DF["date"] <-
  sample(dates_sample, dim(ROOT_LEFT_DF)[1], replace = T)
BRANCHES_LEFT_DF["date"] <-
  sample(dates_sample, dim(BRANCHES_LEFT_DF)[1], replace = T)
ROOT_RIGHT_DF["date"] <-
  sample(dates_sample, dim(ROOT_RIGHT_DF)[1], replace = T)
BRANCHES_RIGHT_DF["date"] <-
  sample(dates_sample, dim(BRANCHES_RIGHT_DF)[1], replace = T)

####################################
###           PLOTTING           ###
####################################

p_line <- ggplot(NULL, aes(X, Y)) +
  geom_point(data = ROOT_LEFT_DF, color = "brown") +
  geom_point(data = BRANCHES_LEFT_DF, color = "green") +
  geom_point(data = ROOT_RIGHT_DF, color = "brown") +
  geom_point(data = BRANCHES_RIGHT_DF, color = "green") +
  xlim(0, 20) +
  ylim(-10, 10) +
  coord_flip() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

p_line

anim_line <- p_line + transition_states(date)

anim_line
