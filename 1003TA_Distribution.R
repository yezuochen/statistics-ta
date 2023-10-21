# Stat1_R: Ch10. Hypotheses testing ####


library(readxl)
library(BSDA)
library(asbio)
library(ggplot2)
library(psych)
library(dplyr)

# Distribution #####

# normal distribution

x <- seq(from = -3.5, to = 3.5, by = 0.1)  # 71 points from -3.5 to 3.5 in 0.1 steps

d <- dnorm(x, mean = 0, sd = 1)
plot(x=x, y=d, type = "l", 
     main = "Standard Normal Distribution Density", 
     xlab = "Z-score", ylab = "density")

p <- pnorm(x, mean = 0, sd = 1)
plot(x, p, type = "l", 
     main = "Standard Normal Distribution", 
     xlab = "Z-score", ylab = "Distribution")

qnorm(0.5)
qnorm(p, mean = 0, sd = 1)

r <- rnorm(n = 10000, mean = 0, sd = 1)
hist(r, breaks = 40, #freq = FALSE, 
     main = "Standard Normal Distribution", 
     xlab = "Z-score")

# t distribution

x <- seq(-6, 6, 0.1)

d <- dt(x, df = 30)
plot(x, d, type = "l", main = "t distribution", 
     xlab = "t-score", ylab = "density")
lines(x, dt(x, 5), col = "blue")
lines(x, dt(x, 1), col = "red")
lines(x, dnorm(x), lty = 2)
legend("topright", legend = c("df = 1", "df = 5", "df = 30", "normal"), 
       col = c("red", "blue", "black", "black"),
       lty = c(1,1,1,2))

p <- pt(x, df = 30)
plot(x, p, type = "l", main = "t distribution", 
     xlab = "t", ylab = "Distribution")


qt(p, df = 30)

r <- rt(n = 10000, df = 30)

hist(r, breaks = 40, 
     main = "t distribution", 
     xlab = "t-score", prob = TRUE)
curve(dt(x, 30), add = TRUE, col = "red")
lines(x, dt(x, 30))

# Data ####
exams <- read_excel("exams.xlsx", skip = 0)

ggplot(exams, aes(x = reading)) +
  ggtitle("Histogram of Reading VS N(70, 15^2)") +
  geom_histogram(aes(y = after_stat(density)),
                 color = "blue", fill = "white", bins = 20) +
  stat_function(fun = dnorm, args = list(mean = mean(exams$reading), 
                                         sd = 15)) +
  theme_light()

ggplot(exams, aes(x=reading)) +
  ggtitle("Histogram of Reading VS N(69.422, 14.37^2)") +
  geom_histogram(aes(y = after_stat(density)),
                 color = "blue", fill = "white", bins = 20) +
  stat_function(fun = dnorm, args = list(mean = mean(exams$reading), 
                                         sd = sd(exams$reading))) +
  theme_light()

# dplyr