# Statistics II TA 10/11 #########


library(BSDA)
library(psych)
library(dplyr)
library(readxl)
library(magrittr)


setwd("C:/Users/zuoch/OneDrive/桌面/統計學（二）")  # change to your file location
exams <- read_xlsx("exams.xlsx")

## Test ####
# z-test
mu <- 70
sd <- sd(exams$math)

z <- z.test(exams$math, alternative = "less", mu = mu, sigma.x = sd)
print(z)

z$statistic
z$p.value

# t-test
t <- t.test(exams$math, alternative = "less", mu = mu)
print(t)

t$statistic
t$parameter  #df


# dplyr

# add a new line
exams_m <- mutate(.data = exams, math2 = math^2)
head(exams_m)

# grouping and Summarise
exams_g <- group_by(.data = exams, gender)
head(exams_g)  # Groups: gender [2]

summarise(.data = exams_g, mean_math = mean(math))

group_by(exams, gender) %>% 
  summarise(mean_math = mean(math))

# Select Variable
select(.data = exams, c(gender, math))
select(.data = exams, gender, race, math)

# Filter Some Row
filter(.data = exams, race == "group A")

filter(exams, math >= 60)

filter(exams, gender == "female" & math >= 60)
