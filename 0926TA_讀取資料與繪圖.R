# 統計學（二）TA 9/27 讀取資料及繪圖 ####

setwd("/Users/yezzuo-chen/Library/CloudStorage/OneDrive-個人/桌面/統計學（二）")
getwd()

# Package ####

library(readxl)
library(ggplot2)

# 讀取資料 ####

# CSV
df <- read.csv("exams.csv", header = TRUE, sep = ",")

write.csv(df, file = "exams.csv", row.names = FALSE)

# EXCEL
df <- read_excel("exams.xlsx", sheet = "exams", skip = 0)
df <- read_xlsx("exams.xlsx", sheet = "exams", skip = 0)

# Data Structure ####
head(df)
str(df)
#colnames(df) <- c("gender", "race", "par.edu", "lunch", "prep.course", "math", "reading", "writing")
colnames(df)
summary(df)

# Descriptive Statistics ####
mean(df$math)
sum(df$math)
median(df$math)

table(df$gender)
table(df$par.edu)

table(df$gender, df$race)

# Base Plot ####

# bar plot
tb_gender <- table(df$gender)
prop.table(tb_gender)

barplot(tb_gender)

barplot(tb_gender, main = "Gender Bar Plot", names.arg = c("Female", "Male"),
        xlab = "Gender", ylab = "number")

# pie plot
pie(tb_gender)

# scatter plot
plot(df$reading, df$writing)
plot(writing ~ reading, data = df)

# historgram
hist(df$math)

hist(df$math, freq = FALSE)
lines(density(df$math))

# box plot
boxplot(df$math, main = "Math Score Box Plot", xlab = "Math Score")

# ggplot ####

# bar plot
ggplot(data = df, aes(x = race)) +
  geom_bar()

ggplot(data = df, aes(x = race, y = after_stat(count), fill = gender)) +
  geom_bar() +
  labs(title = "Bar Plot of Race and Gender", x = "Race", fill = "Gender")

# historgram
ggplot(data = df, aes(x = math)) +
  geom_histogram()

ggplot(data = df, aes(x = math)) +
  geom_density()
