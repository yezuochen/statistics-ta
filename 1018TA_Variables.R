# 統計學II TA 10/17 Variables #########

# 載入所需的套件
library(BSDA)
library(psych)
library(dplyr)
library(readxl)
library(magrittr)

# 設定工作目錄
setwd("C:/Users/zuoch/OneDrive/Desktop/Statistics (II)")

# 讀取 exams.xlsx 檔案
exams <- read_xlsx("exams.xlsx")

# 載入內建資料集 iris 和 mtcars
data(iris)
data(mtcars)


# 計算 reading 和 writing 成績的標準差
sd.r <- sd(exams$reading)
sd.w <- sd(exams$writing)

# 執行 Z 檢定
z.test(exams$reading, exams$writing, sigma.x = sd.r, sigma.y = sd.w)

# 進行成對 t 檢定
t.test(exams$reading, exams$writing, paired = TRUE, var.equal = TRUE)



# 以 t 檢定比較 prep.course 對 reading 成績的影響
t.test(reading ~ prep.course, data = exams)

# 使用 t 檢定比較不同 prep.course 對 reading 成績的影響
t.test(exams$reading ~ exams$prep.course)

# 將 prep.course 為 "completed" 的 reading 成績分組

# 從 exams 資料集中，選取 prep.course 為 "completed" 的學生的閱讀成績，
# 並存在 reading.completed 變數中。
reading.completed <- exams$reading[exams$prep.course == "completed"]

# 選取 prep.course 為 "none" 的學生的閱讀成績，
# 並存在 reading.none 變數中。
reading.none <- exams$reading[exams$prep.course == "none"]

# 進行 t 檢定比較 prep.course 為 "completed" 和 "none" 的 reading 成績
t.test(reading.completed, reading.none)


# 生成二項分佈的亂數
rbinom(n = 100, size = 1, prob = 0.3)


#計算 exams 資料集中 race 變數的頻數
table(exams$race)

# 使用 ifelse 函數判斷 race 是否為 "group C"
ifelse(exams$race == "group C", "is.groupC", "not.groupC")

# 新增一個欄位 is_groupC 到 exams 資料集中
exams.is_groupC <- mutate(exams, is_groupC = ifelse(race == "group C", TRUE, FALSE))

# 使用 t 檢定比較 is_groupC 對 reading 成績的影響
t.test(reading ~ is_groupC, data = exams.is_groupC)


# 計算向量 x 的平均值，忽略 NA 值
x <- c(1,3,4,5,NA,1)
mean(x, na.rm = TRUE)


# 使用 for 迴圈從 1 開始，迭代到 10，每次迭代 i 的值增加 1。
for (i in 1:10){
  print(i)
}