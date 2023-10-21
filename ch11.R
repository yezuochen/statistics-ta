# ch11 

library(BSDA)
library(psych)

n1=40
sh1=44.44
sc1=0.675

n2=35
sh2=42.96
sc2=0.675

x1_mean = rep(NA, 1000)
x2_mean = rep(NA, 1000)
d_mean = rep(NA, 1000)

for (i in 1:1000){
  x1_mean[i]=mean(rgamma(n1,shape=sh1,scale=sc1))
  x2_mean[i]=mean(rgamma(n2,shape=sh2,scale=sc2))
  d_mean[i]=x1_mean[i]-x2_mean[i]
}

library(psych)
describe(x1_mean)
describe(x2_mean)
describe(d_mean)

par(mfrow = c(1,3))
hist(x1_mean)
hist(x2_mean)
hist(d_mean)
par(mfrow = c(1,1))

biology <- c(3, 7, 11, 0, 7, 0, 4, 5, 6, 2, 4, 7, 2, 9)
english <- c(8, 5, 4, 10, 4, 5, 7, 2, 6, 1, 2, 7, 0, 6, 4, 12, 5, 2)

z.test(biology, english, mu=0,sigma.x=3^.5, sigma.y=2^.5)

x=c(2,4,9,3,2)
y=c(3,7,5,8,4,3)
t.test(x,y,var.equal = T)

#Method 2
#Data: Two variables V1=proc, V2=time
#Data: 11 observations, each=(proc, time)
proc=c(rep(1,5),rep(2,6))
time=c(x,y)
t.test(time ~ proc,var.equal=T)

#There are totally 21 observations
#V1: brand
#V2: abs
brand=c(rep(1,9),rep(2,12))
abs=c(8,8,3,1,9,7,5,5,12,12,11,10,6,8,9,9,10,11,9,8,10)
t.test(abs ~ brand)

Schadek=c(235,210,231,242,205,230,231,210,225,249)
Bowyer=c(228,205,219,240,198,223,227,215,222,245)
t.test(Schadek, Bowyer, paired=T)

#Method 1: Input 2 vectors of driving time 
#
R25=c(52,67,56,45,70,54,64)
I75=c(59,60,61,51,56,63,57,65)
var.test(R25,I75)

#Method 2: 
#For total 15 observations
#V1=route
#V2=time
#Construct dataframe limos including route and time
route=c(rep("R25", 7), rep("I75", 8))
time=c(52,67,56,45,70,54,64, 59,60,61,51,56,63,57,65)
limos <- data.frame(route, time)
var.test(time ~ route, limos)


ggplot(limos, aes(x=route, y=time)) + 
  geom_boxplot()+  
  theme_light()

