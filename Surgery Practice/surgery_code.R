surgery <- read.csv("surgery_two_group.csv",header=TRUE)
library(tidyverse)

#to check normality distribution, use a Normal QQplot

boxplot(surgery$Strength ~ surgery$Method, ylab="Strength")

#below is the qqnorm for Tumor and Surgery data
#par(mfrow=c(1,2)) puts two plots in 1 row

#first separate the data into two different data sets
#one for Arrow and the other for Vertical

surgery_Arr <- surgery %>%
  filter(Method=="Meniscus_Arrow")
surgery_Ver <- surgery %>%
  filter(Method=="Vertical_Suture")

par(mfrow=c(2,1))
{qqnorm(surgery_Arr$Strength,main="Arrow")
  qqline(surgery_Arr$Strength)}

{qqnorm(surgery_Ver$Strength, main="Vertical")
  qqline(surgery_Ver$Strength)}

par(mfrow=c(1,1))

#Below is a set of code which computes
#sample variance of strengths for each method
Vertdat <- surgery %>%
  filter(Method=="Vertical_Suture")
Arrdat <- surgery %>%
  filter(Method=="Meniscus_Arrow")
varVert <- var(Vertdat$Strength)
varArr <- var(Arrdat$Strength)
varVert
varArr

#code below conducts a t-test to determine 
#mean differences

t.test(Strength ~ Method,var.equal=TRUE,data=surgery)

#code below uses a model to to determine
#mean differences

library(ggplot2)
qplot(Method, Strength, data = surgery)

surgery$Arrow_Ind <- (surgery$Method=="Meniscus_Arrow")*1

m1 <- lm(Strength ~ Arrow_Ind, data=surgery)
summary(m1)

