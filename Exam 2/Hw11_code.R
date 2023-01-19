##Homework 11 code

wool <- read.csv("laundry.csv",header=T)

library(tidyverse)
library(lme4)
library(lmerTest)

wool$Machine <- as.factor(wool$Machine)
wool$Treatment <- as.factor(wool$Treatment)
wool$Cycle_Speed <- as.factor(wool$Cycle_Speed)

#whole plot effect (i.e. Nitrogen effect)

woolmod <- lmer(Shrink_pct ~ Machine + Treatment + Cycle_Speed + 
                  Treatment:Cycle_Speed + 
                 (1 | Machine:Treatment), data = wool)
anova(woolmod)
summary(woolmod)
anova(woolmod)

woolmod2 <- lm(Shrink_pct ~ Machine + Treatment +Machine:Treatment
              + Cycle_Speed + Treatment:Cycle_Speed, 
              data = wool)
anova(woolmod2)
1004.18/12.74

interaction.plot(wool$Cycle_Speed, wool$Treatment, wool$Shrink_pct, 
                 fun = mean,
                 type = "b", 
                 pch=c(1:4),
                 legend = TRUE,
                 trace.label = "Treatment",
                 fixed = FALSE,
                 xlab = "Cycle Speed",
                 ylab = "Shrinkage Percent")

#Contrast to compare mean of Treatment 1 to mean of Treatment 2
#at 1400 RVM

summary(woolmod)

varBvarC <- matrix(c(0,0,1,-1,0),nrow=1)
contest(massmod2,L=varBvarC,
        joint=F,confint=T)

woolmod2 <- lm(Shrink_pct ~ Machine + Treatment + Machine:Treatment + 
                Cycle_Speed + Treatment:Cycle_Speed, data = wool)
anova(woolmod2)

