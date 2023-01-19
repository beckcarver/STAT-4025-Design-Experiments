library(tidyverse)
library(gplots)
library(multcomp)



# HW10 part 1 ---------------------------------------------------------------- #
anchor <- read.csv("ortho_study.csv")

m1 <- lm(pull_force~Foam_Dense +
           Anchor_Type + Foam_Dense:Anchor_Type, data = anchor)
aovm1 <- aov(m1)

summary(m1)
# (b high) v (b low)

contrast1 <- matrix(c(1 , -1 , 0 , 0 , -1 , 0 ),nrow=1,byrow=T)
rownames(contrast1) <- c("B High vs. B Low")
colnames(contrast1)<-names(coef(aovm1))
contrast1

summary(glht(m1, linfct = contrast1))
confint(glht(m1, linfct = contrast1))

# i am extremely confused how to get our contrast values,
# everyone is giving different messages in the codeshare

# HW 10 part 2 ----------------------------------------------------------------#

barrel <-  read.csv("barrel_accuracy.csv")

m2coded <- lm(pattern_diameter~ (A + B + C)^2 ,data=barrel)
anova(m2coded)
summary(m2coded)

m2best <- lm(pattern_diameter~ (A + B + C)^2 - A - A:B - B:C,data=barrel)
summary(m2best)

library(gplots)
par(mfrow=c(1,3))
plotmeans(pattern_diameter~A,xlab="A",ylab="Pattern", 
          main="A",p=0.95,
          n.label=F,data=barrel)
plotmeans(pattern_diameter~B,xlab="B",ylab="Pattern", 
          main="B",p=0.95,
          n.label=F,data=barrel)
plotmeans(pattern_diameter~C,xlab="C",ylab="Pattern", 
          main="C",p=0.95,
          n.label=F,data=barrel)

par(mfrow=c(1,1))
interaction.plot(barrel$C, barrel$A,barrel$pattern_diameter, 
                 fun = mean,
                 type = "b", 
                 pch=c(1:3),
                 legend = TRUE,
                 trace.label = "Method",
                 fixed = FALSE,
                 xlab = "Tool",
                 ylab = "Pattern")

bar_means <- barrel %>%
  group_by(A,B,C)%>%
  summarise(cellmn=mean(pattern_diameter),
            sumvals =sum(pattern_diameter),
            sde = sd(pattern_diameter),
            count = n(),
            stdmn = sde/sqrt(count),
            lower = cellmn-qt(p=0.975,df=5,lower.tail = T) * stdmn,
            upper = cellmn+qt(p=0.975,df=5,lower.tail = T) * stdmn)

bar_conf <-bar_means%>%filter(A==-1,B==-1,C==-1)
summary(bar_conf)


C <-seq(-1,1,length.out=50)
A <-seq(-1,1,length.out=50)

z<- outer(C,A,function(a,b)
  predict(m2coded,newdata=
            data.frame(C=a,A=b,B=-1)))
library(plot3D)

contour2D(x = C,y = A, z = z)
