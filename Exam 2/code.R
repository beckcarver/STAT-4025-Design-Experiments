library(tidyverse)

## problem 2
p2 <- read.csv("exam2p2.csv", header = T)
m1 <- lm(Froth~Speed+Nozzle+Speed:Nozzle,data = p2)
summary(m1)
anova(m1)

p2$resids <- residuals(m1)

interaction.plot(p2$Speed, p2$Nozzle, p2$Froth, 
                 fun = mean,
                 type = "b", 
                 pch=c(1:3),
                 legend = TRUE,
                 trace.label = "Nozzle Type",
                 fixed = FALSE,
                 xlab = "Speed",
                 ylab = "Frothing")


TukeyHSD(aov(m1),conf.level = 0.95)



# problem 3b
p3 <- read.csv("Part3b.csv", header = T)
m2 <- lm(Qual~Heat_Time+Trans_Time+Heat_Time:Trans_Time,data=p3)

summary(m2)
anova(m2)

p3$resids <- residuals(m2)

library(gplots)

plotmeans(Qual~Heat_Time,xlab="Heat Time",ylab="Quality", p=.95,
          main="Heat TIme Main Effect Plot",barcol="black", n.label=F,data=p3)




B_coded<-seq(-1,1,length.out=50)
A_coded<-seq(-1,1,length.out=50)

z<- outer(B_coded,A_coded,function(a,b)
  predict(m2,newdata=
            data.frame(Heat_Time=a,Trans_Time=b)))
z

library(plot3D)

contour2D(x = B_coded,y = A_coded, z = z, 
          xlab = "Heat Time", 
          ylab = "Transition Time", 
          clab = "Quality")

# problem 4

pf(22.213,df1=2,df2=6,lower.tail=F)
pf(10.483,df1=8,df2=9,lower.tail=F)
pf(1.172,df1=10,df2=17,lower.tail=F)
