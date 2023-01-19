barrels <- read.csv("barrel_accuracy.csv",header=TRUE)
m1 <- lm(pattern_diameter~A*B*C,data=barrels)
summary(m1)


m2 <- lm(pattern_diameter~A+B+C+A:C,data=barrels)
summary(m2)

barrels$residsm2 <- residuals(m2)

{qqnorm(barrels$residsm2,xlab="Normal Quantiles",
        ylab="Model Residuals")
  qqline(barrels$residsm2)}

library(tidyverse)
ggplot(barrels,aes(x=A,y=residsm2))+
  geom_point()+
  geom_hline(yintercept=0,colour="blue")


C <-seq(-1,1,length.out=50)
A <-seq(-1,1,length.out=50)

z<- outer(C,A,function(a,b)
  predict(m2,newdata=
            data.frame(C=a,A=b,B=-1)))

interaction.plot(barrels$C, barrels$A, barrels$pattern_diameter, 
                 fun = mean,
                 type = "b", 
                 pch=c(1:3),
                 legend = TRUE,
                 trace.label = "Cut Speed",
                 fixed = FALSE,
                 xlab = "Cut Angle",
                 ylab = "mean diameter")

library(plot3D)

contour2D(x = C,y = A, z = z, 
          xlab = "C: Cutting Angle", 
          ylab = "A: Cutting Speed", 
          clab = "Barrel Accuracy")

persp3D(x = C, y = A, z = z, 
        ticktype = "detailed", phi = 15, theta = 45, 
        xlab = "C: Cutting Angle", 
        ylab = "A: Cutting Speedn",
        grid=TRUE,
        zlab = "Emissions", clab = "Barrel Accuracy", 
        contour = TRUE, cex.axis = 0.75, cex.lab = 0.75)

scatter3D(x = barrels$B, y = barrels$A, 
          z = barrels$pattern_diameter, add = TRUE, type = "p", 
          pch = 19, col = "black", lwd = 2, colkey = FALSE)

#confidence interval at optimal combination

pred_opt <-predict(m2,newdata=
          data.frame(C=-1,A=-1,B=-1),se.fit=T)
pred_opt

tmult <- qt(0.975,df=19,lower.tail=T)
tmult
lower <- pred_opt$fit - tmult*pred_opt$se.fit
upper <- pred_opt$fit + tmult*pred_opt$se.fit

lower
upper

###############block design ###############

pf(12.5,df1=4,df2=36,lower.tail=FALSE)
