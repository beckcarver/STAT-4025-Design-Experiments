library(tidyverse)
coal <- read.csv("coal.csv",header=TRUE)

#mcoal is modeling raw units in flow and temp
mcoal <- lm(emissions~A_flow+B_temp+A_flow:B_temp,
            data=coal)
anova(mcoal)

mcoal_code <- lm(emissions~A_coded+B_coded+A_coded:B_coded,
                data=coal)
anova(mcoal_code)
summary(mcoal_code)

coal_cellmns <- coal %>%
               group_by(A_flow,B_temp)%>%
               summarise(cellmn=mean(emissions),
                         sumvals =sum(emissions))

mcode_coal <- lm(emissions~A_coded+B_coded+A_coded:B_coded,
                 data=coal)

summary(mcode_coal)
anova(mcode_coal)

anova(mcoal)

#main effects plots

#library(Rcmdr)
#library(RcmdrPlugin.DoE)
library(gplots)
par(mfrow=c(1,2))

plotmeans(emissions~A_coded,xlab="Air Flow",ylab="Emissions", 
          main="Air Flow",p=0.95,
          n.label=F,data=coal)

plotmeans(emissions~B_coded,xlab="Temperature",ylab="Emissions", 
          main="Air Flow",p=0.95,
          n.label=F,data=coal)

par(mfrow=c(1,1))
interaction.plot(coal$B_coded, coal$A_coded,coal$emissions, 
                 fun = mean,
                 type = "b", 
                 pch=c(1:3),
                 legend = TRUE,
                 trace.label = "Flow Reduction",
                 fixed = FALSE,
                 xlab = "Furnace Temperature",
                 ylab = "Mean emissions")


#below code produces contour plot

B_coded<-seq(-1,1,length.out=50)
A_coded<-seq(-1,1,length.out=50)

z<- outer(B_coded,A_coded,function(a,b)
          predict(mcode_coal,newdata=
              data.frame(B_coded=a,A_coded=b)))
z

library(plot3D)

contour2D(x = B_coded,y = A_coded, z = z, 
          xlab = "B: Furnace Temp", 
          ylab = "A: Flow Reduction", 
          clab = "Emissions")

scatter3D(x = coal$B_coded, y = coal$A_coded, 
          z = coal$emissions, add = FALSE, type = "p",
          ticktype="detailed",
          surface=FALSE, grid=TRUE,
          xlab = "B: Furnace Temp", 
          ylab = "A: Flow Reduction", 
          zlab = "Emissions",
          phi = 15, theta = 45,
          pch = 19, col = "black", lwd = 2, colkey = FALSE)

persp3D(x = B_coded, y = A_coded, z = z, 
        ticktype = "detailed", phi = 15, theta = 45, 
        xlab = "B: Furnace Temp", 
        ylab = "A: Flow Reduction",
        grid=TRUE,
        zlab = "Emissions", clab = "Emissions", 
        contour = TRUE, cex.axis = 0.75, cex.lab = 0.75)

scatter3D(x = coal$B_coded, y = coal$A_coded, 
          z = coal$emissions, add = TRUE, type = "p", 
          pch = 19, col = "black", lwd = 2, colkey = FALSE)

sucrose_dat <- read.csv("sucrose2.csv",header=TRUE)

m1 <- lm(turbidity~poly_code + prop_code + sucrose_code +
                 poly_code:prop_code + poly_code:sucrose_code + 
                 prop_code:sucrose_code + 
                 poly_code:prop_code:sucrose_code, 
         data=sucrose_dat)
summary(m1)
anova(m1)

#running m1, we found polysorb:propylene and 
#polysorb:propylene:sucrose not important

m2 <- lm(turbidity~poly_code + prop_code + sucrose_code +
                 poly_code:sucrose_code + 
                 prop_code:sucrose_code, 
         data=sucrose_dat)
summary(m2)
anova(m2)

pred_data <- data.frame(poly_code=1,prop_code=-1,
                        sucrose_code=-1)

#use the predict function to obtain standard error
check2 <- predict(m2,pred_data,se=TRUE,interval="confidence")
check2

check <- predict(m2,pred_data,se=TRUE)
check

lowerbound <- check$fit - qt(0.975,df=check$df)*check$se.fit
lowerbound

sqrt(0.2424/2)

sucrose_dat$resids <- residuals(m2)

library(tidyverse)

ggplot(sucrose_dat,aes(x=poly_code,y=resids))+
        geom_point()+
        geom_hline(yintercept=0,colour="blue")


sucrose_dat_sum <- sucrose_dat %>%
         mutate(poly_code=as.factor(poly_code))%>%
         group_by(poly_code)%>%
         summarise(sampv=var(turbidity),
                   n=n())
