orange <- read.csv("orangesilage.csv", header = TRUE)

names(orange)[1] <- "Type"

names(orange)[3] <- "Grpmean"

library(tidyverse)

boxplot(Amount~Type,data = orange)

m1 <- aov(Amount~Type,data=orange)

orange$Resids <- residuals(m1)

summary(m1)

shapiro.test(orange$Resids)

{qqnorm(orange$Resids,xlab = "Norm", 
        ylab = "Reids")
  qqline(orange$Resids)}

#### Set up ####
studsat <- read.csv("student_satisfaction.csv")

library(tidyverse)
#### Question 6 ####
an2 <- lm(BMSLSS~Employ_Status, data = studsat)
anova(an2)

#### Question 7 ####
sat_sum <- studsat %>%
  group_by(BMSLSS,Employ_Status) %>%
  summarise(grpmns = mean(BMSLSS))

sat_sump2 <- sat_sum %>%
  group_by(BMSLSS)%>%
  summarise(satis = mean(grpmns))

mean(sat_sump2$satis)

#### Question 8 ####
library(tidyverse)
studsat$natlog <- ln(BMSLSS)
sat_sumar$BMSLSS <- as.factor(sat_sumar$BMSLSS)
sat_sumar$logmeans <- log(satsumar$grpmns)

an2 <- lm(logmeans~BMSLSS, data = sat_sumar)
satisfaction_sum$residan2 <- residuals(an2)

shapiro.test(satisfaction_summary$residan2)

{qqnorm(satisfaction_summary$residan2, xlab="Normal Quantiles", ylab="Model Residuals")
  qqline(satisfaction_summary$residan2)}

anova(an2)




