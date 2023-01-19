#### Set up ####
oransil <- read.csv("orangesilage.csv")

library(tidyverse)

#### Q 1 ####
oransil$indCon <- (oransil$Type == "Control")*1

an1 <-lm(Amount~Type, data = oransil)
anova(an1)

summarise(oransil)

#### Question 3 ####
oransil$resids <- residuals(an1)

#### Question 4 ####
{qqnorm(oransil$resids,xlab = "Normal Quantiles", 
        ylab = "Model Residuals")
  qqline(oransil$resids)}

#### Question 5 ####
shapiro.test(oransil$resids)



#### Set up ####
studsat <- read.csv("student_satisfaction.csv")

library(tidyverse)
#### Question 6 ####
an2 <- lm(BMSLSS~Employ_Status, data = studsat)
anova(an2)

#### Question 7 ####
satisfaction_summary <- studsat %>%
  group_by(BMSLSS,Employ_Status) %>%
  summarise(grpmns = mean(BMSLSS))
           
studsata <- satisfaction_summary %>%
  group_by(BMSLSS)%>%
  summarise(satis = mean(grpmns))

mean(studsata$satis)

#### Question 8 ####
library(tidyverse)
studsat$natlog <- ln(BMSLSS)
satisfaction_summary$BMSLSS <- as.factor(satisfaction_summary$BMSLSS)
satisfaction_summary$logmeans <- log(satisfaction_summary$grpmns)

an2 <- lm(logmeans~BMSLSS, data = satisfaction_summary)
satisfaction_summary$residan2 <- residuals(an2)

shapiro.test(satisfaction_summary$residan2)

{qqnorm(satisfaction_summary$residan2, xlab="Normal Quantiles", ylab="Model Residuals")
  qqline(satisfaction_summary$residan2)}

anova(an2)



