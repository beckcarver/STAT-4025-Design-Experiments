library(tidyverse)
migraine <- read.csv("migraine.csv", header = T)

m1<-lm(Pain~Drug, data = migraine)

m1$residsm1<-residuals(m1)

anova(m1)

mean(m1$residuals^2)

relief <- migraine %>%
  mutate(logpain = log(Pain))%>%
  summarise(grpmns = mean(Pain),
            grpstd = log(logpain))
  



m2 <- lm(logpain~Drug,data=relief)
summary(m2)
library(multcomp)
anovm2 <- aov(m2)
tukecomp <- glht(anovm2,linfct=
                   mcp(Drug='Tukey'))
summary(tukecomp)
confint(tukecomp)
