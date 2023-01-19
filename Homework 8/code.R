library(tidyverse)
library(gplots)
library(multcomp)

yield <- read.csv("tree.csv")
interaction.plot(yield$Variety,yield$Pesticide,
                 yield$Yield,fun = mean,type = "b",
                 pch=c(1:4), legend = TRUE, 
                 trace.label = "Pesticide", fixed = FALSE,
                 xlab = "Variety", ylab = "mean fruit yield")

yield$Variety <- as.factor(yield$Variety)
yield$Pesticide <- as.factor(yield$Pesticide)

m1 <- lm(Yield~Variety+Pesticide+Pesticide:Variety,data = yield)

summary(m1)

anova(m1)

m2 <- lm(Yield~Variety+Pesticide,data = yield)
anova(m2)

yield$resids <- residuals(m1)

shapiro.test(yield$resids)

{qqnorm(yield$resids,
        xlab="Normal Quantiles",
        ylab="Resids")
  qqline(yield$resids)}

