library(tidyverse)
library(gplots)
library(multcomp)


#read file
anchor <- read.csv("ortho_study.csv")

# one way anova, one factor
m1 <- lm(pull_force~Anchor_Type, data = anchor)
anchor$resids <-residuals(m1)
anova(m1)

# CI plot
plotmeans(pull_force~Anchor_Type,xlab="Anchor Type",ylab="Pull Force", p=.95, 
          main="Ortho Main Effect Plot",barcol="black",
          n.label=F,data=anchor)

# Calculate CI terms
c_anchor <- filter(anchor, Anchor_Type == "C")

mean(c_anchor$pull_force)
sd(c_anchor$pull_force)
qt(p = 0.05/2, df = 4, lower.tail = F)

# calculate MSE
mean(anchor$resids^2)

# multi-factor anova
m2 <- lm(pull_force~Anchor_Type + Foam_Dense + Anchor_Type:Foam_Dense, data = anchor)
anova(m2)
anchor$resids_multi <- residuals(m2)

# calculate new MSE\
mean(anchor$resids_multi^2)

# interaction plot

interaction.plot(anchor$Anchor_Type, anchor$Foam_Dense, anchor$pull_force, 
                 fun = mean,
                 type = "b", 
                 pch=c(1:3),
                 legend = TRUE,
                 trace.label = "Foam Density",
                 fixed = FALSE,
                 xlab = "Anchor Type",
                 ylab = "Pull force")

# Tukey for type B
b_anchor <- filter(anchor, Anchor_Type == "B")
m3 <- aov(pull_force~Foam_Dense, data = b_anchor)
summary(m3)
TukeyHSD(m3, conf.level = .95)
qt(p=0.05, df = 4, lower.tail = F)

# Contrast


