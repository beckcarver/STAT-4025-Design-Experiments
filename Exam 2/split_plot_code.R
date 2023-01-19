golf <- read.csv("golf.csv",header=T)
golf$Nitrogen <- as.factor(golf$Nitrogen)
golf$Green <- as.factor(golf$Green)
golf$Thatch_yrs <- as.factor(golf$Thatch_yrs)
library(lme4)
library(lmerTest)

#below code, the 1|Green:Nitrogen is a variance term associated with the 
#block by whole plot factor interaction which is the error term for the 
#whole plot effect (i.e. Nitrogen effect)

chlorm <- lmer(chlorophyll ~ Green + Nitrogen*Thatch_yrs + 
                    (1 | Green:Nitrogen), data = golf)
anova(chlorm)

#what happens if we fit without using correct error terms

chlorm2 <- lm(chlorophyll ~ Green+Nitrogen+Green:Nitrogen + 
                            Nitrogen*Thatch_yrs, data = golf)
anova(chlorm2)

interaction.plot(golf$Thatch_yrs, golf$Nitrogen, golf$chlorophyll, 
                 fun = mean,
                 type = "b", 
                 pch=c(1:4),
                 legend = TRUE,
                 trace.label = "Nitrogen",
                 fixed = FALSE,
                 xlab = "Thatch Years",
                 ylab = "mean chlorophyll")

library(emmeans)

chlorm.mm <- emmeans(chlorm, ~ Nitrogen*Thatch_yrs)
chlorm.mm

##SPD(CRD,RCBD)

#strawberry

strawberry <- read.csv("strawberry.csv",header=T)

library(lme4)
library(lmerTest)
massmod <- lmer(mass ~ fertilizer + variety + fertilizer: variety + 
                 (1 | plot), data = strawberry)
anova(massmod)

#naively ignoring the split-plot randomization structure and
#treating fertilizer and variety as a two-factor CRD

massnaive <- lm(mass ~ fertilizer + variety + 
                       fertilizer: variety, data = strawberry)
anova(massnaive)

#we can get the model estimated marginal and cell means from the
#lmerTest library but you need the fixed effects to be factors

strawberry$fertilizer <- as.factor(strawberry$fertilizer)
strawberry$variety <- as.factor(strawberry$variety)
#now, re-run your model with fertilizer and variety as factors

massmod <- lmer(mass ~ fertilizer + variety + fertilizer: variety + 
                  (1 | plot), data = strawberry)
summary(massmod)
anova(massmod)

ls_means(massmod)

#below you can get the beta coefficients associated with the massmod using
#the summary function and these coefficients can be used to test for 
#differences between cell means and for differences among marginal means as
#we have done before

summary(massmod)

#We can reduce our model by eliminating the interaction term

massmod2 <- lmer(mass ~ fertilizer + variety  + 
                (1 | plot), data = strawberry)
summary(massmod2)
anova(massmod2)
library(tidyverse)
straw_sum <- strawberry %>%
  group_by(fertilizer)%>%
  summarise(meanfert=mean(mass))
  
#Contrast to compare mean of variety B to mean
#of variety C

varBvarC <- matrix(c(0,0,1,-1,0),nrow=1)
contest(massmod2,L=varBvarC,
        joint=F,confint=T)


