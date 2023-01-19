laundry <- read.csv("laundry.csv",header=T)
laundry$Machine <- as.factor(laundry$Machine)
laundry$Treatment <- as.factor(laundry$Treatment)
laundry$Cycle_Speed <- as.factor(laundry$Cycle_Speed)
library(lme4)
library(lmerTest)

#### 1 D ####
pf(12.5, df1=4,df2 = 36, lower.tail = F)

#### 3 A ####

launda <- lm(Shrink_pct ~ Treatment + Machine + Treatment:Machine + 
                Machine*Cycle_Speed, data = laundry)
anova(launda)

pf( ?? , df1=3,df2 = 9, lower.tail = F)

#### 3 B ####

laundb <- lmer(Shrink_pct ~ Treatment + Machine*Cycle_Speed + 
                 (1 | Treatment:Machine), data = laundry)
anova(laundb)

#### 3 C ####
interaction.plot(laundry$Cycle_Speed, laundry$Treatment, laundry$Shrink_pct, 
                 fun = mean,
                 type = "b", 
                 pch=c(1:4),
                 legend = TRUE,
                 trace.label = "Treatment",
                 fixed = FALSE,
                 xlab = "Cycle Speed",
                 ylab = "mean Shrink Impact")

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