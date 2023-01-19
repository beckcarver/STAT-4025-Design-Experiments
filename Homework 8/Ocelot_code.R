ocelot1 <- read.csv("Ocelot1.csv",header=TRUE)
library(tidyverse)

library(gplots) 

plotmeans(abund~habitat,xlab="Habitat",ylab="Abundance", p=.95, 
          main="Habitat Main Effect Plot",barcol="black",
          n.label=F,data=ocelot1)

plotmeans(abund~aspect,xlab="Aspect",ylab="Abundance", p=.95, 
          main="Aspect Main Effect Plot",barcol="black",
          n.label=F,data=ocelot1)

#Main effects plots plot the mean response
#across each level of your factor

#Understanding standard error bars in plotmeans

aspect <-  ocelot1 %>%
           group_by(aspect)%>%
           summarise(mnasp = mean(abund),
                     sdasp = sd(abund),
                     count = n(),
                     stderrmn = sdasp/sqrt(count),
                     lower=mnasp-qt(p=0.975,df=5,lower.tail=TRUE)*stderrmn,
                     upper=mnasp+qt(p=0.975,df=5,lower.tail=TRUE)*stderrmn)

qt(p=0.975,df=5,lower.tail=TRUE)

write.csv(aspect,"aspect.csv")

#Next plot is an interaction plot with habitat on x-axis

interaction.plot(ocelot1$habitat, ocelot1$aspect, ocelot1$abund, 
                 fun = mean,
                 type = "b", 
                 pch=c(1:3),
                 legend = TRUE,
                 trace.label = "aspect",
                 fixed = FALSE,
                 xlab = "habitat",
                 ylab = "mean ocelot abundance")

#Next plot is an interaction plot with aspect on x-axis

interaction.plot(ocelot1$aspect, ocelot1$habitat, ocelot1$abund, 
                 fun = mean,
                 type = "b", 
                 pch=c(1:3),
                 legend = TRUE,
                 trace.label = "habitat",
                 fixed = FALSE,
                 xlab = "aspect",
                 ylab = "mean ocelot abundance")

#Now will run the ANOVA to determine which effects are 
#statistically significant

m1 <- lm(abund~habitat+aspect+aspect:habitat,data=ocelot1)
summary(m1)
anova(m1)

#alternatively, you can specify the following

m1a <- lm(abund~habitat*aspect,data=ocelot1)
anova(m1a)

ocelot1$residsm1 <- residuals(m1)
shapiro.test(ocelot1$residsm1)

{qqnorm(ocelot1$residsm1,xlab="Normal Quantiles",
        ylab="Model Residuals")
  qqline(ocelot1$residsm1)}

#ANOVA if we only modeled Aspect

m2_aspect <- lm(abund~aspect,data=ocelot1)
anova(m2_aspect)

#Final fit without interaction
ocelot1$habitat <- as.factor(ocelot1$habitat)
m3 <- lm(abund~habitat+aspect,data=ocelot1)
summary(m3)
anovm3 <- anova(m3)
anovm3$`Mean Sq`[3]
anovm3

library(multcomp)
tuke_mult <- qtukey(0.975, 6, 13, lower.tail = TRUE)
tuke_mult

se_mn_diff <- sqrt(anovm3$`Mean Sq`[3]/6)
tuke_moe <- (tuke_mult/sqrt(2))*se_mn_diff
tuke_moe

hab_compare <- TukeyHSD(aov(m3),"habitat",conf.level=0.95)

#to view the Tukey's comparisons

library(multcompView)
plot(hab_compare , col="blue")

#############interaction ex - gas additives ###
guinea <- read.csv("pigs.csv",header=T)

guinea$dose <- as.factor(guinea$dose)

m1 <- lm(len~supp + dose + supp:dose,data=guinea)
anova(m1)
coef(aov(m1))
summary(m1)

aovm1 <- aov(len~supp + dose + supp:dose,data=guinea)

guinea$residsm1 <- residuals(m1)

{qqnorm(guinea$residsm1,xlab="Normal Quantiles",
        ylab="Model Residuals")
  qqline(guinea$residsm1)}

shapiro.test(guinea$residsm1)

guinea_sum <- guinea %>%
          group_by(supp,dose)%>%
          summarise(varlen = var(len),
                    mnlen = mean(len),
                    n=n())

guinea_dose <- guinea %>%
               group_by(dose)%>%
               summarise(mndoselen = mean(len),
                         n=n())

var_rat <- max(guinea_sum$varlen)/
           min(guinea_sum$varlen)

var_rat

interaction.plot(guinea$dose, guinea$supp, guinea$len, 
                 fun = mean,
                 type = "b", 
                 pch=c(1:3),
                 legend = TRUE,
                 trace.label = "supp",
                 fixed = FALSE,
                 xlab = "dose",
                 ylab = "mean tooth length")
library(multcomp)
contrast1 <- matrix(c(0,0,1,-1,0.5,-0.5),nrow=1,byrow=T)
rownames(contrast1) <- c("Mn Dose1 vs. Mn Dose2")
colnames(contrast1)<-names(coef(aovm1))
contrast1

summary(glht(m1, linfct = contrast1))
confint(glht(m1, linfct = contrast1))

qt(0.975,df=54,lower.tail=T)

guinea_dose$mndoselen[2]-guinea_dose$mndoselen[3]

mnOJ1_mnVC1 <- matrix(c(0,-1,0,0,-1,0),nrow=1,byrow=T)
rownames(mnOJ1_mnVC1) <- c("Mn OJ1 vs. Mn VC1")
colnames(mnOJ1_mnVC1)<-names(coef(aovm1))
mnOJ1_mnVC1

summary(glht(m1, linfct = mnOJ1_mnVC1))
confint(glht(m1, linfct = mnOJ1_mnVC1))

guinea_sum$mnlen[2]-guinea_sum$mnlen[5]