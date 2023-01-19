ocelot1 <- read.csv("Ocelot1.csv",header=TRUE)
library(tidyverse)

#Main effects plots plot the mean response
#across each level of your factor

#library(Rcmdr)
#library(RcmdrPlugin.DoE)

#Need Rcmdr libary for plotMeans function
#which will be used to generate main effects
#plots

plotMeans(ocelot1$abund, ocelot1$aspect, error.bars="se",main="",
          xlab="aspect",ylab="mean abundance")

plotMeans(ocelot1$abund, ocelot1$aspect, error.bars="conf.int",main="",
          xlab="aspect",ylab="mean abundance")

plotMeans(ocelot1$abund, ocelot1$habitat, error.bars="se",main="",
          xlab="habitat",ylab="mean abundance")

#Understanding standard error bars in plotMeans

aspect <-  ocelot1 %>%
           group_by(aspect)%>%
           summarise(mnhab = mean(abund),
                     sdhab = sd(abund),
                     count = n(),
                     stderrmn = sdhab/sqrt(count))
qt(p=0.95,df=5,lower.tail=TRUE)

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

#Next obtain residuals to check Normality

ocelot1$residsm1 <- residuals(m1)
shapiro.test(ocelot1$residsm1)

{qqnorm(ocelot1$residsm1,xlab="Normal Quantiles",
        ylab="Model Residuals")
  qqline(ocelot1$residsm1)}

#ANOVA if we only modeled Aspect

m2_aspect <- lm(abund~aspect,data=ocelot1)
anova(m2_aspect)

#Final fit without interaction
#m3 has habitat:aspect ssq and df dumped into the residua
ocelot1$habitat <- as.factor(ocelot1$habitat)
m3 <- lm(abund~habitat+aspect,data=ocelot1)
summary(m3)
anovm3 <- anova(m3)
anovm3
anovm3$`Mean Sq`
anovm3$`Mean Sq`[3]
anovm3

library(multcomp)
#Now that interaction not important, you can look at
#Tukey's comparisons of marginal means...important to see that
#the TukeyHSD() function doesn't take into account the fact
#that both factors are important so you need to construct your
#own Tukey intervals

tuke_mult <- qtukey(0.95, 6, 13, lower.tail = TRUE)
tuke_mult

se_mn_diff <- sqrt(anovm3$`Mean Sq`[3]/3)
tuke_moe <- (tuke_mult/sqrt(2))*se_mn_diff
tuke_moe

#TukeyHSD function will obtain the confidence limits
#on the 'habitat' main efect levels

hab_compare <- TukeyHSD(aov(m3),"habitat",conf.level=0.95)
hab_compare
summary(hab_compare)
#to view the Tukey's comparisons in a plot

library(multcompView)
plot(hab_compare , col="blue")

#############guinea pig data and associated analyses###

guinea <- read.csv("pigs.csv",header=T)
#need to make sure each explanatory variable is a factor
guinea$dose <- as.factor(guinea$dose)
guinea$supp <- as.factor(guinea$supp)

m1 <- lm(len~supp + dose + supp:dose,data=guinea)
anova(m1)
#the coef() function will obtain the betas from your
#model
coef(aov(m1))
summary(m1)
anova(m1)

aovm1 <- aov(len~supp + dose + supp:dose,data=guinea)

#obtain residuals to check for Normality

guinea$residsm1 <- residuals(m1)

{qqnorm(guinea$residsm1,xlab="Normal Quantiles",
        ylab="Model Residuals")
  qqline(guinea$residsm1)}

shapiro.test(guinea$residsm1)

#below code will compute sample variance, sample means
#and sample size for each supp by dose combination...
#these are sample variances and sample means for 
#each cell

library(tidyverse)

guinea_sum <- guinea %>%
          group_by(supp,dose)%>%
          summarise(varlen = var(len),
                    mnlen = mean(len),
                    n=n())

#below computes marginal means for the dose factor

guinea_dose <- guinea %>%
               group_by(dose)%>%
               summarise(mndoselen = mean(len),
                         n=n())

#checking equal variance assumption by comparing
#max to min sample variances for each cell

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

#we know we have a significant interaction term and 
#can either do Tukey's to compare all cell means or can
#do contrasts to make individual cell comparisons

library(multcomp)

#Tukey's on all of the cell means

cell_mn_guinea <- TukeyHSD(aov(m1),
                            "supp:dose",
                            conf.level=0.95)
cell_mn_guinea

#below computes the Tukey's multiplier which is needed
#for hand computations of the Tukey confidence interval

tukemult<-qtukey(p=0.95, nmeans=6, df=54)/sqrt(2)
tukemult

summary(m1)

#next two lines computes the lower and upper bounds of
#the Tukey interval on mean difference in OJ1 mg vs
#VC 1 mg

diff_oj1_vc1_low <- diff_oj1_vc1-tukemult*sqrt(13.19*(2/10))
diff_oj1_vc1_high <- diff_oj1_vc1+tukemult*sqrt(13.19*(2/10))

#below is a contrast which looks at marginal mean
#difference comparing average growth at 1 mg per day
#to average growth of 2 mg per day

contrast1 <- matrix(c(0,0,1,-1,0.5,-0.5),nrow=1,byrow=T)
rownames(contrast1) <- c("Mn Dose1 vs. Mn Dose2")
colnames(contrast1)<-names(coef(aovm1))
contrast1

summary(glht(m1, linfct = contrast1))
confint(glht(m1, linfct = contrast1))

#below we find the 97.5th percentile of t-distribution
#for computing the confidence interval by hand for the
#contrast approach

qt(0.975,df=54,lower.tail=T)
diff_dose1_2 <- guinea_dose$mndoselen[2]-guinea_dose$mndoselen[3]
diff_1_2_se <- sqrt(13.19)

#Now will compare OJ 1 mg vs. VC 1 mg using contrasts
#mnOJ1_mnVC1 computes the contrast for the OJ 1 mg vs
#VC 1 mg mean difference

mnOJ1_mnVC1 <- matrix(c(0,-1,0,0,-1,0),nrow=1,byrow=T)
rownames(mnOJ1_mnVC1) <- c("Mn OJ1 vs. Mn VC1")
colnames(mnOJ1_mnVC1)<-names(coef(aovm1))
mnOJ1_mnVC1

summary(glht(m1, linfct = mnOJ1_mnVC1))
confint(glht(m1, linfct = mnOJ1_mnVC1))

guinea_sum$mnlen[2]-guinea_sum$mnlen[5]

