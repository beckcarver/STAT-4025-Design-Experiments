contaminant <- read.csv("WYDEQ.csv",header=TRUE)

t.test(contam~Lab, alternative='greater', conf.level=.95, var.equal=TRUE, 
       data=contaminant)

m1 <- lm(contam~Lab,data=contaminant)
anova(m1)

qf(0.95, df1=1, df2=18, lower.tail = TRUE)

library(tidyverse)
Lab_sum <- contaminant %>%
           group_by(Lab)%>%
           summarize(n=n(),
                     meancont=mean(contam),
                     sampvar=var(contam))


spsquare <- (9*Lab_sum$sampvar[1] + 9*Lab_sum$sampvar[2])/18
spsquare

#Note that spsquare above is the same as the mean square
#residuals in the anova analysis

t.test(contaminant$LabA, contaminant$LabB, alternative='greater', 
       conf.level=.95, paired=TRUE)

contaminant$Specimen <- as.factor(contaminant$Specimen)

m2 <- lm(contam~Specimen+Lab,data=contaminant)
anova(m2)

qf(0.95, df1=1, df2=9, lower.tail = TRUE)

burndata <- read.csv("burndata.csv",header=TRUE)
burndata$Blocks <- as.factor(burndata$Blocks)
mburn <- lm(relabund~Blocks+Treatment,data=burndata)
anova(mburn)

mburncrd <- lm(relabund~Treatment,data=burndata)
anova(mburncrd)

(9*213.547 + 10*0.202)/19
101.26/0.202
(10/12)*(21/19)*501.287
