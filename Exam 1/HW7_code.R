anaerobic <- read.csv("anaerobic.csv",header=TRUE)

anaerobic$logthresh <- log(anaerobic$Anaerobic_Threshold)

m1 <- lm(Anaerobic_Threshold~Athlete_Type,data=anaerobic)
summary(m1)
anovam1 <- summary(aov(m1))
anovam1
anaerobic$resids <- residuals(m1)

library(tidyverse)

#2 state assumptions
#Normality, constant variance, independence
#Normality by qqnorm and shapiro

{qqnorm(anaerobic$resids,xlab="Normal Quantiles",
        ylab="Model Residuals")
  qqline(anaerobic$resids)}

shapiro.test(anaerobic$resids)
boxplot(Anaerobic_Threshold~Athlete_Type,data=anaerobic)

anaerobic_summary <- anaerobic %>%
             group_by(Athlete_Type) %>%
             summarise(vargroup = sd(Anaerobic_Threshold)^2,
                       mngroup = mean(Anaerobic_Threshold),
                       varlogs = var(logthresh))
varcheck <- max(anaerobic_summary$vargroup)/min(anaerobic_summary$vargroup)
varcheck

#var assumption not met due to ratio of max to min = 2.87
# equal to 268.3/98.3

#Now do log transform
anaerobic$logthresh <- log(anaerobic$Anaerobic_Threshold)
m2 <- lm(logthresh~Athlete_Type,data=anaerobic)
summary(m2)
anaerobic$residsm2 <- residuals(m2)

{qqnorm(anaerobic$residsm2,xlab="Normal Quantiles",
        ylab="Model Residuals")
  qqline(anaerobic$residsm2)}
shapiro.test(anaerobic$residsm2)

varcheck2 <- max(anaerobic_summary$varlogs)/min(anaerobic_summary$varlogs)
varcheck2
#var assumption appears to be ok since ration < 2.5

#Q4 - permutation test for determining mean differences

Fobs <-  anovam1[[1]]$"F value"[1]
Fobs

permutation.test <- function(obsdat,respvar,treatvar,numsims){
  Fperm=c()
  result=0
  for(i in 1:numsims){
    newtreat <- sample(treatvar,nrow(obsdat),replace=FALSE)
    mod <- lm(respvar ~ newtreat,data=obsdat)
    anovamod <- summary(aov(mod))
    Fperm[i] <- anovamod[[1]]$"F value"[1]
  }
  result=sum(Fperm >= Fobs)
  return(list(result, Fperm))
}

test1 <- permutation.test(obsdat=anaerobic, 
                          respvar=anaerobic$Anaerobic_Threshold, 
                          treatvar=anaerobic$Athlete_Type,
                          numsims=10000)

hist(test1[[2]], breaks=50, col='grey', main="Permutation Distribution of Variance Ratio", 
     xlab='variance ratio',xlim=c(1,30))
abline(v=Fobs, lwd=3, col="red")

perm_pvalue <- test1[[1]]/10000

perm_pvalue

#problem 6 bootstrap

bootf <- function(dat,sampsize,B){
  meanvec <- 1:B
  
  for (i in 1:B){
    samp <- sample(dat,sampsize, replace=TRUE)
    mnsamp <- mean(samp)
    meanvec[i] <- mnsamp
  } # end of i loop
  return (list(meanvec))
} #end of function bootf


options(digits=4)

thresh_swim_dat <- anaerobic[anaerobic$Athlete_Type=="Dist_Swim",]
thresh_swim_dat_vals <- thresh_swim_dat$Anaerobic_Threshold

thresh_cycle_dat <- anaerobic[anaerobic$Athlete_Type=="Dist_Swim",]
thresh_cycle_dat_vals <- thresh_cycle_dat$Anaerobic_Threshold

swimboot <- bootf(thresh_swim_dat_vals,5,10000)
cycleboot <- bootf(thresh_cycle_dat_vals,5,10000)

bootdiff_swim_cycle <- swimboot[[1]]-cycleboot[[1]]
conf_int_diff_sw_cy <- quantile(bootdiff_swim_cycle,c(0.025,0.975))
conf_int_diff_sw_cy

#problem 7 power
#MSE from raw analysis = 182.3
groupmeans <- anaerobic_summary$mngroup
5*var(groupmeans)

ybargrand <- mean(groupmeans)
ssq_bet <- sum((anaerobic_summary$mngroup-ybargrand)^2)
sig_bet <- ssq_bet/4

cohen <- sqrt(sig_bet/182.3)
library(pwr)
pwr.anova.test(k=4,n=5,f=cohen,sig.level=0.05,
               power=NULL)

pwr.anova.test(k=4,n=NULL,f=cohen,sig.level=0.05,
               power=0.9)

msqbet <- (5*sum((anaerobic_summary$mngroup-ybargrand)^2))/3

p <- power.anova.test(groups = length(groupmeans), 
                      between.var = var(groupmeans), within.var = 182.3, 
                      power=NULL,sig.level=0.05,n=5)
p





