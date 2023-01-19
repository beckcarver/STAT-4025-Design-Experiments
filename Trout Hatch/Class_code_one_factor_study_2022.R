#WPA data to illustrate RATG

wpa_dat <- read.csv("WPA_manage.csv",header=TRUE)

#Next line determines which WPAs will be assigned
#to which management group (Burn, Herbicide, Graze)

wpa_dat$rand_assign <- sample(15,15,replace=FALSE)

#Will use trout_hatchery_22.csv file to demonstrate
#graphical and statistical analysis of a one-factor
#experiment

trout <- read.csv("trout_hatchery_22.csv",header=TRUE)

library(tidyverse)

#trout_summary below contains the data which will be
#used in our statistical analysis comparing all 
#four Diet groups in terms of mean gain in length

trout_summary <- trout %>%
     group_by(Diet,raceway) %>%
     summarise(grpmns = mean(Length_6w_mm),
               grpstd = sd(Length_6w_mm))

write.csv(trout_summary,"trout_summary.csv")

trout_A_B <- trout_summary %>%
             filter(Diet=="A"|Diet=="B")
trout_A_B$indA <- (trout_A_B$Diet=="A")*1

m1 <- lm(grpmns~indA,data=trout_A_B)
anova(m1)


#QQ Plots by Level

#we need to transform the 'Diet' variable from the 'chr'
#format to a 'factor' format and we do this using
#the as.factor() function below

trout_summary$Diet <- as.factor(trout_summary$Diet)

par(mfrow=c(2,2))
for(i in levels(trout_summary$Diet)){
  tmp <- with(trout_summary, grpmns[Diet==i])
  qqnorm(tmp,xlab="Mean Growth",main=paste("Diet",i))
  qqline(tmp)
  }

par(mfrow=c(1,1))

#Next line provides boxplots for each group

boxplot(trout_summary$grpmns~trout_summary$Diet,
        xlab="Diet Group",ylab="Length Gain (mm)")

#Next lines use tidyverse to get means and sample
#variances for each Diet level

num_summaries <- trout_summary %>%
          group_by(Diet)%>%
          summarise(mnsamp = mean(grpmns),
                    varsamp=var(grpmns),
                    logmnsamp=log(mnsamp))

mean(num_summaries$mnsamp)

var_ratio <- max(num_summaries$varsamp)/min(num_summaries$varsamp)
var_ratio

#plot the raceways/tubs Length Gains vs. Diet Group

meanA <- num_summaries$mnsamp[1]
meanB <- num_summaries$mnsamp[2]
meanC <- num_summaries$mnsamp[3]
meanD <- num_summaries$mnsamp[4]

ggplot(trout_summary,aes(x=Diet,y=grpmns,color=Diet))+
      geom_point()+
      geom_hline(yintercept=meanB,color="dark green")

trout_summary$Diet <- as.factor(trout_summary$Diet)

m1 <- lm(grpmns~Diet,data=trout_summary)
anova(m1)

#with a pvalue < 0.05, we want to know which means
#are different...
#suppose we want to compare Diet A to B,C, and D
#There are 3 comparisons to make and so a Bonferroni
#approach uses an alpha of 0.05/3 = 0.0167

#For a confidence interval, you divide 0.0167/2 to 
#get 0.0083

#To get the t-multiplier, you need 16 df from the MSE
#and we use the 0.9917 quantile

qt(p=0.9917, df=16, lower.tail = TRUE, log.p = FALSE)

#Need to do residual analysis after every ANOVA analysis
#residuals() function will create residuals for every point

trout_summary$resids <- residuals(m1)

{qqnorm(trout_summary$resids,xlab="Normal Quantiles",
        ylab="Model Residuals")
qqline(trout_summary$resids)}

shapiro.test(trout_summary$resids)

pf(q=30.65,df1=3,df2=16,lower.tail=FALSE)
x <- seq(0,4,.01)
y <- df(x,df1=3,df2=16)

qf(p=0.05,df1=3,df2=16,lower.tail=FALSE)

plot(x,y)

#x would represent MSTRT/MSE = F-ratio - in our case F-ratio = 30.65

#Need to do a natural log transformation on the raceway
#means and re-run our ANOVA

#always make sure that your treatment variable is of
#a factor variable type

trout_summary$Diet <- as.factor(trout_summary$Diet)

trout_summary$logmeans <- log(trout_summary$grpmns)

m2 <- lm(logmeans~Diet,data=trout_summary)
anova(m2)

#Let's get our numerical summaries for the log-trans
#data...note that to use the select() function, if you have
#already used the MASS library, you need to explicitly tell
#R to use the select() function from the dyplr package as 
#I am doing in line 133

ln_num_summaries <- trout_summary %>%
  dplyr::select(Diet,logmeans) %>%
  group_by(Diet) %>%
  summarise(mnlogsamp = mean(logmeans))

trout_summary$residsm2 <- residuals(m2)

shapiro.test(trout_summary$residsm2)

{qqnorm(trout_summary$residsm2,xlab="Normal Quantiles",
        ylab="Model Residuals")
  qqline(trout_summary$residsm2)}

boxplot(trout_summary$residsm2,ylab="Model 2 Residuals")

#Now that residuals look good, let's look at our ANOVA table

anova(m2)

#Let's take a look at the side by side boxplots of the 
#log Length Gains

boxplot(logmeans~Diet,data=trout_summary)

#Need the 'multcomp' library and the glht() function to use 
#Tukey's for all pairwise comparisons

library(multcomp)
anovm2 <- aov(m2)

#Below is Tukey comparison of grouped log means

tukecomp <- glht(anovm2,linfct=
                   mcp(Diet='Tukey'))

tukecomp

qtukey(p=0.95, nmeans=4, df=16)
summary(tukecomp)
plot(tukecomp)
confint(tukecomp)

Quantile <- qtukey(p=0.95, nmeans=4, df=16)/sqrt(2)
Quantile

# Need to backtransform the Tukey confidence intervals to the 
# raw scale and then understand the final interpretation
# the following code shows the relation between the log
# of the means vs. taking the mean of the log raceway values

library(tidyverse)
num_summaries2 <- trout_summary %>%
  group_by(Diet)%>%
  summarise(mnsamp = mean(grpmns),
            varsamp = mean(grpmns),
            mnlogsamp = mean(logmeans),
            varlogsamp=var(logmeans))%>%
  mutate(logmnsamp = log(mnsamp))

write.csv(num_summaries2,"meanlog_logmean.csv")

#Tukey confidence intervals of difference in grouped log
#means is in conf_results

conf_results <- confint(tukecomp)

#conf_results_data is converted to a data frame so that
#we can easily back-transformed the log differences

conf_results_data <- data.frame(conf_results$confint)
conf_results_new <- conf_results_data %>%
  mutate(backmean = exp(Estimate),
         backlower = exp(lwr),
         backupper = exp(upr))


write.csv(conf_results_new,"confidence_intervals_trout.csv")

################permutation test begin ########################
#For permutation test, you don't need to transform the data
#since this is a non-parametric approach

m1 <- lm(grpmns~Diet,data=trout_summary)
anovam1 <- summary(aov(m1))
anovam1

Fobs <-  anovam1[[1]]$"F value"[1]
Fobs

#Permutation test function

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

dietperm <- permutation.test(obsdat=trout_summary, 
                                   respvar=trout_summary$grpmns, 
                                   treatvar=trout_summary$Diet,
                                   numsims=10000)

hist(dietperm[[2]], breaks=50, col='grey', main="Permutation Distribution of Variance Ratio", 
     xlab='variance ratio',xlim=c(1,30))
abline(v=Fobs, lwd=3, col="red")

perm_pvalue <- dietperm[[1]]/10000
perm_pvalue

########bootstrapping to find out if there are pairwise differences ##

bootf <- function(dat,sampsize,B){
  meanvec <- 1:B
  
  for (i in 1:B){
    samp <- sample(dat,sampsize, replace=TRUE)
    mnsamp <- mean(samp)
    meanvec[i] <- mnsamp
  } # end of i loop
  return (list(meanvec))
} #end of function bootf

DietA <- trout_summary[trout_summary$Diet=="A",]

DietB <- trout_summary[trout_summary$Diet=="B",]

DietA_boot <- bootf(dat=DietA$grpmns,5,1000)
DietB_boot <- bootf(dat=DietB$grpmns,5,1000)

bootdiff_B_A <- DietB_boot[[1]]-DietA_boot[[1]]

conf_int_B_A <- quantile(bootdiff_B_A,c(0.025,0.975))
conf_int_B_A





