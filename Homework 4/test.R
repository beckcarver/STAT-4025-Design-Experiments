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
            varsamp=var(grpmns))

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

#Need to do residual analysis after every ANOVA analysis
#residuals() function will create residuals for every point

trout_summary$resids <- residuals(m1)

{qqnorm(trout_summary$resids,xlab="Normal Quantiles",
        ylab="Model Residuals")
  qqline(trout_summary$resids)}

shapiro.test(trout_summary$resids)

#Need to do a natural log transformation on the raceway
#means and re-run our ANOVA

trout_summary$logmeans <- log(trout_summary$grpmns)

m2 <- lm(logmeans~Diet,data=trout_summary)
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

#Now that we see that there is a difference in the group means
#need to use Tukey's to find out which means are different
#Need the 'multcomp' library and the glht() function

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


