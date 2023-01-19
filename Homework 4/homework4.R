library(tidyverse)
library(ggplot2)

trout <- read.csv("trout_hatchery_22.csv",header=TRUE)

trout_p1 <- trout %>%
  group_by(Diet,raceway)%>%
  summarise(count=n(),mn_length=mean(Length_6w_mm,na.rm=T))

m1<-lm(mn_length~Diet, data = migraine)


trout_p1$Ind_Var <- ifelse(trout_p1$Diet == 'A', 1,0)

p1_lm <- lm(mn_length~Ind_Var,data=trout_p1)

summary(p1_lm)
