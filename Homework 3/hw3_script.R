# Beckham Carver
# STAT 4015
# Prof. Robinson
# Homework 3

ppr <- read.csv("PPR_2022.csv", header = T)
trout <- read.csv("trout_hatchery_22.csv", header = T)

library(tidyverse)

# Part 1:

#psuedocode:
  # define ppr_prob1, using ppr for data
  # filter for Low Diversity, or Native Sod
  # filter remaining for year 2018
  # group data structure by WPA (Unit_Name && Habitat_type)
  # analyze with mean and SD of litter depth

ppr_prob1 <- ppr %>%
  filter((Habitat_Type=="Low Diversity Reconstruction"
         |Habitat_Type=="Native Sod")
          & Monitoring_Year==2018
           & Prioritization=="High")%>%
  group_by(Unit_Name,Habitat_Type)%>%
  summarise(count=n(),
            mnlitter=mean(LitterDepth,na.rm=T),
            sdlitter_trans=sd(LitterDepth,na.rm=T))


# Part 2 (A)

#psuedocode:
# define trout_prob2, using trout data
# group by both diet and raceway
# within these groups summarize length into a mean
# remove the count column
# rename columns

trout_prob2 <- trout %>%
  group_by(Diet,raceway)%>%
    summarise(count=n(),mn_length=mean(Length_6w_mm,na.rm=T))%>%
      mutate(count = NULL)

colnames(trout_prob2) <- c("Diet","Raceway","Mean Length (6 weeks)")

# Part 2 (D)

# filtering downs to the two needed diets
trout_prob2D <- trout_prob2 %>%
  filter(Diet=="A" || Diet=="B")

#renaming for convenience
names(trout_prob2D)[3] <- 'mnLength'

t.test(mnLength ~ Diet, var.equal=F, data=trout_prob2D)

