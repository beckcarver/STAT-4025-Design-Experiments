#Next line creates an object named 'ppr' by
#reading the PPR_2022.csv file into RStudio
ppr <- read.csv("PPR_2022.csv",header=T)

#Next line activates the 'tidyverse' library/package

library(tidyverse)

#Next line computes the overall mean litter depth
#mean() function cannot operate on missing values so we
#specify na.rm=T argument to have mean() ignore missing obs.

meandepth <- mean(ppr$LitterDepth,na.rm=T)
#new variable <- take the mean of(select ppr <- select litter depth <- skip missing obsv.)

#There are 20 quadrats for each transect. The '|' symbol is a logical
#operator which means 'or'. We are including habitats
#High Diversity Reconstruction or Native Sod areas
#The group_by() function sets the data up to have summaries for
#every combination of Transect_Name, Habitat_Type and Monitoring
#Year. The summarise() function specifies the summaries of
#interest. n() function counts number of data points for
#each combo of Transect_Name, Habitat_Type and Monitoring_Year

ppr_trans <- ppr %>%
             filter(Habitat_Type=="High Diversity Reconstruction"
                       |Habitat_Type=="Native Sod")%>%
             group_by(Transect_Name,Habitat_Type,
                      Monitoring_Year)%>%
             summarise(count=n(),
               mnlitter=mean(LitterDepth,na.rm=T),
                        sdlitter_trans=sd(LitterDepth,na.rm=T))

ppr_practice <- ppr %>%
  filter(Habitat_Type=="Low Diversity Reconstruction"
         |Habitat_Type=="Native Sod")%>%
  filter(Monitoring_Year==2018)%>%
  group_by(Unit_Name,Habitat_Type)%>%
  summarise(count=n(),
            mnlitter=mean(LitterDepth,na.rm=T),
            sdlitter_trans=sd(LitterDepth,na.rm=T))

t.test()

