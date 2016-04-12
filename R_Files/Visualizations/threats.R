library(ggvis)
library(ggplot2)
library(cowplot)
library(dplyr)
threats <- read.csv("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Newfound_GIT/Data/clean_threats.csv")
demo <- read.csv("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Newfound_GIT/Data/demographics.csv")

#Overall Summary
summary(demo)
#Most respondents were college graduates. Mostly people with their Bachelors or Masters. 
#Most were over the age of 44. 87% of respondents
  #51 between 45 - 64 yrs
  #50 between 65 - 84 yrs 
#Our survey will show bias in these age ranges and education levels
  #Is this bias the group of individuals we wanted to target?

#Summary of Females
demo %>% filter(Gender == "Female") %>% summary(demo)
#41 females 
#30 college educated -- over 50% -- only 4 just with a highschool degree
#Majority of female respondents are between 45-64 yrs (21)
  #Second largest group are 65-84yrs 
    #This trend is 9

#-----------Visualizations-------------------
#Demographics
#edit the order of education factor levels
order <- c("High School", "Associates", "Bachelors", "Masters", "PhD or Higher")
#Only show gendered answers
demo <- demo %>%
    filter(Gender != "")
ed <- ggplot(na.omit(demo), aes(education)) + geom_bar() + aes(fill = Gender) +
  scale_x_discrete(limits = order) + facet_wrap(~Gender)
 

age <- ggplot(na.omit(demo), aes(Select.your.age.range, fill = Gender)) + geom_bar() + facet_grid(~Gender)
  #me and women tend to be between 45 -84
  #women 45-64 & men 64-84
  #no women 18-24

#NLRA Membership
mmbr <- demo %>% filter(NLRA_Member != "", Gender != "")
NLRA <- ggplot(na.omit(mmbr), aes(NLRA_Member, fill = Gender)) + geom_bar() + facet_wrap(~Gender)
NLRA

#Residents
res <- demo %>% filter(NH_Resident_Yes2 != "")
NHResident <- ggplot(na.omit(res), aes(NH_Resident_Yes2, fill = Gender)) + geom_bar() + facet_wrap(~Gender)


#Threats 
sq <- ggplot((na.omit(threats)), aes(factor(soil_quality)))+ geom_bar() + facet_grid(~Gender) + aes(fill = Gender)
wq <- ggplot((na.omit(threats)), aes(factor(waterquality))) + geom_bar()+ facet_grid(~Gender) + aes(fill = Gender)
wl <- ggplot((na.omit(threats)), aes(factor(wildlife))) + geom_bar()+ facet_grid(~Gender) + aes(fill = Gender)
rec <- ggplot((na.omit(threats)), aes(factor(recreation))) + geom_bar() + facet_grid(~Gender) + aes(fill = Gender)
scen <- ggplot((na.omit(threats)), aes(factor(scenic))) + geom_bar() + facet_grid(~Gender) + aes(fill = Gender)
tour <- ggplot((na.omit(threats)), aes(factor(tourism))) + geom_bar() + facet_grid(~Gender) + aes(fill = Gender)
biz <- ggplot((na.omit(threats)), aes(factor(business))) + geom_bar() + facet_grid(~Gender) + aes(fill = Gender)
rur <- ggplot((na.omit(threats)), aes(factor(rural))) + geom_bar() + facet_grid(~Gender) + aes(fill = Gender) 


#Generating Grid of all threats and frequency counts
allthreats <- plot_grid(sq,wq,wl,rec, scen, tour, biz, rur, ncol = 2)
ggsave("allthreats.pdf",
       width = 8.5,
       height = 10.5,
       plot = allthreats, 
       path = c("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Newfound_GIT/Graphs"))



#plotting waterquality values based on education (?)
  ggplot(na.omit(threats), aes(waterquality)) + geom_bar() + facet_wrap(~ Gender) + aes(fill = Gender) + 
  scale_x_discrete(limits = c("High School", "Associates", "Bachelors", "Masters", "PhD or Higher"))

ggplot(na.omit(threats), aes(education, fill = Gender)) + geom_bar(position = "dodge") + facet_grid(~Gender)
