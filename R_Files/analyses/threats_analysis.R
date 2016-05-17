library(dplyr)
threats <- read.csv("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Newfound_GIT/Data/clean_threats.csv")

#partition data
member <- threats %>% filter(NLRA_Member == "Yes") # n = 208 observations
nonmmbr <- threats %>% filter(NLRA_Member == "No") # n = 66 observations
NAs <- threats %>% filter(is.na(NLRA_Member)) # n = 185 no responses

#Sumarise these data
summary(member[,16:23])
summary(nonmmbr[,16:23])
#based on visual insepction I do not anticipate any significant difference btwn (non)/members
#environmental threats are generally homogenously held
#action taken on environmental issues tend to be significantly different
  #I would expect to see most differences in development areas
    #developent is where I think you would see the differences in beliefs that people would act on
#-----------------------------------------------------------------
#Check for gender differences between (non)/member responses

p1 <- member %>% filter(Gender == "Male") %>% summarize(n = n())
p2 <-nonmmbr %>% filter(Gender == "Male") %>% summarize(n = n())

prop.test(x = c(p1$n, p2$n), n = c(dim(member)[1], dim(nonmmbr)[1]), correct = FALSE)
#members: 75% male, 25% female
#nonmmbrs: 64% male, 36% female

#Almost significantly different (p = 0.06). I would consider this a significant difference.
#some responses may differ by gender
#--------------------------------------------------------------------
#Now time to compare responses 
#where things are different, we need to note this, and hypothesize why
#where they are the same, we can note that these are places / things that 
#ALL respondents believe to be equally endangered, or have the same degree of danger

#Soil Quality
t.test(member$soil_quality, nonmmbr$soil_quality)
  #approaching significants (p = 0.08)
  #this is considered between moderate and highly threatened

#Water Quality
t.test(member$waterquality, nonmmbr$waterquality)
  #insignificant
  #insignificant differences, but both think moderate > highly threatened

#Recreation
t.test(member$recreation, nonmmbr$recreation)
  #insignificant
  #think moderately threatend. Means: 1.9, 2.0

#Scenic
t.test(member$scenic, nonmmbr$scenic)
  #insignificant
  #both think moderately threatened. Means: 1.9, 2.0

#Tourism
t.test(member$tourism, nonmmbr$tourism)
  #insignificant
  #both think moderately threatened. Means: 1.95, 1.80

#Business
t.test(member$business, nonmmbr$business)
  #insignificant differences
  #all think slightly threatened. Means: 1.0, 1.1

#Rural
t.test(member$rural, nonmmbr$rural) 
  #significantly different
  #members think that it is moderately threatened. Mean: 2.2
  #nonmembers think rural is moderate / highly threatened. Mean: 2.6
  #could adress both equally. Signficant difference might not be enough to influence methods of appeal
#----------------------------------------------------------------------------------------------
#Proportions of total respondents who think something is moderately or highly threatened
propsoil <- threats %>% filter(soil_quality %in% c(2,3)) %>% summarise(n = n()/nrow(threats)) 



#OVERARCHING QUESTION:
#what differences between non-members and members are most important to communicate?
#how do you communicate differently to adress differences?
#----------------------------------------------------------------------------------------------