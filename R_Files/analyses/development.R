#load packages and data

library(dplyr)
dev <- read.csv("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Newfound_GIT/Data/clean_development.csv")

#-----------------------------------------------------------------------
#Partition Data by NLRA membership.
#Want to see if there are diferences between members / non-members


#Creating DF with members n = 47
member <- dev %>% filter(NLRA_Member == "Yes")
#Creating DF with non-members. n = 27
nonmmbr <- dev %>% filter(NLRA_Member == "No")
#Created DF with empty responses, n = 10
NAs <- dev %>% filter(is.na(NLRA_Member))

#-----------------------------------------------------------------------
#Summarise both data
summary(member)
summary(nonmmbr)
#Want to check gender differences between members & non member responses 

p1 <- member %>% filter(Gender == "Male") %>% summarize(n = n())
p2 <-nonmmbr %>% filter(Gender == "Male") %>% summarize(n = n())

prop.test(x = c(p1$n, p2$n), n = c(dim(member)[1], dim(nonmmbr)[1]), correct = FALSE)

#Responses by gender proportion significantly different (p = 0.02882)
#members are 70% male, 30% female
#nonmembers are 45% male, 55% female
#this is just good to note, as some differences may be caused by gendered difference 

#----------------------------------------------------------------------------------
#Now time to compare responses 
#where things are different, we need to note this, and hypothesize why
#where they are the same, we can note that these are places / things that 
#ALL respondents believe should be developed at the same rate

#Housing
#high density appartments
t.test(member$highdensity_apartments, nonmmbr$highdensity_apartments)
      #extremely different. p = 0

#high density single family homes
t.test(member$highdensity_singlefamily, nonmmbr$highdensity_singlefamily)
  #significantly different
  # p = 0.005    
    #most differences occur between high density in regards to membership
    #members may be more apt to want to preserve the area around and keep it more

#rural density  less than 10 acrres
t.test(member$ruraldensity_lessthan10, nonmmbr$ruraldensity_lessthan10)
  #insignificant

#rural density 10> acres
t.test(member$ruraldensity_10plus, nonmmbr$ruraldensity_10plus)
  #insignificant

#Workforce Housing
t.test(member$workforce_housing, nonmmbr$workforce_housing)
  #insignificant not many people at all think this should be developed

#accessory apartment housing
t.test(member$accessory_apartments, nonmmbr$accessory_apartments)
  #insignificant. only 10% of members would like to see this developed
  # 4% of non-members would like to see this developed
#recreation
t.test(member$recreation, nonmmbr$recreation)
  #insignificant

#Small Business
t.test(member$small_business, nonmmbr$small_business)
  #insignicant

#commercial business
t.test(member$commercial_business, nonmmbr$commercial_business)
  #SIGNIFICANT, femer non-members want commercial development
    #small business is more expensive? locals want easier / cheaper access to goods?


#-----------------------------------------------------------------
#See total percents in favor of a certain type of development for (non)/members
summary(dev[,16:24], na.omit = TRUE)

#high density apartments = 17%
#high density single family = 31%
#rural > 10 acres = 31%
#rural < 10 acres = 17%
#workforce = 11%
#accessory apartments = 8%

#recreation 29.5%
#small business = 53.5%
#commercial business = 24%

#------------------------------------------------------------------------
#CONCLUSIONS

#High density housing apartments and family
#Significant differences between members and non-members 
  #43% of members would like to see development of single family homes
      #only 12% nonmembers
  #29% of members would like to see apartments developed
  # 0% of non-members woulld like to see development of apartments
