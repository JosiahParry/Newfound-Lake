library(dplyr)
summary(consv)

#Partition Data by NLRA membership.
#Want to see if there are diferences between members / non-members
dfno <- consv %>% filter(NLRA_Member == "No")
dfyes <- consv %>% filter(NLRA_Member == "Yes")

#sumamrise both
summary(dfno)
summary(dfyes)

#Explore Differences in Demographics

#Check M/F Differences: do this by using a prop test
#find counts of male "successes" each df
p1 <- dfyes %>% filter(Gender == "Male") %>% summarize(n = n())
p2 <-dfno %>% filter(Gender == "Male") %>% summarize(n = n())

prop.test(x = c(p1$n, p2$n), n = c(dim(dfyes)[1], dim(dfno)[1]), correct = FALSE)
#Not statistically different, but at glance different. 
#10% chance that I will find it as different or more different than current proportion
#There is no significant different in gender distribution between members & non members
#--------------------------------------------------------------------------------------
# Lets Look at actual responses 

#treating categorical responses as continuous helps us generalize results better
#we can also preform more rigorous statistical tests when treates as continuous

#Since we can treat as continuous we can calculate means and SD, then preform t-test to test differences

#Recreation
t.test(dfyes$recreation, dfno$recreation)
#Ecosystem
t.test(dfyes$ecosystem, dfno$ecosystem)

#Economic
t.test(dfyes$economic, dfno$economic)
  #approaching significance. This should reach a significant p value with a larger sample of non-members
  #this may be explained 

#Viewshed
t.test(dfyes$viewshed, dfno$viewshed)

#historic
t.test(dfyes$historic, dfno$historic)

#culture
t.test(dfyes$culture, dfno$culture)



#Use ANOVA to differentiate by education