library(dplyr)
summary(consv)


#import data
consv <- read.csv("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Newfound_GIT/Data/clean_conservation.csv")
#Partition Data by NLRA membership.
#Want to see if there are diferences between members / non-members
nonmmbr <- consv %>% filter(NLRA_Member == "No")
member <- consv %>% filter(NLRA_Member == "Yes")

#sumamrise both
summary(member)
summary(nonmember)

#Explore Differences in Demographics

#Check M/F Differences: do this by using a prop test
#find counts of male "successes" each df
p1 <- member %>% filter(Gender == "Male") %>% summarize(n = n())
p2 <-nonmember %>% filter(Gender == "Male") %>% summarize(n = n())

prop.test(x = c(p1$n, p2$n), n = c(dim(member)[1], dim(nonmmbr)[1]), correct = FALSE)
#Not statistically different, but at glance different. 
#10% chance that I will find it as different or more different than current proportion
#There is no significant different in gender distribution between members & non members
#--------------------------------------------------------------------------------------
# Lets Look at actual responses 

#treating categorical responses as continuous helps us generalize results better
#we can also preform more rigorous statistical tests when treates as continuous

#Since we can treat as continuous we can calculate means and SD, then preform t-test to test differences

#Recreation
t.test(member$recreation, nonmmbr$recreation)
  #Insignificant

#Ecosystem
t.test(member$ecosystem, nonmmbr$ecosystem)
  #Insignificant

#Economic
t.test(member$economic, nonmmbr$economic)
  #approaching significance. This should reach a significant p value with a larger sample of non-members
  #this may be explained 

#Viewshed
t.test(member$viewshed, nonmmbr$viewshed)
  #Insignificant

#historic
t.test(member$historic, nonmmbr$historic)
  #Insignificant

#culture
t.test(member$culture, nonmmbr$culture)
  #Insignificant

#No difference betweeen members and non-members

#----------------------------------------------------------------------
#ANOVA for EDUCATIONAL Differences


#Use ANOVA to differentiate by education
rec <- (aov(recreation ~ education, data = consv))
eco <- (aov(ecosystem ~ education, data = consv))
econ <- (aov(economic ~ education, data = consv))
view <- (aov(viewshed ~ education, data = consv))
historic <- (aov(historic ~ education, data = consv))
culture <- (aov(culture ~ education, data = consv))
#all significantly different between some educational groups
#need to preform a post hoc test to see the actual difference

#Identify the differences between groups / which groups are signficantly difference 
summary(rec) #significant
summary(eco) #significant
summary(econ) #signficant
summary(view) #signficant 
summary(historic) #significant
summary(culture) #significant

recposthoc <- TukeyHSD(x=rec, "education", conf.level = 0.95, data = (consv))
  #difference between PhD & Bachelor
  #Difference between PhD & Master
  #Difference between PhD & Associates
  #NO Difference between High Schoolers

ecoposthoc <- TukeyHSD(x=eco, "education", conf.level = 0.95, data = (consv)) 
  #Bachelor & associate difference 

economicposthoc <- TukeyHSD(x=econ, "education", conf.level = 0.95, data = na.omit(consv))
  #Difference between Bachelors & Associate
  #Difference between Masters & Associates
  #Differecnce Between PhD & Associates
  #Difference between Masters & Bachelors
  #Somewhat different between PhD & Masters

viewposthoc <- TukeyHSD(x=view, "education", conf.level = 0.95, data = na.omit(consv))
  #Nothing Significantly Different

historicposthoc <- TukeyHSD(x=historic, "education", conf.level = 0.95, data = na.omit(consv))
  #Difference between PhD & Associates

cultureposthoc <- TukeyHSD(x=culture, "education", conf.level = 0.95, data = na.omit(consv))
  #Difference between Masters & Associates
  #Difference between Masters & Bachelors


  

#-----------------------------------------------------------------------
#recpost hoc
#PhD > believe that recreation needs to be conserved less than do masters and bachelors & associates
    #all other higher ed  except high schoolers
#show plots comparing significantly difference groups and their values
  #subset data by education create box plots by subsets 
  #potentially show estimated differences in means -differences means group on left rates less important 
#low values / low concern might indicate groups that would need better targeting
#create a boxplot for all categories on same plot

#-----------------------------------------------------------------------

#nothing significantly different
#just show differences in mean scores for recreation subsetted by academic group
#potentially a mosaic potentially determined by p-value or mean score