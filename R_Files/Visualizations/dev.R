require(dplyr)
require(cowplot)
require(ggplot)
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
#Visualize differences in high density apartments
na.omit(dev) %>% 
  ggplot() + geom_bar(aes(highdensity_apartments), fill = "#93eabf") + 
  labs(title = "High Density Apartments (Members)") + facet_wrap(~NLRA_Member)

memberapartments <- na.omit(dev) %>% filter(NLRA_Member == "Yes") %>%
  ggplot() + geom_bar(aes(highdensity_apartments), fill = "#93eabf") + 
  labs(title = "High Density Apartments (Members)") + ylim(0,30)

nonmmbrapartments <-  ggplot(nonmmbr) + geom_bar(aes(highdensity_apartments), fill = "#f6546a") + 
  labs(title = "High Density Apartments (Non-Members)") + ylim(0, 30) + 
  scale_x_discrete(limits = c(0,1), drop = FALSE)


