require(dplyr)
require(cowplot)
require(ggplot)
dev <- read.csv("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Newfound_GIT/Data/clean_development.csv")

#-----------------------------------------------------------------------
#Only visualize Significantly Different Groups based on t.tests in "development_analysis.R"
#Partition Data by NLRA membership.
#Want to see if there are diferences between members / non-members

#Creating DF with members n = 47
member <- dev %>% filter(NLRA_Member == "Yes")
#Creating DF with non-members. n = 27
nonmmbr <- dev %>% filter(NLRA_Member == "No")
#Created DF with empty responses, n = 10
NAs <- dev %>% filter(is.na(NLRA_Member))

#Translate No <- "Non-Member", Yes <- "Member"
levels(dev$NLRA_Member) <- c("Non-Member", "Member")
#-----------------------------------------------------------------------
#Visualize differences in high density apartments



member.apartments <- na.omit(member) %>%
    ggplot(aes(highdensity_apartments)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#ffa24e") +
    scale_x_discrete(limits = c(0,1), labels = c("No", "Yes")) +
    labs(y = "", x = "") + ylim(0,1) 

###Duplicating the first one and editing it in Photoshop to show the second group and add titles

ggsave("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Newfound_GIT/R_Files/Images/apartments.png", plot = last_plot(), height = 5, width = 2.5, units = c("in"), dpi = 300)

#------------------------------------------------------------------------
#Visualize High Density Single Family Homes

#singlefamvis <- na.omit(dev) %>%        ####MADE WITH FACETS BAD REPRESENTATION OF PROPORTIONS OF GROUPS####
 # ggplot(aes(highdensity_singlefamily)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#ffa24e") + 
  #labs(title = "High Density Single Family Homes") + facet_wrap(~NLRA_Member) +
  #scale_x_discrete(limits = c(0,1), labels = c("No", "Yes")) + 
  # labs(x = "Should High Density Single Family Homes Be Developed?", y = "Proportion of Total Respondents")

member.single.family <- na.omit(member) %>%
    ggplot(aes(highdensity_singlefamily)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#ffa24e") +
    scale_x_discrete(limits = c(0,1), labels = c("No", "Yes")) +
    labs(y = "", x = "") + ylim(0,.88) 

non.member.single.family <- na.omit(nonmmbr) %>%
    ggplot(aes(highdensity_singlefamily)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#ffa24e") +
    scale_x_discrete(limits = c(0,1), labels = c("No", "Yes")) + 
  labs(y = "", x = "") + ylim(0,.88) 


plot_grid(member.single.family, non.member.single.family)

ggsave("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Newfound_GIT/R_Files/Images/singlefamily.png", 
       plot = last_plot(), height = 5, width = 5, units = c("in"), dpi = 300)
#------------------------------------------------------------------------
#Visualize Commercial Business
members.commercialviz <-  na.omit(member) %>% 
  ggplot(aes(commercial_business)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#ffa24e") + 
  labs(x = "", y = "") +
  scale_x_discrete(limits = c(0,1), labels = c("No", "Yes")) + ylim(0,.95)

non.members.commercial <-  na.omit(nonmmbr) %>% 
  ggplot(aes(commercial_business)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#ffa24e") + 
  labs( x = "", y = "") +
  scale_x_discrete(limits = c(0,1), labels = c("No", "Yes")) + ylim(0,.95)

plot_grid(members.commercialviz, non.members.commercial)

ggsave("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Newfound_GIT/R_Files/Images/commercialbusiness.png", 
       plot = last_plot(), height = 5, width = 5, units = c("in"), dpi = 300)

#------------------------------------------------------------------------

plot_grid(apartmentvis, singlefamvis, commercialviz, nrow = 3)


