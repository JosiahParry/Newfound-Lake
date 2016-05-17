#Libraries & Such
require(cowplot)
require(ggplot2)
library(dplyr)

#Import Data
threats <- read.csv("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Newfound_GIT/Data/clean_threats.csv")
#partition data
member <- threats %>% filter(NLRA_Member == "Yes") # n = 208 observations
nonmmbr <- threats %>% filter(NLRA_Member == "No") # n = 66 observations

#Translate No <- "Non-Member", Yes <- "Member"
levels(threats$NLRA_Member) <- c("Non-Member", "Member")

###########################################################################
#Visualize

memberruralviz <- na.omit(member) %>% 
  ggplot(aes(rural)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#ff6666") + 
  scale_x_discrete(limits = c(0,1,2,3), labels = c("Not at All", "Slightly", "Moderately", "Highly")) + 
  ylim(0, .7) + xlab("") + ylab("")
nonmmbrruralviz <- na.omit(nonmmbr) %>% 
  ggplot(aes(rural)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#ff6666") + 
  scale_x_discrete(limits = c(0,1,2,3), labels = c("Not at All", "Slightly", "Moderately", "Highly")) +
  ylim(0.0, 0.7) + ylab("") + xlab("")

ruralviz <- plot_grid(memberruralviz, nonmmbrruralviz)
ruralviz

ggsave("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Newfound_GIT/R_Files/Images/Threats/rural.png", 
       plot = last_plot(),
       width = 8.5,
       height = 5,
       units = c("in"), dpi = 300)


#-------------------------------------------------------------------------------
#Business **** NOT NECESSARY 

busnonmmbr <- na.omit(nonmmbr) %>% 
  ggplot(aes(business)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#ff6666") + 
  labs(title = "Business (Non-Members)") + ylim(0,.6) +
  scale_x_discrete(limits = c(0,1,2,3), labels = c("Not at All", "Slightly", "Moderately", "Highly")) + ylab("") + xlab("Level of Threat")

busmember <- na.omit(member) %>% 
  ggplot(aes(business)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#ff6666") + 
  labs(title = "Business (Members)") +
  scale_x_discrete(limits = c(0,1,2,3), labels = c("Not at All", "Slightly", "Moderately", "Highly")) + 
  ylab("Proportion of Respondents") + 
  xlab("Level of Threat") + ylim(0,.6)

businessviz <- plot_grid(busmember, busnonmmbr)
businessviz

#-------------------------------------------------------------------------------
#SOIL QUALITY
sqnonmmbr <- na.omit(nonmmbr) %>% 
  ggplot(aes(soil_quality)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#ff6666") +
  scale_x_discrete(limits = c(0,1,2,3), labels = c("Not at All", "Slightly", "Moderately", "Highly")) + ylab("") + 
  xlab("") +ylim(0,.6)

sqmember <- na.omit(member) %>% 
  ggplot(aes(soil_quality)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#ff6666") +
  scale_x_discrete(limits = c(0,1,2,3), labels = c("Not at All", "Slightly", "Moderately", "Highly")) + 
  ylab("") +xlab("") +ylim(0,.6)

wqviz <- plot_grid(sqmember, sqnonmmbr)
wqviz


