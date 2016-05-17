library(dplyr)
require(ggplot2)
require(cowplot)



#import data
consv <- read.csv("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Newfound_GIT/Data/clean_conservation.csv")
#Partition Data by NLRA membership.
#Want to see if there are diferences between members / non-members
nonmmbr <- consv %>% filter(NLRA_Member == "No")
member <- consv %>% filter(NLRA_Member == "Yes")

summary(consv)

#Visualizations

#Economic
membereconviz <- na.omit(member) %>% 
  ggplot(aes(economic)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#feffa3") + 
  labs(x = "", y = "") +
  scale_x_discrete(limits = c(0,1,2,3), labels = c("Not Important", "Low", "Medium", "High")) + ylim(0,.4)

nonmmbreconviz <- na.omit(nonmmbr) %>% 
  ggplot(aes(economic)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#feffa3") +
  scale_x_discrete(limits = c(0,1,2,3), labels = c("Not Important", "Low", "Medium", "High")) + ylim(0,.4) +
  ylab("") + xlab("")

econviz <- plot_grid(membereconviz, nonmmbreconviz)
econviz

ggsave("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Newfound_GIT/R_Files/Images/Conservation/economy.png", 
       plot = last_plot(), height = 5, width = 8.5, units = c("in"), dpi = 300)

#####


#Recreation
memberrecviz <- na.omit(member) %>% 
  ggplot(aes(recreation)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#FFFF7F") + 
  labs(title = "Recreation (Members)") +
  scale_x_discrete(limits = c(0,1,2,3), labels = c("Not Important", "Low", "Medium", "High")) + 
  xlab("Level of Threat") + ylab("Proportion of Respondents") + ylim(0,.8)

nonmmbrrecviz <- na.omit(nonmmbr) %>% 
  ggplot(aes(recreation)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#FFFF7F") + 
  labs(title = "Recreation (Non-Members)") +
  scale_x_discrete(limits = c(0,1,2,3), labels = c("Not Important", "Low", "Medium", "High")) + 
  xlab("Level of Threat") + ylab("") + ylim(0,.8)

recviz <- plot_grid(memberrecviz, nonmmbrrecviz)
recviz
#####

#Ecosystem
memberecoviz <- na.omit(member) %>% 
  ggplot(aes(ecosystem)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#feffa3") + 
  scale_x_discrete(limits = c(1,2,3), labels = c("Low", "Medium", "High")) + 
  xlab("") + ylab("") + ylim(0,.9)

nonmmbrecoviz <- na.omit(nonmmbr) %>% 
  ggplot(aes(ecosystem)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#feffa3") +
  scale_x_discrete(limits = c(1,2,3), labels = c("Low", "Medium", "High")) + 
  xlab("") + ylab("") + ylim(0,.9)

ecosystemviz <- plot_grid(memberecoviz, nonmmbrecoviz)
ecosystemviz

ggsave("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Newfound_GIT/R_Files/Images/Conservation/ecosystem.png", 
       plot = last_plot(), height = 5, width = 8.5, units = c("in"), dpi = 300)
#----------------------------------------------------------------------------------------------
