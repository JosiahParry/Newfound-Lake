#import data
threats <- read.csv("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Newfound_GIT/Data/clean_threats.csv")

#libraries
require(ggplot2)
require(dplyr)
require(cowplot)
#re order levels 
levels(threats$education) <- c("High School", "Associates", "Bachelors", "Masters", "PhD or Higher")


boxplot(threats$soil_quality ~ threats$education)

sq <- ggplot(na.omit(threats)) + geom_boxplot(aes(x = education, y = soil_quality))
wq <- ggplot(na.omit(threats)) + geom_boxplot(aes(x = education, y = waterquality))
wl <- ggplot(na.omit(threats)) + geom_boxplot(aes(x = education, y = wildlife))
rec <- ggplot(na.omit(threats)) + geom_boxplot(aes(x = education, y = recreation))
scen <- ggplot(na.omit(threats)) + geom_boxplot(aes(x = education, y = scenic))
tour <- ggplot(na.omit(threats)) + geom_boxplot(aes(x = education, y = tourism))
bus <- ggplot(na.omit(threats)) + geom_boxplot(aes(x = education, y = business))
rural <- ggplot(na.omit(threats)) + geom_boxplot(aes(x = education, y = rural))

ggplot(na.omit(threats)) + geom_boxplot(aes(y = rural))

plot_grid(sq, wq, wl, rec, scen, tour, bus, rural, ncol = 2)