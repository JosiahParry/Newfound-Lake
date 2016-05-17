development <- read.csv("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Data/development.csv")
demographics <- read.csv("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Data/demographics.csv")


#combining demographics and development
clean_development <- merge(demographics, development, by="respondent", all.y=TRUE)

write.csv(clean_development, file = ("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Data/clean_development.csv"))