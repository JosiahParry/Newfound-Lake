#import data
conservation <- read.csv(("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Data/conservation.csv"))
demographics <- read.csv("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Data/demographics.csv")

#create empty fields for each measure
conservation$recreation <- c(rep(NA, dim(conservation)[1]))
conservation$ecosystem <- c(rep(NA, dim(conservation)[1]))
conservation$economic <- c(rep(NA, dim(conservation)[1]))
conservation$viewshed <- c(rep(NA, dim(conservation)[1]))
conservation$historic <- c(rep(NA, dim(conservation)[1]))
conservation$culture <- c(rep(NA, dim(conservation)[1]))

#translate recreation
rechigh <- which(conservation$recreation_high == 1)
recmedium <- which(conservation$recreation_medium == 1)
reclow <- which(conservation$recreation_low == 1)
recnot <- which(conservation$recreation_not_important == 1)

#populate recreation
conservation$recreation[rechigh] <- 3
conservation$recreation[recmedium] <- 2
conservation$recreation[reclow] <- 1
conservation$recreation[recnot] <- 0

#translate ecosystem
ecohigh <- which(conservation$ecosystems___High == 1)
ecomedium <- which(conservation$ecosystems___Medium == 1)
ecolow <- which(conservation$ecosystems___Low == 1)
econot <- which(conservation$ecosystem_not_at_all == 1)

#populate ecosystem
conservation$ecosystem[ecohigh] <- 3
conservation$ecosystem[ecomedium] <- 2
conservation$ecosystem[ecolow] <- 1
conservation$ecosystem[econot] <- 0

#translate economic
economichigh <- which(conservation$economic_high == 1)
economicmedium <- which(conservation$economic_medium == 1)
economiclow <- which(conservation$economic_low == 1)
economicnot <- which(conservation$economic_not_at_all == 1)

#populate economic
conservation$economic[economichigh] <- 3
conservation$economic[economicmedium] <- 2
conservation$economic[economiclow] <- 1
conservation$economic[economicnot] <- 0

#translate viewshed
viewhigh <- which(conservation$viewshed_high == 1)
viewmed <- which(conservation$viewshed_medium == 1)
viewlow <- which(conservation$viewshed_low == 1)
viewnot <- which(conservation$viewshed_not_at_all == 1)

#populate viewshed
conservation$viewshed[viewhigh] <- 3
conservation$viewshed[viewmed] <-2
conservation$viewshed[viewlow] <- 1
conservation$viewshed[viewnot] <- 0

#translate historic
histhigh <- which(conservation$historical_value_high == 1)
histmed <- which(conservation$historical_value_medium == 1)
histlow <- which(conservation$historical_value_low == 1)
histnot <- which(conservation$historical_value_not_at_all == 1)

#populate historic
conservation$historic[histhigh] <- 3
conservation$historic[histmed] <- 2
conservation$historic[histlow] <- 1
conservation$historic[histnot] <- 0

#translate cultural
culturalhigh <- which(conservation$cultural_high == 1)
culturalmedium <- which(conservation$cultural_medium == 1)
culturallow <- which(conservation$cultural_low == 1)
culturalnot <- which(conservation$cultural_not_at_all == 1)

#populate cultural
conservation$culture[culturalhigh] <- 3
conservation$culture[culturalmedium] <- 2
conservation$culture[culturallow] <- 1
conservation$culture[culturalnot] <- 0

#create cleaned data frame
cleaned_conservation <- conservation[, c(1:9, 39:44)]

#add demographics
cleaned_conservation <- merge(demographics, cleaned_conservation, by="respondent", all.y=TRUE)

#write csv
write.csv(cleaned_conservation, file = ("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Data/clean_conservation.csv"))
