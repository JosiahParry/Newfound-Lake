threats <- read.csv("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Data/threats.csv")
demographics <- read.csv("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Data/demographics.csv")

#My Threat Values are measured on an ordinal scale
#However, they came out as binary for each level in the scale
#I need to condense this into 1 common field


#creating empty columns to fill
threats$soil_quality <- c(rep(NA,dim(threats)[1]))
threats$waterquality <- c(rep(NA,dim(threats)[1]))
threats$wildlife <- c(rep(NA,dim(threats)[1]))
threats$recreation <- c(rep(NA,dim(threats)[1]))
threats$scenic <- c(rep(NA,dim(threats)[1]))
threats$tourism <- c(rep(NA,dim(threats)[1]))
threats$business <- c(rep(NA,dim(threats)[1]))
threats$rural <- c(rep(NA,dim(threats)[1]))

#Translating Soil Quality
soilhigh <- which(threats$soil_quality_highly == 1)
soilmoderate <- which(threats$soil_quality_moderate == 1)
soilslight <- which(threats$soil_quality_slightly == 1)
soilnot <- which(threats$soil_quality_not_threatened == 1)

#populating soil quality
threats$soil_quality[soilhigh] <- 3
threats$soil_quality[soilmoderate] <- 2
threats$soil_quality[soilslight] <- 1
threats$soil_quality[soilnot] <- 0


#translating water quality
waterhigh <- which(threats$waterquality_highly == 1)
watermoderate <- which(threats$waterquality_moderately == 1)
waterslight <- which(threats$waterquality_slightly == 1)
waternot <- which(threats$waterquality_not == 1)

#populating waterquality
threats$waterquality[waterhigh] <- 3
threats$waterquality[watermoderate] <- 2
threats$waterquality[waterslight] <- 1
threats$waterquality[waternot] <- 0

#translating wildlife
wildhigh <- which(threats$wildlife_highly == 1)
wildmoderate <- which(threats$wildlife_moderately == 1)
wildslight <- which(threats$wildlife_slightly ==1)
wildnot <- which(threats$wildlife_not_threatened == 1)

#populating wildlife
threats$wildlife[wildhigh] <- 3
threats$wildlife[wildmoderate] <- 2
threats$wildlife[wildslight] <- 1
threats$wildlife[wildnot] <- 0

#translating recreation
rechigh <- which(threats$recreation_highly ==1)
recmoderate <- which(threats$recreation_moderately == 1)
recslight <- which(threats$recreation_slightly == 1)
recnot <- which(threats$recreation_not_threatened == 1)

#populating recreation
threats$recreation[rechigh] <- 3
threats$recreation[recmoderate] <- 2
threats$recreation[recslight] <- 1
threats$recreation[recnot] <- 0

#translating scenic 
scenichigh <- which(threats$recreation_highly == 1)
scenicmoderate <- which(threats$recreation_moderately == 1)
scenicslight <- which(threats$recreation_slightly == 1)
scenicnot <- which(threats$recreation_not_threatened == 1)

#populating scenic 
threats$scenic[scenichigh] <- 3
threats$scenic[scenicmoderate] <-2 
threats$scenic[scenicslight] <- 1
threats$scenic[scenicnot] <- 0

#translating tourism
tourhigh <- which(threats$tourism_highly == 1)
tourmoderate <- which(threats$tourism_moderately == 1)
tourslight <- which(threats$tourism_slightly == 1)
tournot <- which(threats$tourism_not == 1)

#populating tourism 
threats$tourism[tourhigh] <- 3
threats$tourism[tourmoderate] <- 2
threats$tourism[tourslight] <- 1
threats$tourism[tournot] <- 0

#translating business
bushigh <-which(threats$business_highly == 1)
busmoderate <- which(threats$business_moderately == 1)
busslight <- which(threats$business_slightly == 1)
busnot <- which(threats$business_not == 1)

#populating business
threats$business[bushigh] <- 3
threats$business[busmoderate] <- 2
threats$business[busslight] <- 1
threats$business[busnot] <- 0

#translating rural
ruralhigh <- which(threats$rural_highly == 1)
ruralmoderate <- which(threats$rural_moderately == 1)
ruralslight <- which(threats$rural_slightly == 1)
ruralnot <- which(threats$rural_not == 1)

#populating rural
threats$rural[ruralhigh] <- 3
threats$rural[ruralmoderate] <- 2
threats$rural[ruralslight] <- 1
threats$rural[ruralnot] <- 0

#Creating new DF with only clean Data 
clean_threats <- threats[, c(1:9, 47:54)]

#add demographics
clean_threats <- merge(demographics, clean_threats, by="respondent", all.y=TRUE)

#Write CSV
write.csv(clean_threats, file = ("/Users/Josiah/Google Drive/College/Spring 2016/PPGIS/Data/clean_threats.csv"))
