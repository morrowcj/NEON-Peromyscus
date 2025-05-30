


require(dplyr)

require(ggplot2)

require(suncalc)

require(lubridate)




### Load Data ####



# Load the 2024 NEON Capture data from our 8 sites. The capture time of all individual captures with logger pairs present has already been calculated using the 
# "iButton_captimes" R files for each sampling bout at each site during the 2024 season. This has been merged back in with the full trapping data from our 8 sites

dat<-read.csv(file = "C:/Users/allib/Documents/NEON/Analyses/NEON_capdata2022_captimes_3-21-24.csv", stringsAsFactors = TRUE)
# this above NEON_capdata2022_captimes.csv file was created in the merging_captimes.R" file

summary(dat$siteID)

# BLAN HARV MLBS ORNL SCBI SERC STEI UNDE 
# 74  645  284   75  188   76  303  270 

summary(dat$taxonID)

# BLBR     DIVI     GLSP     GLVO     MYGA     NAIN     OTHE     PELE PELEPEMA     PEMA     PESP     SOCI     TAST     ZAHU 
# 75        2        4       15      105       16      113      921      259      321       15        4       45       20 


#### check colnames and delete unused columns so that we can rbind these together


data.frame(colnames(dat))

#### We want to keep the following columns

# [1] "domainID"              "siteID"                "plotID"                "trapCoordinate.x"      "plotType"              "nlcdClass"             "decimalLatitude"      
# [8] "decimalLongitude"      "geodeticDatum"         "coordinateUncertainty" "elevation"             "elevationUncertainty"  "trapStatus"            "trapType"             
# [15] "collectDate"           "endDate.x"             "tagID.x"               "taxonID"               "scientificName"        "sex"                   "recapture"            
# [22] "fate"                  "replacedTag"           "lifeStage"             "testes"                "nipples"               "pregnancyStatus"       "vagina"               
# [29] "hindfootLength"        "earLength"             "tailLength"            "totalLength"           "weight"                "larvalTicksAttached"   "nymphalTicksAttached" 
# [36] "adultTicksAttached"    "tickNumber"            "additionalParasites"   "bloodSampleID"         "fecalSampleID"         "earSampleID"           "hairSampleID"         
# [43] "voucherSampleID"       "iButtonNumber"         "year"                  "month"                 "capturetime_line"      "captime"



# so, the column names below may have to be adjusted depending on whether NEON has changed things - they sometimes do

# dat<-dat[,-c(1, 40:41, 43,44,46,48,49, 51,60, 61,62)]
# 
# colnames(dat)
# 
# dat<-dat[,c(1:37,43, 38:42, 44:46, 49:55)]
# 
# 
# colnames(dat)


# Rename the tag.ID.x variable to be tagID
names(dat)[17]<-paste("tagID")



# Check to make sure there are no duplicated tagIDs (I noticed some in the full NEON mammal box trapping data)
dat$IDunique<-paste(dat$plotID, dat$tagID, sep = "_")

# Create new id thatll be nicer to work with
dat <- transform(dat,                                 
                 id = as.numeric(factor(IDunique)))
dat$id<-as.factor(paste(dat$id))



count(dat[unique(dat$id),]) # number of unique individuals
count(dat[unique(dat$tagID),]) # checks for duplicat tagIDs

dat<-dat %>% group_by(id) %>% mutate(ncap = n()) %>% ungroup()

mean(dat$ncap) # mean number of captures per individual
sd(dat$ncap) # SD



### Make sure that all datetime entries are in the same format
dat$captime<-parse_date_time(dat$captime, orders = c('mdy_HM', 'ymd_HMS'))


dat$capture_date<-date(dat$captime)

dat$capture_time<-format(as.POSIXct(dat$captime), format = "%H:%M:%S")





### HPS & Proportion night elapsed ####

# I want to estimate capture time in hours past sunset format and also terms of 
# the proportion of the night that has elapsed. So I first need to estimate the sunset and sunrise times, 
# and then estimate the length of the night as the time difference between the two points. Then, proportion of
# the night will be equal to hps/nightlength. This really only makes sense for the nocturnal species, but I'll
# just do it for all species and we can only use it when we want to


# Calculate sunset and sunrise on the day of capture - I need to do this separately for each site since the lat and lon are different

# Suncalc is weird about having more than 1 latitude and since ours are duplicated multiple times for the same site, I'll just create a 
# new data frame with just lat and lon

# Create a data frame that contains the lat and lon of each study site (taken from the NEON website, coordinates specific to each trapping grid)
latlon<-data.frame(matrix(ncol = 3, nrow = 8))
colnames(latlon)<-c('site', 'lat', 'lon')
latlon$site<-c("HARV", "SERC", "SCBI", "BLAN", "STEI", "UNDE", "MLBS", "ORNL")
latlon$lat<-c(42.44339, 38.89957, 38.88645, 39.08557, 45.78460, 46.21119, 37.42820, 35.96543)
latlon$lon<-c(-72.22512, -76.55726, -78.14642, -77.95841, -90.04806, -89.49139, -80.56425, -84.22918)




# Eastern time is all sites except UNDE and STEI
blan<-subset(dat, siteID=="BLAN")
harv<-subset(dat, siteID=="HARV")
mlbs<-subset(dat, siteID=="MLBS")
ornl<-subset(dat, siteID=="ORNL")
scbi<-subset(dat, siteID=="SCBI")
serc<-subset(dat, siteID=="SERC")
stei<-subset(dat, siteID=="STEI")
unde<-subset(dat, siteID=="UNDE")








#### BLAN ####
head(blan$captime)
blan$captime<-ymd_hms(blan$captime, tz = "America/New_York") # specifies that these dates and times are in eastern daylight savings time

suntimes_blan<-getSunlightTimes(date = blan$capture_date, lat = latlon[4,2] , lon = latlon[4,3], tz = "America/New_York", keep = c("sunrise", "sunset"))

sunset<-as.data.frame(suntimes_blan$sunset)
colnames(sunset)<-"sunset"
sunrise<-as.data.frame(suntimes_blan$sunrise)
colnames(sunrise)<-"sunrise"

suntimes2_blan<-getSunlightTimes(date = (blan$capture_date - 1), lat = latlon[4,2] , lon = latlon[4,3] , tz = "America/New_York", keep = c("sunset")) # the day before's sunset
sunset2<-as.data.frame(suntimes2_blan$sunset)
colnames(sunset2)<-"sunset2"

### These sunset and sunrise times were cross-checked with NOAA estimates https://gml.noaa.gov/grad/solcalc/

# Add this to the blan dataset
blan<-cbind(blan, sunset, sunset2, sunrise)


blan$night_length<-as.numeric(difftime(blan$sunrise, blan$sunset2, units = "hours")) # this calculates time difference between sunrise on 
# change the class to numeric                                            # capture morning and sunset the previous night


blan$sunset_diff<-difftime(blan$captime, blan$sunset, units = "hours")
blan$sunset2_diff<-difftime(blan$captime, blan$sunset2, units = "hours")

#Any number that comes up negative here is because the second date is after the first date. This happens in cases when
#the animal was captured after midnight - so technically the sunset from the day before is the one that should be used

#So, use ifelse, if the difftime for "sunset_diff" is negative, instead return the difftime for "sunset_diff2" - which uses the night before's sunset
#Make sure that this only applies to ones that happened between midnight and sunset the next evening - not exactly sure how to deal with daytime captures yet 
blan$hps<-ifelse(blan$sunset_diff<0 & blan$capture_time<"12:00:00", blan$sunset2_diff, blan$sunset_diff)


summary(blan$night_length)

# And use this to estimate the proportion of night elapsed at the time of capture

blan$prop_elapsed<-(blan$hps/blan$night_length)






#### HARV ####
### All HARV sunset and sunrise are estimated an hour early Not sure why and I've tried several times to fix it, but I'll just add an hour to make it correct
head(harv$captime)
harv$captime<-ymd_hms(harv$captime, tz = "America/New_York")

suntimes_harv<-getSunlightTimes(date = harv$capture_date, lat = latlon[1,2] , lon = latlon[1,3], tz = "America/New_York", keep = c("sunrise", "sunset"))

sunset<-as.data.frame(suntimes_harv$sunset)
colnames(sunset)<-"sunset"
sunrise<-as.data.frame(suntimes_harv$sunrise)
colnames(sunrise)<-"sunrise"

suntimes2_harv<-getSunlightTimes(date = (harv$capture_date - 1), lat = latlon[1,2] , lon = latlon[1,3] , tz = "America/New_York", keep = c("sunset")) # the day before's sunset
sunset2<-as.data.frame(suntimes2_harv$sunset)
colnames(sunset2)<-"sunset2"

### These sunset and sunrise times were cross-checked with NOAA estimates https://gml.noaa.gov/grad/solcalc/ 

# Add this to the harv dataset
harv<-cbind(harv, sunset, sunset2, sunrise)


harv$night_length<-as.numeric(difftime(harv$sunrise, harv$sunset2, units = "hours")) # this calculates time difference between sunrise on 
# change the class to numeric                                            # capture morning and sunset the previous night


harv$sunset_diff<-difftime(harv$captime, harv$sunset, units = "hours")
harv$sunset2_diff<-difftime(harv$captime, harv$sunset2, units = "hours")

#Any number that comes up negative here is because the second date is after the first date. This happens in cases when
#the animal was captured after midnight - so technically the sunset from the day before is the one that should be used

#So, use ifelse, if the difftime for "sunset_diff" is negative, instead return the difftime for "sunset_diff2" - which uses the night before's sunset
#Make sure that this only applies to ones that happened between midnight and sunset the next evening - not exactly sure how to deal with daytime captures yet 
harv$hps<-ifelse(harv$sunset_diff<0 & harv$capture_time<"12:00:00", harv$sunset2_diff, harv$sunset_diff)


summary(harv$night_length)

# And use this to estimate the proportion of night elapsed at the time of capture

harv$prop_elapsed<-(harv$hps/harv$night_length)




#### MLBS ####
head(mlbs$captime)
mlbs$captime<-ymd_hms(mlbs$captime, tz = "America/New_York")

suntimes_mlbs<-getSunlightTimes(date = mlbs$capture_date, lat = latlon[7,2] , lon = latlon[7,3], tz = "America/New_York", keep = c("sunrise", "sunset"))

sunset<-as.data.frame(suntimes_mlbs$sunset)
colnames(sunset)<-"sunset"
sunrise<-as.data.frame(suntimes_mlbs$sunrise)
colnames(sunrise)<-"sunrise"

suntimes2_mlbs<-getSunlightTimes(date = (mlbs$capture_date - 1), lat = latlon[7,2] , lon = latlon[7,3] , tz = "America/New_York", keep = c("sunset")) # the day before's sunset
sunset2<-as.data.frame(suntimes2_mlbs$sunset)
colnames(sunset2)<-"sunset2"

### These sunset and sunrise times were cross-checked with NOAA estimates https://gml.noaa.gov/grad/solcalc/ 

# Add this to the mlbs dataset
mlbs<-cbind(mlbs, sunset, sunset2, sunrise)


mlbs$night_length<-as.numeric(difftime(mlbs$sunrise, mlbs$sunset2, units = "hours")) # this calculates time difference between sunrise on 
# change the class to numeric                                            # capture morning and sunset the previous night


mlbs$sunset_diff<-difftime(mlbs$captime, mlbs$sunset, units = "hours")
mlbs$sunset2_diff<-difftime(mlbs$captime, mlbs$sunset2, units = "hours")

#Any number that comes up negative here is because the second date is after the first date. This happens in cases when
#the animal was captured after midnight - so technically the sunset from the day before is the one that should be used

#So, use ifelse, if the difftime for "sunset_diff" is negative, instead return the difftime for "sunset_diff2" - which uses the night before's sunset
#Make sure that this only applies to ones that happened between midnight and sunset the next evening - not exactly sure how to deal with daytime captures yet 
mlbs$hps<-ifelse(mlbs$sunset_diff<0 & mlbs$capture_time<"12:00:00", mlbs$sunset2_diff, mlbs$sunset_diff)


summary(mlbs$night_length)

# And use this to estimate the proportion of night elapsed at the time of capture

mlbs$prop_elapsed<-(mlbs$hps/mlbs$night_length)





#### ORNL ####
head(ornl$captime)
ornl$captime<-ymd_hms(ornl$captime, tz = "America/New_York")

suntimes_ornl<-getSunlightTimes(date = ornl$capture_date, lat = latlon[8,2] , lon = latlon[8,3], tz = "America/New_York", keep = c("sunrise", "sunset"))

sunset<-as.data.frame(suntimes_ornl$sunset)
colnames(sunset)<-"sunset"
sunrise<-as.data.frame(suntimes_ornl$sunrise)
colnames(sunrise)<-"sunrise"

suntimes2_ornl<-getSunlightTimes(date = (ornl$capture_date - 1), lat = latlon[8,2] , lon = latlon[8,3] , tz = "America/New_York", keep = c("sunset")) # the day before's sunset
sunset2<-as.data.frame(suntimes2_ornl$sunset)
colnames(sunset2)<-"sunset2"

### These sunset and sunrise times were cross-checked with NOAA estimates https://gml.noaa.gov/grad/solcalc/
# Add this to the ornl dataset
ornl<-cbind(ornl, sunset, sunset2, sunrise)


ornl$night_length<-as.numeric(difftime(ornl$sunrise, ornl$sunset2, units = "hours")) # this calculates time difference between sunrise on 
# change the class to numeric                                            # capture morning and sunset the previous night


ornl$sunset_diff<-difftime(ornl$captime, ornl$sunset, units = "hours")
ornl$sunset2_diff<-difftime(ornl$captime, ornl$sunset2, units = "hours")

#Any number that comes up negative here is because the second date is after the first date. This happens in cases when
#the animal was captured after midnight - so technically the sunset from the day before is the one that should be used

#So, use ifelse, if the difftime for "sunset_diff" is negative, instead return the difftime for "sunset_diff2" - which uses the night before's sunset
#Make sure that this only applies to ones that happened between midnight and sunset the next evening - not exactly sure how to deal with daytime captures yet 
ornl$hps<-ifelse(ornl$sunset_diff<0 & ornl$capture_time<"12:00:00", ornl$sunset2_diff, ornl$sunset_diff)


summary(ornl$night_length)

# And use this to estimate the proportion of night elapsed at the time of capture

ornl$prop_elapsed<-(ornl$hps/ornl$night_length)



#### SCBI ####
head(scbi$captime)
scbi$captime<-ymd_hms(scbi$captime, tz = "America/New_York")

suntimes_scbi<-getSunlightTimes(date = scbi$capture_date, lat = latlon[3,2] , lon = latlon[3,3], tz = "America/New_York", keep = c("sunrise", "sunset"))

sunset<-as.data.frame(suntimes_scbi$sunset)
colnames(sunset)<-"sunset"
sunrise<-as.data.frame(suntimes_scbi$sunrise)
colnames(sunrise)<-"sunrise"

suntimes2_scbi<-getSunlightTimes(date = (scbi$capture_date - 1), lat = latlon[3,2] , lon = latlon[3,3] , tz = "America/New_York", keep = c("sunset")) # the day before's sunset
sunset2<-as.data.frame(suntimes2_scbi$sunset)
colnames(sunset2)<-"sunset2"

### These sunset and sunrise times were cross-checked with NOAA estimates https://gml.noaa.gov/grad/solcalc/

# Add this to the scbi dataset
scbi<-cbind(scbi, sunset, sunset2, sunrise)


scbi$night_length<-as.numeric(difftime(scbi$sunrise, scbi$sunset2, units = "hours")) # this calculates time difference between sunrise on 
# change the class to numeric                                            # capture morning and sunset the previous night


scbi$sunset_diff<-difftime(scbi$captime, scbi$sunset, units = "hours")
scbi$sunset2_diff<-difftime(scbi$captime, scbi$sunset2, units = "hours")

#Any number that comes up negative here is because the second date is after the first date. This happens in cases when
#the animal was captured after midnight - so technically the sunset from the day before is the one that should be used

#So, use ifelse, if the difftime for "sunset_diff" is negative, instead return the difftime for "sunset_diff2" - which uses the night before's sunset
#Make sure that this only applies to ones that happened between midnight and sunset the next evening - not exactly sure how to deal with daytime captures yet 
scbi$hps<-ifelse(scbi$sunset_diff<0 & scbi$capture_time<"12:00:00", scbi$sunset2_diff, scbi$sunset_diff)


summary(scbi$night_length)

# And use this to estimate the proportion of night elapsed at the time of capture

scbi$prop_elapsed<-(scbi$hps/scbi$night_length)




#### SERC ####
head(serc$captime)
serc$captime<-ymd_hms(serc$captime, tz = "America/New_York")

suntimes_serc<-getSunlightTimes(date = serc$capture_date, lat = latlon[2,2] , lon = latlon[2,3], tz = "America/New_York", keep = c("sunrise", "sunset"))

sunset<-as.data.frame(suntimes_serc$sunset)
colnames(sunset)<-"sunset"
sunrise<-as.data.frame(suntimes_serc$sunrise)
colnames(sunrise)<-"sunrise"

suntimes2_serc<-getSunlightTimes(date = (serc$capture_date - 1), lat = latlon[2,2] , lon = latlon[2,3] , tz = "America/New_York", keep = c("sunset")) # the day before's sunset
sunset2<-as.data.frame(suntimes2_serc$sunset)
colnames(sunset2)<-"sunset2"

### These sunset and sunrise times were cross-checked with NOAA estimates https://gml.noaa.gov/grad/solcalc/ 

# Add this to the serc dataset
serc<-cbind(serc, sunset, sunset2, sunrise)


serc$night_length<-as.numeric(difftime(serc$sunrise, serc$sunset2, units = "hours")) # this calculates time difference between sunrise on 
# change the class to numeric                                            # capture morning and sunset the previous night


serc$sunset_diff<-difftime(serc$captime, serc$sunset, units = "hours")
serc$sunset2_diff<-difftime(serc$captime, serc$sunset2, units = "hours")

#Any number that comes up negative here is because the second date is after the first date. This happens in cases when
#the animal was captured after midnight - so technically the sunset from the day before is the one that should be used

#So, use ifelse, if the difftime for "sunset_diff" is negative, instead return the difftime for "sunset_diff2" - which uses the night before's sunset
#Make sure that this only applies to ones that happened between midnight and sunset the next evening - not exactly sure how to deal with daytime captures yet 
serc$hps<-ifelse(serc$sunset_diff<0 & serc$capture_time<"12:00:00", serc$sunset2_diff, serc$sunset_diff)


summary(serc$night_length)

# And use this to estimate the proportion of night elapsed at the time of capture

serc$prop_elapsed<-(serc$hps/serc$night_length)








#### STEI ####
head(stei$captime)
stei$captime<-ymd_hms(stei$captime, tz = "US/CENTRAL")

suntimes_stei<-getSunlightTimes(date = stei$capture_date, lat = latlon[5,2] , lon = latlon[5,3], tz = "US/CENTRAL", keep = c("sunrise", "sunset"))

sunset<-as.data.frame(suntimes_stei$sunset)
colnames(sunset)<-"sunset"
sunrise<-as.data.frame(suntimes_stei$sunrise)
colnames(sunrise)<-"sunrise"

suntimes2_stei<-getSunlightTimes(date = (stei$capture_date - 1), lat = latlon[5,2] , lon = latlon[5,3] , tz = "US/CENTRAL", keep = c("sunset")) # the day before's sunset
sunset2<-as.data.frame(suntimes2_stei$sunset)
colnames(sunset2)<-"sunset2"

### These sunset and sunrise times were cross-checked with NOAA estimates https://gml.noaa.gov/grad/solcalc/


# Add this to the stei dataset
stei<-cbind(stei, sunset, sunset2, sunrise)

stei$night_length<-as.numeric(difftime(stei$sunrise, stei$sunset2, units = "hours")) # this calculates time difference between sunrise on 
# change the class to numeric                                            # capture morning and sunset the previous night


stei$sunset_diff<-difftime(stei$captime, stei$sunset, units = "hours")
stei$sunset2_diff<-difftime(stei$captime, stei$sunset2, units = "hours")

#Any number that comes up negative here is because the second date is after the first date. This happens in cases when
#the animal was captured after midnight - so technically the sunset from the day before is the one that should be used

#So, use ifelse, if the difftime for "sunset_diff" is negative, instead return the difftime for "sunset_diff2" - which uses the night before's sunset
#Make sure that this only applies to ones that happened between midnight and sunset the next evening - not exactly sure how to deal with daytime captures yet 
stei$hps<-ifelse(stei$sunset_diff<0 & stei$capture_time<"12:00:00", stei$sunset2_diff, stei$sunset_diff)


summary(stei$night_length)

# And use this to estimate the proportion of night elapsed at the time of capture

stei$prop_elapsed<-(stei$hps/stei$night_length)




#### UNDE ####
head(unde$captime)
unde$captime<-ymd_hms(unde$captime, tz = "US/CENTRAL")

suntimes_unde<-getSunlightTimes(date = unde$capture_date, lat = latlon[6,2] , lon = latlon[6,3], tz = "US/CENTRAL", keep = c("sunrise", "sunset"))

sunset<-as.data.frame(suntimes_unde$sunset)
colnames(sunset)<-"sunset"
sunrise<-as.data.frame(suntimes_unde$sunrise)
colnames(sunrise)<-"sunrise"

suntimes2_unde<-getSunlightTimes(date = (unde$capture_date - 1), lat = latlon[6,2] , lon = latlon[6,3] , tz = "US/CENTRAL", keep = c("sunset")) # the day before's sunset
sunset2<-as.data.frame(suntimes2_unde$sunset)
colnames(sunset2)<-"sunset2"

### These sunset and sunrise times were cross-checked with NOAA estimates https://gml.noaa.gov/grad/solcalc/


# Add this to the unde dataset
unde<-cbind(unde, sunset, sunset2, sunrise)

unde$night_length<-as.numeric(difftime(unde$sunrise, unde$sunset2, units = "hours")) # this calculates time difference between sunrise on 
# change the class to numeric                                            # capture morning and sunset the previous night


unde$sunset_diff<-difftime(unde$captime, unde$sunset, units = "hours")
unde$sunset2_diff<-difftime(unde$captime, unde$sunset2, units = "hours")

#Any number that comes up negative here is because the second date is after the first date. This happens in cases when
#the animal was captured after midnight - so technically the sunset from the day before is the one that should be used

#So, use ifelse, if the difftime for "sunset_diff" is negative, instead return the difftime for "sunset_diff2" - which uses the night before's sunset
#Make sure that this only applies to ones that happened between midnight and sunset the next evening - not exactly sure how to deal with daytime captures yet 
unde$hps<-ifelse(unde$sunset_diff<0 & unde$capture_time<"12:00:00", unde$sunset2_diff, unde$sunset_diff)


summary(unde$night_length)

# And use this to estimate the proportion of night elapsed at the time of capture

unde$prop_elapsed<-(unde$hps/unde$night_length)







### Rbind these all back together ####

## before I do this, I need to remove the captime, sunset, sunrise, etc. columns otherwise it will convert the times from 
## stei and unde into EDT just like the rest of the sites... R doesn't support multiple timezones in one column
data.frame(colnames(blan))

### Check that the correct columns are being removed
blan<-blan[,-c(48,54:56, 58:59)]
harv<-harv[,-c(48,54:56, 58:59)]
mlbs<-mlbs[,-c(48,54:56, 58:59)]
ornl<-ornl[,-c(48,54:56, 58:59)]
scbi<-scbi[,-c(48,54:56, 58:59)]
serc<-serc[,-c(48,54:56, 58:59)]
stei<-stei[,-c(48,54:56, 58:59)]
unde<-unde[,-c(48,54:56, 58:59)]

dat<-rbind(blan, harv, mlbs, ornl, scbi, serc, stei, unde)




#write.csv(dat, file = "C:/Users/allib/Documents/NEON/capture_timing_23.csv") # Change filename 


