
# Read in all capturetime files and compile them into a single data frame

setwd("/Users/allib/Documents/NEON/Analyses/CaptureTimes") # Set the location where all of the capture time files are stored


require(readr)
require(lubridate)
require(dplyr)

list_csv_files <- list.files(path = "/Users/allib/Documents/NEON/Analyses/CaptureTimes", pattern = "\\.csv$") # Change to the working directory above
df <- readr::read_csv(list_csv_files, id = "file_name")
df

df<-as.data.frame(df)

df$collectDate<-parse_date_time(df$collectDate, orders = c('mdy', 'ymd'))

summary(as.factor(df$taxonID))
# BLBR     GLVO     MIPI     MYGA     NAIN     PELE PELEPEMA     PEMA     PESP     SOCI     TAST     ZAHU 
# 38       26        2       18       15      906        3      214        4        3       40        5 

df<-dplyr::select(df, uid, siteID, plotID, trapCoordinate.x, collectDate, tagID.x, individualCode, taxonID, capturetime_line, captime,
                  capture_time, sunset, sunset2, sunset_diff, sunset2_diff, hps)


# Load full box trapping data

caps<-read.csv(file="C:/Users/allib/Documents/NEON/Data for PI/mammalcaps_2023.csv") # Change to the 2024 sampling data file we exported from the ibuttondata_Orrock_remdupes.R code

caps$collectDate<-parse_date_time(caps$collectDate, orders = c('mdy', 'ymd'))
caps$publicationDate<-ymd_hms(caps$publicationDate)
caps$endDate.y<-ymd_hms(caps$endDate.y)

join_data<-left_join(caps, df)


summary(df$hps)
summary(join_data$hps)

# Change filename and write .csv file
write.csv(join_data, file = "C:/Users/allib/Documents/NEON/Analyses/NEON_capdata2023_captimes.csv") # this will now be the full 2024 trapping data with capture times estimated and added


