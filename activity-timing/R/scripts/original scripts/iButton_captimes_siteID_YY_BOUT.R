### Unless otherwise stated this code was written by Allison M Brehm and is most recently updated 8/15/22.

#### Overview ####

### This code is intended to perform the following functions:
### 1) Open the NEON iButton data from a single trapping bout and merge it with the full serial number and logger pair information
### 2) Flag captures based on a simple decision rule (code written by Cristian Dambros 7/22)
### 3) Using this decision rule, extract each observation at which this rule becomes true, and extract the date and time of these observations
### ***This extracted date and time is the estimated capture time***
### 4) Pull in the trapping data from this trapping bout and extract known captures from the logger data
### 5) Plot all temperature data from loggers in capture traps and mark the capture time - store in a folder together




### Set working directory as the NEON folder in documents

# setwd(choose.dir(default = "", caption = "Select folder"))





#### PART 1 ####
### 1) Open the NEON iButton data from a single trapping bout and merge it with the full serial number and logger pair information


require(dplyr)
require(stringr)
library(tibble)
require(readxl)



##### filename changes ####
fob_sn <- read.csv(
  file = "data/2024 - UW_Madison_Fob_Serial_Numbers.csv",
  col.names = c(
    "pair", "IO", "name", "new_pair", "SerialNumber", "pairStatus",
    "OK", "Notes"
  ),
  header = TRUE
) # this file should be the most up-to-date fob serial number file. Download it from the drive and place it into the NEON folder.
data <- read.csv(
  file = "data/raw-iButton-data/BLAN_04_01_000s_dowloaded_04_19.csv",
  check.names = FALSE
) # this file will be the raw ibutton data file of interest, stored in the "Data_raw2024" folder.


# Check headings and remove the unnecessary columns
head(fob_sn)
# names(fob_sn)[1] <- "pair"
# names(fob_sn)[2] <- "I_O"
fob_sn <- rename(fob_sn,
  I_O = "IO", FOB = "name",
  Serial.Number = "SerialNumber"
)
# fob_sn <- fob_sn[, c("pair", "I_O", "FOB", "Serial.Number")]


# When reading in the Session iButton data, first I'll look for buttons that were downloaded twice
# So I need to tell R to check for columns that are exactly duplicates, with the column heading just having a ".1" at the end different

# first i want it to show me the dups so i can check them
dups <- data[duplicated(as.list(data))]

# it's returning 0 duplicated columns

summary(dups) # If there are columns identified as duplicates, confirm that they are indeed duplicated

# and then remove them
nodups <- data[!duplicated(as.list(data))]
summary(nodups)

# There are typically also a few instances with "*" at the end of the serial number in the download file. This happens when an ibutton
# has times that don't line up exactly with the first ibutton that was read ... For now I'm going to remove this character
# so that it will match up with the "sn" in the "fob_sn" data frame

colnames(nodups)
colnames(nodups) <- gsub("\\*", "", colnames(nodups))
colnames(nodups)

nodups_wide <- setNames(data.frame(t(nodups[, -1])), nodups[, 1])

nodups_wide <- tibble::rownames_to_column(nodups_wide, "sn")

str(nodups_wide)
nodups_wide$sn <- as.factor(paste(nodups_wide$sn))
summary(nodups_wide$sn)


fobs <- fob_sn |> rename(sn = "Serial.Number")

# rename the serial number column to match the temperature data sheet
fobs$sn <- as.factor(paste(fobs$sn))
summary(fobs$sn)


# merge the pair data in with the temperature data by the common column "sn"
merged <- merge(nodups_wide, fobs, by = "sn", all.x = TRUE)
# put these three new columns at the front instead
nondatecols <- names(merged)[!grepl("\\d{4}-\\d{2}-\\d{2}", names(merged))]
merged <- merged |> 
  dplyr::relocate(all_of(nondatecols), .before = 1) |> 
  filter(!is.na(pair)) |> 
  arrange(pair, I_O)

# Re-order columns so the data is easier to work with
# 
# all_cols <- seq_len(ncol(merged))
# # Get indices of fixed columns
# 
# # "pair", "I_O", "FOB"
# 
# fixed <- c(1, grep("pair|I_O|FOB", names(merged))) # these are the columns with the letter "a" in the column name (only excludes the dates)
# fixed_columns <- merged[, fixed]
# datecolumns <- merged[, -fixed]
# 
# merged <- cbind(fixed_columns, datecolumns)


# Now order the data frame by "pair" number and then "in_out", this will always put the "In" before the "out" (alphabetical by default)

# merged <- merged[with(merged, order(pair, I_O)), ]



### Check "merged" for iButtons that don't have a pair name (scroll to the bottom)




# Remove the buttons that don't have pairs because of download issues, etc.

# merged <- subset(merged, pair != 753)


### PARTS 2&3 ####
### 2) Flag captures based on a simple decision rule (code written by Cristian Dambros 7/22)
### 3) Using this decision rule, extract the first observation at which this rule becomes true, and extract the date and time of this observation



## Script developed by Cristian Dambros (loop for flagging captures based on a decision rule)
### and Allison M Brehm (adaptations for adding maximum threshold value and extracting capture timing)

# No packages required

## Import data
##
dataRaw <- merged

## Check dimensions and first rows of data
dim(dataRaw)
dataRaw[1:5, 1:10]

## Split data by pair
dataSplit <- split(dataRaw, dataRaw$pair)


## Identify columns starting with date/time (starting with X)
# pattern <- "[0-9]{1,2}\\/[0-9]{1,2}\\/.*"
pattern <- "(\\d{4}-\\d{2}-\\d{2})\\s(.*)" # update the date pattern
names <- colnames(dataRaw)

namesDates <- grepl(pattern, names)


# Define a threshold for the max temperature difference between inside and outside loggers
threshmax <- 3


maxmet <- {}

for (i in 1:length(dataSplit)) {
  cat("Processing pair", i, "\n")
  # Get target pair
  dataTarget <- dataSplit[[i]]

  time_diffs = dataTarget[1, namesDates] - dataTarget[2, namesDates]
  
  # Calculate max difference
  maxDiffs <- max(dataTarget[1, namesDates] - dataTarget[2, namesDates])


  maxmet[i] <- maxDiffs >= threshmax
}






# Define a threshold for the temperature difference between traps
thresh <- 1

# Define the consecutive number of observations for rule to be true
nobs <- 10

# I will use a for loop here to make things clearer (lapply would be my first choice, there are ways to simplify further)
flag <- {}
firstdatetime <- {}
seconddatetime <- {}
thirddatetime <- {}
fourthdatetime <- {}
fifthdatetime <- {}
sixthdatetime <- {}
seventhdatetime <- {}
eighthdatetime <- {}
ninthdatetime <- {}
tenthdatetime <- {}
matchposition1 <- {}
matchposition2 <- {}
matchposition3 <- {}
matchposition4 <- {}
matchposition5 <- {}
matchposition6 <- {}
matchposition7 <- {}
matchposition8 <- {}
matchposition9 <- {}
matchposition10 <- {}

for (i in 1:length(dataSplit))
{
  cat("Processing pair", i, "\n")
  # Get target pair
  dataTarget <- dataSplit[[i]]

  # Calculate differences
  dataDiffs <- abs(dataTarget[1, namesDates] - dataTarget[2, namesDates])

  # Get TRUE for events with temperature difference equal to or above thresh
  aboveThresh <- dataDiffs >= thresh

  # Find lengths of repeated values
  dataRle <- rle(as.numeric(aboveThresh))

  # Check repeated values with at least n observations and TRUE (differences above thresh)
  dataMatch <- dataRle$lengths >= nobs & dataRle$values


  ##


  # I want R to give me the position where dataMatch becomes true the first time
  firstdatamatch <- which(dataRle$lengths >= nobs & dataRle$values)[1]
  lengthposition <- ifelse(!is.na(firstdatamatch), sum(dataRle$lengths[1:(firstdatamatch - 1)]), NA)

  matchposition1[i] <- lengthposition

  # And find the date and time when this occurs (return NA if there is no time when this happens) ##### we need to add 5 because that's the column where date/time observations begin
  firstdatetime[i] <- ifelse(!is.na(firstdatamatch), colnames(dataTarget[matchposition1[i] + 5]), NA)



  # I want R to give me the position where dataMatch becomes true the second time
  seconddatamatch <- which(dataRle$lengths >= nobs & dataRle$values)[2]
  lengthposition <- ifelse(!is.na(seconddatamatch), sum(dataRle$lengths[1:(seconddatamatch - 1)]), NA)

  matchposition2[i] <- lengthposition

  # And find the date and time when this occurs (return NA if there is no time when this happens)
  seconddatetime[i] <- ifelse(!is.na(seconddatamatch), colnames(dataTarget[matchposition2[i] + 5]), NA)



  # I want R to give me the position where dataMatch becomes true the third time
  thirddatamatch <- which(dataRle$lengths >= nobs & dataRle$values)[3]
  lengthposition <- ifelse(!is.na(thirddatamatch), sum(dataRle$lengths[1:(thirddatamatch - 1)]), NA)

  matchposition3[i] <- lengthposition

  # And find the date and time when this occurs (return NA if there is no time when this happens)
  thirddatetime[i] <- ifelse(!is.na(thirddatamatch), colnames(dataTarget[matchposition3[i] + 5]), NA)


  # I want R to give me the position where dataMatch becomes true the fourth time
  fourthdatamatch <- which(dataRle$lengths >= nobs & dataRle$values)[4]
  lengthposition <- ifelse(!is.na(fourthdatamatch), sum(dataRle$lengths[1:(fourthdatamatch - 1)]), NA)

  matchposition4[i] <- lengthposition

  # And find the date and time when this occurs (return NA if there is no time when this happens)
  fourthdatetime[i] <- ifelse(!is.na(fourthdatamatch), colnames(dataTarget[matchposition4[i] + 5]), NA)


  # I want R to give me the position where dataMatch becomes true the fifth time
  fifthdatamatch <- which(dataRle$lengths >= nobs & dataRle$values)[5]
  lengthposition <- ifelse(!is.na(fifthdatamatch), sum(dataRle$lengths[1:(fifthdatamatch - 1)]), NA)

  matchposition5[i] <- lengthposition

  # And find the date and time when this occurs (return NA if there is no time when this happens)
  fifthdatetime[i] <- ifelse(!is.na(fifthdatamatch), colnames(dataTarget[matchposition5[i] + 5]), NA)




  # I want R to give me the position where dataMatch becomes true the sixth time
  sixthdatamatch <- which(dataRle$lengths >= nobs & dataRle$values)[6]
  lengthposition <- ifelse(!is.na(sixthdatamatch), sum(dataRle$lengths[1:(sixthdatamatch - 1)]), NA)

  matchposition6[i] <- lengthposition

  # And find the date and time when this occurs (return NA if there is no time when this happens)
  sixthdatetime[i] <- ifelse(!is.na(sixthdatamatch), colnames(dataTarget[matchposition6[i] + 5]), NA)




  # I want R to give me the position where dataMatch becomes true the seventh time
  seventhdatamatch <- which(dataRle$lengths >= nobs & dataRle$values)[7]
  lengthposition <- ifelse(!is.na(seventhdatamatch), sum(dataRle$lengths[1:(seventhdatamatch - 1)]), NA)

  matchposition7[i] <- lengthposition

  # And find the date and time when this occurs (return NA if there is no time when this happens)
  seventhdatetime[i] <- ifelse(!is.na(seventhdatamatch), colnames(dataTarget[matchposition7[i] + 5]), NA)



  # I want R to give me the position where dataMatch becomes true the eighth time
  eighthdatamatch <- which(dataRle$lengths >= nobs & dataRle$values)[8]
  lengthposition <- ifelse(!is.na(eighthdatamatch), sum(dataRle$lengths[1:(eighthdatamatch - 1)]), NA)

  matchposition8[i] <- lengthposition

  # And find the date and time when this occurs (return NA if there is no time when this happens)
  eighthdatetime[i] <- ifelse(!is.na(eighthdatamatch), colnames(dataTarget[matchposition8[i] + 5]), NA)



  # I want R to give me the position where dataMatch becomes true the ninth time
  ninthdatamatch <- which(dataRle$lengths >= nobs & dataRle$values)[9]
  lengthposition <- ifelse(!is.na(ninthdatamatch), sum(dataRle$lengths[1:(ninthdatamatch - 1)]), NA)

  matchposition9[i] <- lengthposition

  # And find the date and time when this occurs (return NA if there is no time when this happens)
  ninthdatetime[i] <- ifelse(!is.na(ninthdatamatch), colnames(dataTarget[matchposition9[i] + 5]), NA)



  # I want R to give me the position where dataMatch becomes true the tenth time
  tenthdatamatch <- which(dataRle$lengths >= nobs & dataRle$values)[10]
  lengthposition <- ifelse(!is.na(tenthdatamatch), sum(dataRle$lengths[1:(tenthdatamatch - 1)]), NA)

  matchposition10[i] <- lengthposition

  # And find the date and time when this occurs (return NA if there is no time when this happens)
  tenthdatetime[i] <- ifelse(!is.na(tenthdatamatch), colnames(dataTarget[matchposition10[i] + 5]), NA)










  # Check number of times the rule is true (at least 3 consecutive observations above thresh)
  flag[i] <- sum(dataMatch)
}

# maxmet returns TRUE if the max temp difference threshold was met for each pair
maxmet

# flag has the number of observations matching the rule for each pair
flag

# flag>0 returns TRUE if rule was true at least once
flag > 0

# should give the time when "dataMatch" is first true for each pair
firstdatetime

# should give the second time when "dataMatch" is true for each pair
seconddatetime

# should give the third time when "dataMatch" is true for each pair
thirddatetime

# should give the fourth time when "dataMatch" is true for each pair
fourthdatetime

# should give the fifth time when "dataMatch" is true for each pair
fifthdatetime
sixthdatetime
seventhdatetime
### and so on...

# and the position of this above time
matchposition1
matchposition2
matchposition3
matchposition4
matchposition5
matchposition6
matchposition7
### etc...


# Return camera/flag in a data frame along with whether or not the max temp diff was met AND a final "flagged" value if both are met
final <- data.frame(
  pair = levels(factor(dataRaw$pair)),
  nflags = flag,
  firstdatetime = firstdatetime,
  seconddatetime = seconddatetime,
  thirddatetime = thirddatetime,
  fourthdatetime = fourthdatetime,
  fifthdatetime = fifthdatetime,
  sixthdatetime = sixthdatetime,
  seventhdatetime = seventhdatetime,
  eighthdatetime = eighthdatetime,
  ninthdatetime = ninthdatetime,
  tenthdatetime = tenthdatetime,
  matchposition1 = matchposition1,
  matchposition2 = matchposition2,
  matchposition3 = matchposition3,
  matchposition4 = matchposition4,
  matchposition5 = matchposition5,
  matchposition6 = matchposition6,
  matchposition7 = matchposition7,
  matchposition8 = matchposition8,
  matchposition9 = matchposition9,
  matchposition10 = matchposition10,
  flag = flag > 0,
  maxdiff = maxmet,
  flagged = flag > 0 & maxmet == TRUE
)


# put into original data frame
dataRaw$flagged <- final$flag[match(dataRaw$pair, final$pair)]
dataRaw$maxdiff <- final$maxdiff[match(dataRaw$pair, final$pair)]
dataRaw$firstdatetime <- final$firstdatetime[match(dataRaw$pair, final$pair)]
dataRaw$seconddatetime <- final$seconddatetime[match(dataRaw$pair, final$pair)]
dataRaw$thirddatetime <- final$thirddatetime[match(dataRaw$pair, final$pair)]
dataRaw$fourthdatetime <- final$fourthdatetime[match(dataRaw$pair, final$pair)]
dataRaw$fifthdatetime <- final$fifthdatetime[match(dataRaw$pair, final$pair)]
dataRaw$sixthdatetime <- final$sixthdatetime[match(dataRaw$pair, final$pair)]
dataRaw$seventhdatetime <- final$seventhdatetime[match(dataRaw$pair, final$pair)]
dataRaw$eighthdatetime <- final$eighthdatetime[match(dataRaw$pair, final$pair)]
dataRaw$ninthdatetime <- final$ninthdatetime[match(dataRaw$pair, final$pair)]
dataRaw$tenthdatetime <- final$tenthdatetime[match(dataRaw$pair, final$pair)]
dataRaw$matchposition1 <- final$matchposition1[match(dataRaw$pair, final$pair)]
dataRaw$matchposition2 <- final$matchposition2[match(dataRaw$pair, final$pair)]
dataRaw$matchposition3 <- final$matchposition3[match(dataRaw$pair, final$pair)]
dataRaw$matchposition4 <- final$matchposition4[match(dataRaw$pair, final$pair)]
dataRaw$matchposition5 <- final$matchposition5[match(dataRaw$pair, final$pair)]
dataRaw$matchposition6 <- final$matchposition6[match(dataRaw$pair, final$pair)]
dataRaw$matchposition7 <- final$matchposition7[match(dataRaw$pair, final$pair)]
dataRaw$matchposition8 <- final$matchposition8[match(dataRaw$pair, final$pair)]
dataRaw$matchposition9 <- final$matchposition9[match(dataRaw$pair, final$pair)]
dataRaw$matchposition10 <- final$matchposition10[match(dataRaw$pair, final$pair)]
dataRaw$nflags <- final$nflags[match(dataRaw$pair, final$pair)]



# Re-order columns so the data is easier to work with

all_cols <- seq_len(ncol(dataRaw))
# Get indices of fixed columns
fixed <- c(1:4, grep("flagged|maxdiff|datetime|matchposition|nflags", names(dataRaw))) # these are the columns at the end that we want to put up front
fixed_columns <- dataRaw[, fixed]
datecolumns <- dataRaw[, -fixed]

sorted <- cbind(fixed_columns, datecolumns)


# look at the data now
sorted[1:5, 1:30]



#### PART 4 ####
### 4) Pull in the trapping data from this trapping bout and extract known captures from the logger data



## First split this data again by pair
dataSplit <- split(sorted, sorted$pair)


## Again, identify columns starting with date/time (starting with X)
# pattern <- "[0-9]{1,2}\\/[0-9]{1,2}\\/.*"
names <- colnames(sorted)

namesDates <- grepl(pattern, names)



# I will use a for loop here to calculate the difference between the in and out loggers of each pair, and save all of the results in a dataframe
# This will result in a dataframe with as many rows as there are pairs of loggers in this dataset, and as many columns as there are time points (observations)
diffs <- data.frame()

for (i in 1:length(dataSplit)) {
  cat("Processing pair", i, "\n")
  # Get target pair
  dataTarget <- dataSplit[[i]]

  # Calculate differences
  dataDiffs <- dataTarget[1, namesDates] - dataTarget[2, namesDates]
  diffs <- rbind(diffs, dataDiffs)
}


# diffs how has the difference between the in and out loggers at each time period for each pair


# Return diffs in a data frame along with the pair name, whether it's flagged, and the date and position of the flagging events

pairdiffs <- cbind(pair = levels(factor(dataRaw$pair)), diffs)
pairdiffs <- cbind(pairdiffs, final$flagged)
pairdiffs <- cbind(pairdiffs, final$firstdatetime)
pairdiffs <- cbind(pairdiffs, final$seconddatetime)
pairdiffs <- cbind(pairdiffs, final$thirddatetime)
pairdiffs <- cbind(pairdiffs, final$fourthdatetime)
pairdiffs <- cbind(pairdiffs, final$fifthdatetime)
pairdiffs <- cbind(pairdiffs, final$sixthdatetime)
pairdiffs <- cbind(pairdiffs, final$seventhdatetime)
pairdiffs <- cbind(pairdiffs, final$eighthdatetime)
pairdiffs <- cbind(pairdiffs, final$ninthdatetime)
pairdiffs <- cbind(pairdiffs, final$tenthdatetime)
pairdiffs <- cbind(pairdiffs, final$matchposition1)
pairdiffs <- cbind(pairdiffs, final$matchposition2)
pairdiffs <- cbind(pairdiffs, final$matchposition3)
pairdiffs <- cbind(pairdiffs, final$matchposition4)
pairdiffs <- cbind(pairdiffs, final$matchposition5)
pairdiffs <- cbind(pairdiffs, final$matchposition6)
pairdiffs <- cbind(pairdiffs, final$matchposition7)
pairdiffs <- cbind(pairdiffs, final$matchposition8)
pairdiffs <- cbind(pairdiffs, final$matchposition9)
pairdiffs <- cbind(pairdiffs, final$matchposition10)

# And again put these columns first so we can more easily see and work with the data

# Get indices of fixed columns
fixed <- c(grep("a", names(pairdiffs))) # these are the columns with the letter "a" in the column name (only excludes the dates)
fixed_columns <- pairdiffs[, fixed]
datecolumns <- pairdiffs[, -fixed]

sorted <- cbind(fixed_columns, datecolumns)
pairdiffs <- sorted

names(pairdiffs)[2] <- paste("flagged")
names(pairdiffs)[3] <- paste("firstdatetime")
names(pairdiffs)[4] <- paste("seconddatetime")
names(pairdiffs)[5] <- paste("thirddatetime")
names(pairdiffs)[6] <- paste("fourthdatetime")
names(pairdiffs)[7] <- paste("fifthdatetime")
names(pairdiffs)[8] <- paste("sixthdatetime")
names(pairdiffs)[9] <- paste("seventhdatetime")
names(pairdiffs)[10] <- paste("eighthdatetime")
names(pairdiffs)[11] <- paste("ninthdatetime")
names(pairdiffs)[12] <- paste("tenthdatetime")
names(pairdiffs)[13] <- paste("matchposition1")
names(pairdiffs)[14] <- paste("matchposition2")
names(pairdiffs)[15] <- paste("matchposition3")
names(pairdiffs)[16] <- paste("matchposition4")
names(pairdiffs)[17] <- paste("matchposition5")
names(pairdiffs)[18] <- paste("matchposition6")
names(pairdiffs)[19] <- paste("matchposition7")
names(pairdiffs)[20] <- paste("matchposition8")
names(pairdiffs)[21] <- paste("matchposition9")
names(pairdiffs)[22] <- paste("matchposition10")


# Now, pull in the capture data from this trapping session and use this to extract known captures from the logger data

require(readxl)
##### filename change ####
# "data/raw-iButton-data/BLAN_04_01_000s_dowloaded_04_19.csv"
trapping <- read_xlsx("data/out_files/BLAN_23_1.xlsx")
head(trapping)


pairs <- as.data.frame(trapping$iButtonLabel)
names(pairs) <- paste("pair")

str(pairdiffs$pair)
str(pairs$pair)

pairs$pair <- as.character(pairs$pair)

require(dplyr)

captures <- inner_join(pairdiffs, pairs) # Apply inner_join function
head(captures) # Print common data


# transpose the data frame so that it can be more easily plotted

require(tibble)

caps_t <- setNames(data.frame(t(captures[, -1])), captures[, 1])

colnames(caps_t) <- make.unique(colnames(caps_t), sep = ".")

str(caps_t)

caps_t <- caps_t %>%
  rownames_to_column(var = "date")


require(lubridate)

caps_t$date <- mdy_hm(caps_t$date) # it will say 21 failed to parse but thats only bc old column names are now the first 21 rows in this date column
str(caps_t)

caps <- caps_t


# I want to write a loop that will go through and plot each capture for me automatically


#### PART 5 ####
# Plot the temperature difference over time. Tell R to add a vertical break at every flagged potential capture time. Then export each plot into
# the appropriate folder



# Define the rows/observations that occur at each midnight...

# Get indices of fixed columns
midnight <- c(grep("00:00:00", caps$date)) # these are the row numbers that contain the time "00:00:00"


par(mfrow = c(1, 1))

##### foldername change ####
for (i in 2:ncol(caps)) {
  mypath <- file.path(
    "C:/Users/allib/Documents/NEON/Analyses/CapturePlots_2023/MLBS3",
    paste("myplot_", colnames(caps[i]), ".tiff", sep = "")
  )
  tiff(file = mypath, width = 700, height = 450)
  mytitle <- paste(names[i])

  par(mar = c(7, 4, 1, 1))
  plot(caps[22:nrow(caps), i], ylim = c(-5, 10), type = "l", xaxt = "n", ylab = colnames(caps[i]), xlab = "")
  abline(v = caps[12, i], col = adjustcolor("cornflowerblue", alpha = 0.75), lwd = 2) # We want to put a vertical break where the first dataMatch happened, which is at caps[12,i]
  abline(v = caps[13, i], col = adjustcolor("coral", alpha = 0.75), lwd = 2) # We want to put a vertical break where the second dataMatch happened, which is at caps[13,i]
  abline(v = caps[14, i], col = adjustcolor("cornflowerblue", alpha = 0.75), lwd = 2) # We want to put a vertical break where the third dataMatch happened, which is at caps[14,i]
  abline(v = caps[15, i], col = adjustcolor("coral", alpha = 0.75), lwd = 2) # We want to put a vertical break where the fourth dataMatch happened, which is at caps[15,i]
  abline(v = caps[16, i], col = adjustcolor("cornflowerblue", alpha = 0.75), lwd = 2) # We want to put a vertical break where the fifth dataMatch happened, which is at caps[16,i]
  abline(v = caps[17, i], col = adjustcolor("coral", alpha = 0.75), lwd = 2) # We want to put a vertical break where the sixth dataMatch happened
  abline(v = caps[18, i], col = adjustcolor("cornflowerblue", alpha = 0.75), lwd = 2) # We want to put a vertical break where the seventh dataMatch happened
  abline(v = caps[19, i], col = adjustcolor("coral", alpha = 0.75), lwd = 2) # We want to put a vertical break where the eighth dataMatch happened
  abline(v = caps[20, i], col = adjustcolor("cornflowerblue", alpha = 0.75), lwd = 2) # We want to put a vertical break where the ninth dataMatch happened
  abline(v = caps[21, i], col = adjustcolor("coral", alpha = 0.75), lwd = 2) # We want to put a vertical break where the tenth dataMatch happened
  axis(side = 1, at = midnight - 22, labels = caps[midnight, 1], las = 2) # must subtract 22 because date/time observations start at row 22

  dev.off()
}



### PART 6 ####

# Open the correct trapping data file. Add a column called "capturetime_line".
# *Important*, the trapping data file must be sorted by iButtonNumber first, and then collectDate
# Then quickly click through every single plot in the appropriate folder and identify/confirm which line
# indicates the actual capture event. Especially important when a logger was in a trap with more than one capture event.
# Save.as this file into the CaptureTimeLines folder and add "_capline" to the end of the file name

##### filename change ####
# Pull in this "_capline" file
capline <- read_xlsx("Analyses/CaptureTimeLines/MLBS_23_3_capline.xlsx")

# Change "iButtonNumber" to "pair" so that it will work with the earlier code

names(capline)[77] <- paste("pair")

# I want to tell R: create a new column called "captime". Then, look at the "capturetime_line" column. If there is a 1 in this column,
# paste the date/time from the "firstdatetime" column in the "final" data table - for the row of the "pair" that matches the "pair"
# in the capline data table.

str(capline$pair)
str(final$pair)
final$pair <- as.numeric(final$pair)

# Joining the two tables together by pair first should make this more simple
final1 <- inner_join(final, capline)




final1$captime <- ifelse(final1$capturetime_line == 1, final1$firstdatetime, NA)
final1$captime <- ifelse(final1$capturetime_line == 2, final1$seconddatetime, final1$captime)
final1$captime <- ifelse(final1$capturetime_line == 3, final1$thirddatetime, final1$captime)
final1$captime <- ifelse(final1$capturetime_line == 4, final1$fourthdatetime, final1$captime)
final1$captime <- ifelse(final1$capturetime_line == 5, final1$fifthdatetime, final1$captime)
final1$captime <- ifelse(final1$capturetime_line == 6, final1$sixthdatetime, final1$captime)
final1$captime <- ifelse(final1$capturetime_line == 7, final1$seventhdatetime, final1$captime)
final1$captime <- ifelse(final1$capturetime_line == 8, final1$eighthdatetime, final1$captime)
final1$captime <- ifelse(final1$capturetime_line == 9, final1$ninthdatetime, final1$captime)
final1$captime <- ifelse(final1$capturetime_line == 10, final1$tenthdatetime, final1$captime)



# Fix ones that missed the capture by hand
### examples...
pair250 <- subset(pairdiffs, pair == 250)
final1$captime <- ifelse(final1$pair == "250" & is.na(final1$capturetime_line), "8/9/23 2:15", final1$captime)

pair256 <- subset(pairdiffs, pair == 256)
final1$captime <- ifelse(final1$pair == "256" & is.na(final1$capturetime_line), "8/8/23 21:35", final1$captime)

### Examples for if the same button missed more than one capture
pair532 <- subset(pairdiffs, pair == 532)
final1$captime <- ifelse(final1$pair == "532" & final1$trapCoordinate.x == "J1", "8/9/23 22:10", final1$captime)
final1$captime <- ifelse(final1$pair == "532" & final1$trapCoordinate.x == "F2", "8/8/23 20:00", final1$captime)



# Now keep only the columns that I need

final1 <- final1[, -c(2:26)]




#### PART 7 ####

# I need to tell R that "captime" is either in EDT or CDT, so I need to convert the time if the grid is in EDT.
# This is because when we set the iButtons, all of them are set to CDT. So EDT needs to be the current written time +1 hr
# We can specify tz = "EDT" or tz = "America/New_York" for eastern times OR tz = "CDT" or tz = "America/Chicago" or tz = "US/CENTRAL" for our sites in the central tz

final1$captime <- mdy_hm(final1$captime, tz = "America/New_York") # This would be for a timezone in eastern daylight time
final1$captime <- final1$captime + hours(1) # And because this is an eastern site, we run this line to shift the time to one hour
# later since 1am CDT occurs at 2am EDT


final1$collection_date <- ymd(final1$collectDate)
final1$capture_date <- sapply(strsplit(as.character(final1$captime), " "), "[", 1)
final1$capture_time <- sapply(strsplit(as.character(final1$captime), " "), "[", 2)

final1$capture_date <- ymd(final1$capture_date)



# save this into the "CaptureTimes" folder stored in the working directory
##### filename change ####
write.csv(final1, "Analyses/CaptureTimes/MLBS_23_3_captimes.csv")
