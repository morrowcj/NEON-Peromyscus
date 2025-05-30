# rm(list=ls())
#
# #Create ibutton dataset from non-public data:
# files<-list.files('/Users/paull/Library/CloudStorage/Box-Box/Community Outreach Education/Assignable Assets/Orrock_Martin/2022dataL0all/')
# fulldat<-data.frame()
# for(i in 1:length(files)){
#   dat<-read.delim(paste0('/Users/paull/Library/CloudStorage/Box-Box/Community Outreach Education/Assignable Assets/Orrock_Martin/2022dataL0all//',files[i]))
# fulldat<-rbind(dat, fulldat)
# }
#
# ibuttons<-fulldat %>% filter(!is.na(iButtonNumber) & !iButtonNumber%in%'') %>%
#   select(nightuid, endDate, tagID, trapCoordinate, iButtonNumber) %>%
#  mutate(link=paste0(nightuid, trapCoordinate))
#
# write.csv(ibuttons, file='/Users/paull/Library/CloudStorage/Box-Box/Community Outreach Education/Assignable Assets/Orrock_Martin/2022dataL0all/ibuttons_2022.csv',
#           row.names = F)

# This next section ingests the full ibutton dataset from teams and downloads the
# publicly available data from HARV
# You will want to loop through the public data download section to grab the
# other sites for which there are ibutton data and then rbind them into a single dataset
# Next you use the linking variable which just concatenates the nightuid column
# and the trap coordinate to join the ibutton data (not public but uploaded to Teams)
# to the public mammal data.


# First upload the file from Teams that has the ibutton data:

ibuttons <- read.csv(file = "data/ibuttons_2024L0.csv") # next year it will likely be called "ibuttons_2024.csv"


require(neonUtilities)

# Then download the publicly available data
smammals <- loadByProduct(
  startdate = "2024-01", # will have to
  enddate = "2025-01", # switch these dates
  dpID = "DP1.10072.001",
  check.size = F,
  site = c("HARV", "ORNL", "SCBI", "BLAN", "SERC", "STEI", "UNDE", "MLBS"),
  package = "basic",
  # token = Sys.getenv('NEON_PAT'),
  include.provisional = TRUE
)
list2env(smammals, envir = .GlobalEnv)

mam_trapNight_nodups <- neonOS::removeDups(
  data = mam_pertrapnight,
  variables = variables_10072,
  table = "mam_pertrapnight"
)



dups <- subset(mam_trapNight_nodups, duplicateRecordQF == 2) ## this code is not working so I'll inspect and see if I can fix it manually

# Looks like these are all "trap not set" records so it's not a problem and they will be removed anyway



caps <- subset(mam_trapNight_nodups, trapStatus == "5 - capture" | trapStatus == "4 - more than 1 capture in one trap")

# keep only our grids
caps <- subset(caps, plotID == "BLAN_009" | plotID == "HARV_008" | plotID == "MLBS_006" | plotID == "ORNL_003" | plotID == "SCBI_008" | plotID == "SERC_015" |
  plotID == "STEI_012" | plotID == "UNDE_027")


# Create a linking variable in the publicly available data:
caps$link <- paste0(caps$nightuid, caps$trapCoordinate)

dups <- caps[duplicated(caps$link), ] # these are instances when there was more than 1 capture in a trap so it makes sense that there are duplicates

require(dplyr)

# Link the ibutton dataset in the Files folder on Teams to the downloaded small mammal dataset:
joindat <- left_join(caps, ibuttons, by = "link")

# because there were 4 traps that had more than 1 capture, these have duplicated and I Need to fix it

joindatdups <- joindat[duplicated(joindat[, c(7, 18, 20)]), ]

joindat <- anti_join(joindat, joindatdups)

caps <- joindat

require(lubridate)

caps$collectDate <- ymd(caps$collectDate)
caps$year <- year(caps$collectDate)
caps$month <- month(caps$collectDate)



write.csv(caps, file = "data/out_files/mammalcaps_2024.csv") # save as 2024



########


require(readxl)
require(writexl)


caps <- read.csv(file = "data/out_files/mammalcaps_2024.csv") # save as 2024






### Specify each distinct trapping session in this full capsframe (before any captures are subset out)
# create a "Session" column that will lump dates that are within 7 days of each other

require(lubridate)

caps$collectDate <- ymd(caps$collectDate)

caps <- caps[with(caps, order(plotID, collectDate)), ]

Test <- data.frame(cumsum(c(abs(difftime(caps$collectDate, lag(caps$collectDate, default = first(caps$collectDate)), units = "days"))) > 7))
caps <- cbind(caps, Test)

names(caps)[80] <- paste("Session")

# caps now has this Session column... this is every single trapping occasion that neon did in 2022

summary(caps$Session)


#### Split the Sampling Bouts #####

### Note that all of these file names need to be reflected to represent 2024 trapping bouts - there will likely be 4 each again ###



## BLAN ###
BLAN_23 <- subset(caps, plotID == "BLAN_009")
summary(as.factor(BLAN_23$Session))

BLAN_23_1 <- subset(BLAN_23, Session == 0)
BLAN_23_2 <- subset(BLAN_23, Session == 1)
BLAN_23_3 <- subset(BLAN_23, Session == 2)
BLAN_23_4 <- subset(BLAN_23, Session == 3)


write_xlsx(BLAN_23_1, "data/out_files/BLAN_23_1.xlsx")
write_xlsx(BLAN_23_2, "data/out_files/BLAN_23_2.xlsx")
write_xlsx(BLAN_23_3, "data/out_files/BLAN_23_3.xlsx")
write_xlsx(BLAN_23_4, "data/out_files/BLAN_23_4.xlsx")



## HARV ###
HARV_23 <- subset(caps, plotID == "HARV_008")
summary(as.factor(HARV_23$Session))

HARV_23_1 <- subset(HARV_23, Session == 4)
HARV_23_2 <- subset(HARV_23, Session == 5)
HARV_23_3 <- subset(HARV_23, Session == 6)
HARV_23_4 <- subset(HARV_23, Session == 7)



write_xlsx(HARV_23_1, "data/out_files/HARV_23_1.xlsx")
write_xlsx(HARV_23_2, "data/out_files/HARV_23_2.xlsx")
write_xlsx(HARV_23_3, "data/out_files/HARV_23_3.xlsx")
write_xlsx(HARV_23_4, "data/out_files/HARV_23_4.xlsx")



## MLBS ###
MLBS_23 <- subset(caps, plotID == "MLBS_006")
summary(as.factor(MLBS_23$Session))

MLBS_23_1 <- subset(MLBS_23, Session == 8)
MLBS_23_2 <- subset(MLBS_23, Session == 9)
MLBS_23_3 <- subset(MLBS_23, Session == 10)
MLBS_23_4 <- subset(MLBS_23, Session == 11)


write_xlsx(MLBS_23_1, "data/out_files/MLBS_23_1.xlsx")
write_xlsx(MLBS_23_2, "data/out_files/MLBS_23_2.xlsx")
write_xlsx(MLBS_23_3, "data/out_files/MLBS_23_3.xlsx")
write_xlsx(MLBS_23_4, "data/out_files/MLBS_23_4.xlsx")



## ORNL ###
ORNL_23 <- subset(caps, plotID == "ORNL_003")
summary(as.factor(ORNL_23$Session))

ORNL_23_1 <- subset(ORNL_23, Session == 12)
ORNL_23_2 <- subset(ORNL_23, Session == 13)
ORNL_23_3 <- subset(ORNL_23, Session == 14)
ORNL_23_4 <- subset(ORNL_23, Session == 15)



write_xlsx(ORNL_23_1, "data/out_files/ORNL_23_1.xlsx")
write_xlsx(ORNL_23_2, "data/out_files/ORNL_23_2.xlsx")
write_xlsx(ORNL_23_3, "data/out_files/ORNL_23_3.xlsx")
write_xlsx(ORNL_23_4, "data/out_files/ORNL_23_4.xlsx")



## SCBI ###
SCBI_23 <- subset(caps, plotID == "SCBI_008")
summary(as.factor(SCBI_23$Session))

SCBI_23_1 <- subset(SCBI_23, Session == 16)
SCBI_23_2 <- subset(SCBI_23, Session == 17)
SCBI_23_3 <- subset(SCBI_23, Session == 18)
SCBI_23_4 <- subset(SCBI_23, Session == 19)


write_xlsx(SCBI_23_1, "data/out_files/SCBI_23_1.xlsx")
write_xlsx(SCBI_23_2, "data/out_files/SCBI_23_2.xlsx")
write_xlsx(SCBI_23_3, "data/out_files/SCBI_23_3.xlsx")
write_xlsx(SCBI_23_4, "data/out_files/SCBI_23_4.xlsx")



## SERC ###
SERC_23 <- subset(caps, plotID == "SERC_015")
summary(as.factor(SERC_23$Session))

SERC_23_1 <- subset(SERC_23, Session == 20)
SERC_23_2 <- subset(SERC_23, Session == 21)
SERC_23_3 <- subset(SERC_23, Session == 22)
SERC_23_4 <- subset(SERC_23, Session == 23)


write_xlsx(SERC_23_1, "data/out_files/SERC_23_1.xlsx")
write_xlsx(SERC_23_2, "data/out_files/SERC_23_2.xlsx")
write_xlsx(SERC_23_3, "data/out_files/SERC_23_3.xlsx")
write_xlsx(SERC_23_4, "data/out_files/SERC_23_4.xlsx")




## STEI ###
STEI_23 <- subset(caps, plotID == "STEI_012")
summary(as.factor(STEI_23$Session))

STEI_23_1 <- subset(STEI_23, Session == 24)
STEI_23_2 <- subset(STEI_23, Session == 25)
STEI_23_3 <- subset(STEI_23, Session == 26)
STEI_23_4 <- subset(STEI_23, Session == 27)



write_xlsx(STEI_23_1, "data/out_files/STEI_23_1.xlsx")
write_xlsx(STEI_23_2, "data/out_files/STEI_23_2.xlsx")
write_xlsx(STEI_23_3, "data/out_files/STEI_23_3.xlsx")
write_xlsx(STEI_23_4, "data/out_files/STEI_23_4.xlsx")






## UNDE ###
UNDE_23 <- subset(caps, plotID == "UNDE_027")
summary(as.factor(UNDE_23$Session))

UNDE_23_1 <- subset(UNDE_23, Session == 28)
UNDE_23_2 <- subset(UNDE_23, Session == 29)
UNDE_23_3 <- subset(UNDE_23, Session == 30)
UNDE_23_4 <- subset(UNDE_23, Session == 31)




write_xlsx(UNDE_23_1, "data/out_files/UNDE_23_1.xlsx")
write_xlsx(UNDE_23_2, "data/out_files/UNDE_23_2.xlsx")
write_xlsx(UNDE_23_3, "data/out_files/UNDE_23_3.xlsx")
write_xlsx(UNDE_23_4, "data/out_files/UNDE_23_4.xlsx")
