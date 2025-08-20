## This script prepares the mammal for analysis and depends upon 
## objects created by 00_map-sites.R. The full pipeline is run within 
## index.Rmd.

# load libraries
library(tidyverse)
library(lubridate)

# path to NEON data directory
neon_data_dir = "neon-data/data/eight_sites"

## ---- Mammal captures ----

# load the small mammal trapping data products
mammal_prods <- readRDS(file.path(neon_data_dir, "mammal-trap-data.rds"))

# Extract a tibble of the "mammals per trap night" data object
mammals <- mammal_prods$mam_pertrapnight %>% tibble()

# number of observations in the mammal table 
n_mammals <- nrow(mammals)

# Keep only necessary columns
mammals <- mammals %>% select(
  # ID variables (location)
  uid, nightuid, domainID, siteID, plotID, trapCoordinate, recapture,
  # Location information
  plotLat = decimalLatitude, plotLon = decimalLongitude, plotElev = elevation,
  landClass = nlcdClass, 
  # time variables
  collectDate,
  # mammal ID variables
  tagID, taxonID, scientificName, nativeStatusCode,
  # captured mammal data
  sex, recapture, fate, lifeStage, testes, nipples, pregnancyStatus, vagina,
  hindfootLength, earLength, tailLength, totalLength,
  weight, 
  # mammal parasite information
  larvalTicksAttached, nymphalTicksAttached, adultTicksAttached, tickNumber, 
  additionalParasites, 
  # biological sampling IDs
  bloodSampleID, bloodSampleBarcode, fecalSampleID, fecalSampleBarcode,
  earSampleID, earSampleBarcode, hairSampleID, hairSampleBarcode, 
  voucherSampleID, voucherSampleBarcode,
  # NEON data status variables
  publicationDate, release
) %>% distinct()

# add new time variables
mammals <- mammals  %>%  
  mutate(
    year = year(collectDate),
    month = month(collectDate),
    day = day(collectDate),
    DOY = yday(collectDate),
    date = date(collectDate)
  ) %>% 
  # group these columns with the other time ones
  relocate(year:date, .before = collectDate)

## ---- Individual ID ----

# estimate individuals
iid_tab <- mammals %>% 
  group_by(siteID, tagID) %>% 
  arrange(siteID, date) %>% 
  mutate(tag_ind_num = cumsum(recapture == "N")) %>% 
  ungroup() %>% 
  mutate(
    # new_tag = gsub("NEON\\.MAM\\..*\\.(.*)", "\\1", tagID),
    ind_id = paste(siteID, tagID, tag_ind_num, sep = "."),
    iid = as.numeric(factor(ind_id))
  ) %>% 
  select(uid, ind_id, iid)

# merge in IIDs.
mammals <- full_join(mammals, iid_tab, by = "uid")
  

## ---- Event metadata ----

# extract the plot night meta data
plot_nights <- mammal_prods$mam_perplotnight %>% 
  mutate(
    year = year(collectDate),
  ) %>% 
  group_by(eventID, plotID) %>% 
  arrange(eventID, plotID, collectDate) %>% 
  mutate(event_trap_nights = n()) %>% 
  tibble()

# save the plotnight metadata
saveRDS(plot_nights, "infection-modeling/data/plotnight-metadata.rds")

# add some important meta info (i.e., eventID)
mammals <- mammals %>% 
  full_join(
    plot_nights %>%
      select(
        nightuid,
        # collectDate, 
        eventID, 
        # trapSampleMethod = mammalGridSamplingMethod,
        # samplingIssues = samplingImpractical, event_trap_nights
      ) %>% distinct(),
    by = c("nightuid")
  ) %>% 
  relocate(eventID, .after = trapCoordinate)

# check the sizes
stopifnot(nrow(mammals) == n_mammals)

## ---- Add in location data ----

# read site location data
site_locations <- readRDS('infection-modeling/data/site-location-data.rds')

# keep only columns of interest
site_locations <- site_locations %>%
  select(
    siteID, siteName, siteType = type, stateCode,
    siteLat = latitude, siteLon = longitude
  ) %>%
  distinct()

# merge with mammal data data
mammals <- full_join(mammals, site_locations, by = "siteID") %>%
  relocate(siteName:siteLon, .after = trapCoordinate)

stopifnot(nrow(mammals) == n_mammals)

# read trap location data
trap_locations <- readRDS("infection-modeling/data/trap-location-data.rds")

# keep only columns of interest
trap_locations <- trap_locations %>%
  select(
    siteID, plotID, trapCoordinate,
    trapLat = adjDecimalLatitude, trapLon = adjDecimalLongitude,
    trapElev = adjElevation
  ) %>%
  distinct()

# merge with full data
mammals <- full_join(
  mammals, trap_locations,
  by = c("siteID", "plotID", "trapCoordinate")
) %>%
  relocate(trapLat:trapElev, .after = trapCoordinate)

stopifnot(nrow(mammals) == n_mammals)

## ---- Clean tick information ----

# refactor tick loads
mammals <- mammals %>% 
  mutate(
    # update the tick counts
    tickNumber = if_else(
      larvalTicksAttached != "Y" & 
        nymphalTicksAttached != "Y" & 
        adultTicksAttached != "Y" & 
        tickNumber != "No count performed", true = "0", false = tickNumber
    ) %>% factor(levels = c("0", "1-5", "6-20", ">20"), ordered = TRUE),
    # convert the tick attached variables to logical
    nymphalTicksAttached = case_when(
      nymphalTicksAttached == "Y" ~ TRUE,
      nymphalTicksAttached == "N" ~ FALSE,
      .default = NA
    ),
    larvalTicksAttached = case_when(
      larvalTicksAttached == "Y" ~ TRUE,
      larvalTicksAttached == "N" ~ FALSE,
      .default = NA
    ),
    adultTicksAttached = case_when(
      adultTicksAttached == "Y" ~ TRUE,
      adultTicksAttached == "N" ~ FALSE,
      .default = NA
    ),
    ticks_attached = if_else(
      larvalTicksAttached | nymphalTicksAttached | adultTicksAttached,
      1, 0
    ),
    nymphalTicksAttached = as.numeric(nymphalTicksAttached),
    larvalTicksAttached = as.numeric(larvalTicksAttached),
    adultTicksAttached = as.numeric(adultTicksAttached),
    sex = if_else(sex == "U", NA, sex),
    sex_male = as.numeric(sex == "M"),
    sex_mature = vagina %in% c("both", "swollen", "plugged") |
      nipples %in% c("enlarged") |
      testes %in% c("scrotal"),
    sex_mature = as.numeric(sex_mature)
  ) 

## ---- Borrelia infection data (NEON) ----

# read the NEON tick-borne pathogen product object
pathogen_prods <- readRDS(
  file.path(
    "neon-data/data/eight_sites", "rodent-tickborne-pathogen-status.rds"
  )
)

# extract the main pathogen testing table
pathogens <- pathogen_prods$rpt2_pathogentesting %>% tibble() %>% 
  filter(grepl("burgdorferi", testPathogenName, TRUE)) %>% 
  mutate(
    year = year(.data$collectDate),
    sample_method = dplyr::case_when(
      grepl(".*\\.E$", sampleID) ~ "ear",
      grepl(".*\\.B$", sampleID) ~ "blood",
      .default = NA
    ),
    NEON_Bb_infected = case_when(
      testResult == "Positive" ~ 1,
      testResult == "Negative" ~ 0,
      .default = NA
    )
  ) %>% 
  select(
    pathTestUid = uid, sampleID, sample_method, 
    testPathogenName, NEON_Bb_infected
  )

blood_samps <- mammals %>% 
  select(uid, bloodSampleID) %>% 
  filter(complete.cases(.)) %>% 
  distinct() %>% 
  right_join(pathogens, by = c("bloodSampleID" = "sampleID"))

ear_samps <- mammals %>% 
  select(uid, earSampleID) %>% 
  filter(complete.cases(.)) %>% 
  distinct() %>% 
  right_join(pathogens, by = c("earSampleID" = "sampleID"))

NEON_infected <- bind_rows(
  blood_samps %>% 
    select(uid, sample_method, NEON_Bb_infected),
  ear_samps %>% 
    select(uid, sample_method, NEON_Bb_infected),
) %>% 
  group_by(uid) %>% 
  summarize(NEON_Bb_infected = as.numeric(any(NEON_Bb_infected == 1)))

mammals <- left_join(mammals, NEON_infected, by = "uid")

## ---- Save the mammal table ----

saveRDS(mammals, "infection-modeling/data/mammal-data.rds")

## ---- TODO ----