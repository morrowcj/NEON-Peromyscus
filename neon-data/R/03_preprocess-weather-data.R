library(tidyverse)

# ---- Air temperatures ----

# path to input folder
airtemp_dir = "neon-data/data/eight_sites/air-temperatures/"
# paths to individual input files
airtemp_files <- list.files(file.path(airtemp_dir), full.names = TRUE)

# file to store all the data
all_airtemps <- tibble()

# prepare progress bar
pb = txtProgressBar(initial = 0, max = length(airtemp_files), style = 3)

# loop through all the files...
for (i in seq_len(length(airtemp_files))) {
  # get the file for this loop
  this_airtemp <- airtemp_files[i]
  
  # load in the data
  airprod <- readRDS(this_airtemp)
  
  # join the data with some sensor info
  airtemps <- airprod$TAAT_1min %>% 
    mutate(
      HOR.VER = paste(horizontalPosition, verticalPosition, sep = "."),
      date = date(startDateTime), year = year(startDateTime), 
      month = month(startDateTime), DOY = yday(startDateTime)
    ) %>% 
    left_join(
      airprod$sensor_positions_00003 %>% tibble() %>% 
        select(
          HOR.VER, sensorLocationID, sensorLocationDescription, 
          referenceLocationID,
          locationReferenceLatitude:locationReferenceElevation
        ) %>% distinct(),
      by = "HOR.VER"
    ) %>% 
    # calculate daily values
    group_by(
      siteID, referenceLocationID, sensorLocationID,
      locationReferenceLatitude, locationReferenceLongitude, 
      locationReferenceElevation,
      HOR.VER, year, month, DOY, 
      date
    ) %>% 
    summarize(
      temp_day_min = min(tempTripleMean, na.rm = TRUE),
      temp_day_mean = mean(tempTripleMean, na.rm = TRUE),
      temp_day_sd = sd(tempTripleMean, na.rm = TRUE), # standard deviation of 1-min means
      temp_day_max = max(tempTripleMean, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    # convert non-finite values to NA
    mutate(
      across(temp_day_min:temp_day_max, ~if_else(is.finite(.x), .x, NA))
    ) %>% 
    arrange(year, HOR.VER, DOY)
  
  # combine with the other data
  all_airtemps <- bind_rows(all_airtemps, airtemps)
  
  # increment progress bar
  setTxtProgressBar(pb, value = i)
}

# check the data (visually)
all_airtemps %>% 
  ggplot(aes(x = date, y = temp_day_mean, colour = referenceLocationID)) + 
  facet_wrap(~siteID) +
  geom_line(alpha = 0.5)

all_airtemps$temp_day_max %>% summary()
all_airtemps$temp_day_min %>% summary()

# and save it 
saveRDS(
  object = all_airtemps, 
  file = "neon-data/data/eight_sites/daily-air-temperatures.rds"
)

# ---- Precipitation ----

# path to input folder
precip_dir = "neon-data/data/eight_sites/precipitation/"
# paths to individual input files
precip_files <- list.files(file.path(precip_dir), full.names = TRUE)

# file to store all the data
all_precips <- tibble()

# prepare progress bar
pb = txtProgressBar(initial = 0, max = length(precip_files), style = 3)

# loop through all the files...
for (i in seq_len(length(precip_files))) {
  # get the file for this loop
  this_precip <- precip_files[i]
  
  # load in the data
  precip_prod <- readRDS(this_precip)
  
  # Through fall gauges
  throughfall <- precip_prod$THRPRE_1min %>% 
    mutate(
      HOR.VER = paste(horizontalPosition, verticalPosition, sep = "."),
      date = date(startDateTime), year = year(startDateTime), 
      month = month(startDateTime), DOY = yday(startDateTime)
    ) %>% 
    left_join(
      precip_prod$sensor_positions_00006 %>% tibble() %>% 
        select(
          HOR.VER, sensorLocationID, sensorLocationDescription, 
          referenceLocationID, 
          locationReferenceLatitude:locationReferenceElevation
        ) %>% distinct(),
      by = "HOR.VER"
    )  %>% 
    # calculate daily values
    group_by(
      siteID, referenceLocationID, sensorLocationID, , 
      locationReferenceLatitude, locationReferenceLongitude, 
      locationReferenceElevation,
      HOR.VER, year, month, DOY, 
      date
    ) %>% 
    summarize(
      daily_precip = sum(TFPrecipBulk, na.rm = TRUE),
      precip_method = "throughfall",
      .groups = "drop"
    ) %>% 
    mutate(
      daily_precip = if_else(is.finite(daily_precip), daily_precip, NA),
    ) %>% group_by(year, HOR.VER) %>% 
    arrange(year, DOY) 
  
  # Secondary gauges
  secondary <- precip_prod$SECPRE_1min %>% 
    mutate(
      HOR.VER = paste(horizontalPosition, verticalPosition, sep = "."),
      date = date(startDateTime), year = year(startDateTime), 
      month = month(startDateTime), DOY = yday(startDateTime)
    ) %>% 
    left_join(
      precip_prod$sensor_positions_00006 %>% tibble() %>% 
        select(
          HOR.VER, sensorLocationID, sensorLocationDescription, 
          referenceLocationID, 
          locationReferenceLatitude:locationReferenceElevation
        ) %>% distinct(),
      by = "HOR.VER"
    )  %>%
    # calulate daily values
    group_by(
      siteID, referenceLocationID, sensorLocationID, 
      locationReferenceLatitude, locationReferenceLongitude, 
      locationReferenceElevation,
      HOR.VER, year, month, DOY, 
      date
    ) %>% 
    summarize(
      daily_precip = sum(secPrecipBulk, na.rm = TRUE),
      precip_method = "secondary",
      .groups = "drop"
    ) %>% 
    mutate(
      daily_precip = if_else(is.finite(daily_precip), daily_precip, NA)
    ) %>% 
    arrange(year, HOR.VER, DOY)
  
  # join the two types
  precip <- bind_rows(throughfall, secondary)
  
  # combine with other sites
  all_precips <- bind_rows(all_precips, precip)
  
  # increment progress bar
  setTxtProgressBar(pb, i)
}

# check the data (visually)
all_precips %>% 
  ggplot(
    aes(
      x = date, y = daily_precip, colour = grepl("TOWER", referenceLocationID)
    )
  ) + 
  facet_wrap(~siteID) +
  geom_line(alpha = 0.5, aes(group = referenceLocationID))

# check site locations
all_precips %>% 
  select(
    siteID, referenceLocationID, 
    locationReferenceLongitude, locationReferenceLatitude
  ) %>% distinct() %>% 
  ggplot(aes(x = locationReferenceLongitude, y = locationReferenceLatitude)) +
    facet_wrap(~siteID, scales = "free") + 
    geom_point(
      aes(color = grepl("TOWER", referenceLocationID)), show.legend = FALSE
    )

# how correlated are the methods?
all_precips %>% 
  pivot_wider(
    names_from = precip_method, values_from = daily_precip, 
    names_prefix = "precip_"
  ) %>% 
  group_by(siteID, date) %>% 
  summarize(
    precip_throughfall = mean(precip_throughfall, na.rm = TRUE),
    precip_secondary = mean(precip_secondary, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = precip_throughfall, y = precip_secondary, col = siteID)) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  geom_point() + 
  geom_smooth(method = "lm", formula = "y ~ x")

# and save it 
saveRDS(
  object = all_precips, 
  file = "neon-data/data/eight_sites/daily-secondary-precips.rds"
)

# ---- Primary precips ----
# path to input folder
precip_dir = "neon-data/data/eight_sites/precip-weights/"
# paths to individual input files
precip_files <- list.files(file.path(precip_dir), full.names = TRUE)

# file to store all the data
primary_precips <- tibble()

# prepare progress bar
pb = txtProgressBar(initial = 0, max = length(precip_files), style = 3)

# loop through all the files...
for (i in seq_len(length(precip_files))) {
  # get the file for this loop
  this_precip <- precip_files[i]
  
  # load in the data
  precip_prod <- readRDS(this_precip)
  
  # skip this iteration, if there was no data for the site
  if (class(precip_prod) == "try-error") {
    next
  }
  
  # collect daily precips from the primary measure
  these_precips <- precip_prod$WEIPRE_daily %>% 
    tibble() %>% 
    mutate(
      HOR.VER = paste(horizontalPosition, verticalPosition, sep = "."),
      date = date(date), year = year(date), 
      month = month(date), DOY = yday(date),
      precip_method = "primary"
    ) %>% 
    left_join(
        precip_prod$sensor_positions_00044 %>% tibble() %>% 
        select(
          HOR.VER, sensorLocationID, sensorLocationDescription, 
          referenceLocationID, 
          locationReferenceLatitude:locationReferenceElevation
        ) %>% distinct(),
      by = "HOR.VER"
    ) %>% 
    arrange(HOR.VER, year, DOY) %>% 
    mutate(daily_precip = precipBulk) %>% 
    select(all_of(names(all_precips))) 
  
  # combine with other sites
  primary_precips <- bind_rows(primary_precips, these_precips)
  
  # increment progress bar
  setTxtProgressBar(pb, i)
}

# check the data
primary_precips %>% 
  ggplot(aes(x = date, y = daily_precip)) + 
  facet_wrap(~siteID) + 
  geom_line(aes(col = referenceLocationID), alpha = 0.5)

# save them
saveRDS(
  object = primary_precips, 
  file = "neon-data/data/eight_sites/daily-primary-precips.rds"
)

# join primary precip with the secondary
joined_precips <- bind_rows(all_precips, primary_precips) %>% 
  arrange(siteID, HOR.VER, year, DOY)

# save the combined data
saveRDS(
  object = joined_precips,
  file = "neon-data/data/eight_sites/all-daily-precips.rds"
)

# look at the different methods
method_precips <- joined_precips %>% 
  group_by(siteID, date, precip_method) %>% 
  summarize(
    mean_precip = mean(daily_precip, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  arrange(siteID, precip_method, date)

method_precips %>% 
  filter(!is.na(mean_precip)) %>% 
  ggplot(aes(x = date, y = mean_precip, col = precip_method)) + 
  facet_wrap(~siteID, scales = "free") + 
  # geom_line(alpha = 0.5) +
  geom_col(position = "dodge", aes(fill = precip_method))

# correlation matrix for the different metrics of rain (given that it rained)
method_precips %>% 
  pivot_wider(names_from = precip_method, values_from = mean_precip) %>% 
  select(-c(siteID:date)) %>% 
  filter(throughfall > 0 | secondary > 0 | primary > 0) %>% # it rained
  cor(use = "pairwise.complete.obs") %>% round(2)

# ---- Relative humidity ----
humid_dir = "neon-data/data/eight_sites/relative-humidity/"

# paths to individual input files
humid_files <- list.files(file.path(humid_dir), full.names = TRUE)

# file to store all the data
all_humids <- tibble()

# prepare progress bar
pb = txtProgressBar(initial = 0, max = length(humid_files), style = 3)

# loop through all the files...
for (i in seq_len(length(precip_files))) {
  # get the file for this loop
  this_humid <- humid_files[i]
  
  # load in the data
  humid_prod <- readRDS(this_humid)
  
  # Through fall gauges
  humid <- humid_prod$RH_1min %>% 
    mutate(
      HOR.VER = paste(horizontalPosition, verticalPosition, sep = "."),
      date = date(startDateTime), year = year(startDateTime), 
      month = month(startDateTime), DOY = yday(startDateTime)
    ) %>% 
    left_join(
      humid_prod$sensor_positions_00098 %>% tibble() %>% 
        select(
          HOR.VER, sensorLocationID, sensorLocationDescription, 
          referenceLocationID, 
          locationReferenceLatitude:locationReferenceElevation
        ) %>% distinct(),
      by = "HOR.VER"
    ) %>% 
    group_by(
      siteID, referenceLocationID, sensorLocationID, 
      locationReferenceLatitude, locationReferenceLongitude, 
      locationReferenceElevation,
      HOR.VER, year, month, DOY, 
      date
    ) %>% 
    summarize(
      # relative humidity
      RH_day_min = min(RHMean, na.rm = TRUE),
      RH_day_mean = mean(RHMean, na.rm = TRUE),
      RH_day_sd = sd(RHMean, na.rm = TRUE), # standard deviation of 1-min means
      RH_day_max = max(RHMean, na.rm = TRUE),
      # dew/frost point
      dew_day_min = min(dewTempMean, na.rm = TRUE),
      dew_day_mean = mean(dewTempMean, na.rm = TRUE),
      dew_day_sd = sd(dewTempMean, na.rm = TRUE),
      dew_day_max = max(dewTempMean, na.rm = TRUE),
      # RH sensor temperature
      tempRH_day_min = min(tempRHMean, na.rm = TRUE),
      tempRH_day_mean = mean(tempRHMean, na.rm = TRUE),
      tempRH_day_sd = sd(tempRHMean, na.rm = TRUE),
      tempRH_day_max = max(tempRHMean, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    mutate(
      across(RH_day_min:tempRH_day_max, ~if_else(is.finite(.x), .x, NA))
    ) %>% 
    arrange(year, DOY) 
    
    # combine with the other data
    all_humids <- bind_rows(all_humids, humid)
    
    # increment progress bar
    setTxtProgressBar(pb, value = i)
}

# check the data (visually)
all_humids %>% 
  ggplot(aes(x = date, y = RH_day_mean, colour = referenceLocationID)) + 
  facet_wrap(~siteID) +
  geom_line(alpha = 0.5)

# remove the clear outlier observation days (anything above 120 RH)
all_humids <- all_humids %>% 
  mutate(
    too_big = if_else(
      RH_day_min > 120 | RH_day_mean > 120 | RH_day_max > 120, 
      TRUE, FALSE
    ),
    across(RH_day_min:RH_day_max, ~if_else(too_big, NA, .x))
  ) %>% select(-too_big)

# re-check the data (visually)
all_humids %>% 
  ggplot(aes(x = date, y = RH_day_mean, colour = referenceLocationID)) + 
  facet_wrap(~siteID) + 
  geom_hline(yintercept = c(0, 100), col = "black", linetype = "dotted") + 
  geom_line(alpha = 0.5)

# now check the temperatures:
all_humids %>% 
  ggplot(aes(x = date, y = tempRH_day_mean, color = referenceLocationID)) +
  facet_wrap(~siteID) + 
  # geom_hline(yintercept = c(0, 100), col = "black", linetype = "dotted") + 
  geom_line(alpha = 0.5)

# and remove clear outliers (again)
all_humids <- all_humids %>% 
  mutate(
    too_small = if_else(tempRH_day_min < -60 | tempRH_day_mean < -60, TRUE, FALSE),
    too_big = if_else(tempRH_day_mean > 60 | tempRH_day_max > 60, TRUE, FALSE),
    across(tempRH_day_min:tempRH_day_max, ~if_else((too_big | too_small), NA, .x))
  ) %>% 
  select(-too_small, -too_big)

# now re-check the temperatures:
all_humids %>% 
  ggplot(aes(x = date, y = tempRH_day_mean, color = referenceLocationID)) +
  facet_wrap(~siteID) + 
  # geom_hline(yintercept = c(0, 100), col = "black", linetype = "dotted") + 
  geom_line(alpha = 0.5)

# and finally the dew points
all_humids %>% 
  ggplot(aes(x = date, y = dew_day_mean, color = referenceLocationID)) + 
  facet_wrap(~siteID) + 
  geom_line(alpha = 0.5)

all_humids <- all_humids %>% 
  mutate(
    too_high = if_else(dew_day_mean > 35 | dew_day_max > 35, TRUE, FALSE),
    across(dew_day_min:dew_day_max, ~if_else(too_high, NA, .x))
  ) %>% 
  select(-too_high)

# revisit the dew points
all_humids %>% 
  ggplot(aes(x = date, y = dew_day_mean, color = referenceLocationID)) + 
  facet_wrap(~siteID) + 
  geom_line(alpha = 0.5)

# and save it 
saveRDS(
  object = all_humids, 
  file = "neon-data/data/eight_sites/daily-relative-humidities.rds"
)