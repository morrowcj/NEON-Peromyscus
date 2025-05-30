# ---- Script packages ----
# library(lutz) # to look up time zones
library(lubridate)
library(tidyverse)

# ---- Script parameters ----

# path to the data from previous years
previous_years_path <- "data/ibuttons/biol_data_22-23_8-7.csv"

# path to NEON mammal trap data
neon_trapping_path <- file.path(
  "R:/morrow5_data_repo/NEON-products/eight_sites_data/mammal-trap-data.rds"
)

# path to the capture data from 2024 that we've previously processed
new_captures_path <- "data/ibuttons/capture_time_estimates_2024.rds"

# path to save the full capture times
captime_out_path <- "data/ibuttons/capture-time-trapping-data_2022-2024.rds"

# path to location information
site_location_path <- "R:/morrow5_data_repo/NEON-products/eight_sites_data/location_data/site-centroid-locations.rds"

# ---- Load the data ----

site_locations <- readRDS(site_location_path)

# neon small mammal trapping object
neon_data <- readRDS(neon_trapping_path)

# actual trap data table
trappings <- neon_data$mam_pertrapnight |> 
  tibble() |> 
  mutate(collectDate = ymd(collectDate), endDate = ymd(endDate))

# data from Alli, containing capture time estimates from 2022-2023
previous_data <- read.csv(previous_years_path, row.names = 1) |> tibble() |> 
  distinct()

# get the time zone for Madison WI
MSN_tz <- tz_lookup_coords(
  lat = 43.073051, lon = -89.401230, method = "accurate"
)

# the captures I estimated for 2024
new_captures <- readRDS(new_captures_path) |> tibble() |> 
  distinct()

# check that the ID columns are unique identifiers
new_captures |> select(
  siteID, plotID, collectDate, trapCoordinate, tagID
) %>% nrow() |> all.equal(nrow(new_captures)) |> stopifnot()

# rearrange the columns
key_columns <- c(
  "year", "bout_label", "siteID", "plot_bout", "plotID", "nightuid", 
  "collectDate", "trapCoordinate", "tagID", "trapStatus", 
  # "scientificName", # "taxonID",
  "uid", "decimalLongitude", "decimalLatitude", "time_zone", "SN_In", "SN_Out",
  "ibutton_pair", "local_midnight", "sunset", "sunrise", "trapping_interval",
  "ibutton_data", "possible_cap_times", "which_captime", "capture_time",
  "captime_issue"
)

new_captures <- new_captures |> 
  mutate(year = year(collectDate)) |> 
  select(all_of(key_columns))

# ---- Clean up the 2022-2023 data ----

# get missing ID columns from the trapping data
## Note that the taxonID and species info have been updated
trap_join_cols <- c(
  "nightuid", "uid", "plotID", "collectDate", "trapCoordinate", "tagID"
)

previous_data <- previous_data |> 
  rename(endDate = endDate.x, trapCoordinate = trapCoordinate.x,
         taxonID.update = taxonID, scientificName.update = scientificName) |> 
  mutate(
    collectDate = mdy(collectDate), endDate = mdy(endDate)
  ) |> 
  left_join(trappings |> select(c(all_of(trap_join_cols), taxonID, scientificName)))

# unmatched rows...
unmatched <- previous_data |> filter(is.na(nightuid) | is.na(uid)) |> 
  select(
    nightuid, uid, plotID, collectDate, trapCoordinate, trapStatus, tagID,
  )

# add in time zone info
previous_data <- previous_data |> 
  left_join(site_locations |> select(siteID, time_zone))

# get only the relevant columns and create others, with proper formatting
cleaned_prevs <- previous_data |> 
  mutate(
    year = year(collectDate),
    capture_date = ymd(capture_date), 
    ibutton_pair = stringr::str_pad(iButtonNumber, 3, "left", "0"),
    captime_issue = ifelse(is.na(ibutton_pair), "no pair ID", NA),
    SN_In = NA, SN_Out = NA, 
    ibutton_data = list(tibble()), 
    possible_cap_times = list(tibble()), 
    which_captime = NA
  ) |> 
  rowwise() |> 
  mutate(
    local_midnight = as_datetime(collectDate, tz = time_zone),
    capture_time = hms(capture_time),
    capture_time = as_datetime(if_else(
      hour(capture_time) >= 12,
      local_midnight - days(1) + capture_time,
      local_midnight + capture_time
    ), tz = time_zone),
    sunset = sunset(
      local_midnight - days(1), decimalLongitude, decimalLatitude,
      tz = .data$time_zone, force_tz = FALSE
    ),
    sunrise = sunrise(
      local_midnight, decimalLongitude, decimalLatitude, tz = .data$time_zone,
      force_tz = FALSE
    ),
    trapping_interval = interval(
      start = sunset - hours(3), end = sunrise + hours(3)
    )
  ) |> 
  ungroup() |> group_by(year, siteID, plotID) |>
  arrange(year, siteID, plotID, collectDate) |>
  mutate(
    lag_diff = lag(collectDate, default = first(collectDate)) %>%
      difftime(collectDate, ., units = "days") |> abs(),
    plot_bout = cumsum(lag_diff > 7) + 1,
    bout_label = paste(siteID, plot_bout, sep = "_")
  ) |> 
  select(all_of(key_columns), taxonID.update, scientificName.update) |> 
  ungroup()

# ---- join 2022-2023 capture estimates with 2024 ----

# join them by the key columns
all_captures <- full_join(new_captures, cleaned_prevs, by = key_columns)

# check the dimensions are correct
stopifnot(nrow(all_captures) == nrow(new_captures) + nrow(cleaned_prevs))

# add in the information from the trapping data
full_captures <- left_join(
  all_captures |> select(-c(decimalLatitude:decimalLongitude)),
  trappings
) 

new_columns <- names(full_captures)[
  !names(full_captures) %in% names(all_captures)
]

# collapse the NEON trap data variables into one list column
full_captures <- full_captures |> 
  nest(neon_trap_data = c(all_of(new_columns), trapStatus, uid, nightuid))

# check that all the list-column elements have exactly 1 row.
full_captures |> rowwise() |> filter(nrow(neon_trap_data) != 1) |> 
  nrow() |> all.equal(0) |> stopifnot()

# calculate night length attributes
full_captures <- full_captures |> 
  rowwise() |> 
  mutate(
    night_length = time_length(sunrise - sunset, unit = "hours"),
    cap_after_sunset = time_length(capture_time - sunset, unit = "hours"),
    cap_before_sunrise = time_length(sunrise - capture_time, unit = "hours"),
    cap_prop_night = cap_after_sunset / night_length
  ) |> 
  ungroup()

# ---- Diagnose night lenght issues ----

# full_captures |> 
#   filter(!is.na(capture_time)) |> 
#   select(
#     collectDate, 
#     # sunset, sunrise, local_midnight, 
#     capture_time, night_length:cap_prop_night
#   ) 

full_captures |> 
  filter(!is.na(capture_time)) |> 
  mutate(night_group = cap_prop_night > 1.25) |> 
  group_by(year, night_group) |> 
  tally()

# ---- Save output ----
saveRDS(full_captures, file = captime_out_path)
