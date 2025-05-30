# ---- Script packages ----
library(dplyr)
library(lutz)
library(lubridate)

# ---- Script parameters ----

# location of the fob-trap data
fob_trap_path <- file.path(
  "activity-timing/data/ibuttons/cleaned-metadata/neon-trap-ibutton-pairs_2024.rds"
)

# location of the neon data
# neon_data_dir <- "R:/morrow5_data_repo/NEON-products/eight_sites_data"
neon_data_dir <- "data-curation/data/eight_sites/"

location_data_dir <- file.path(neon_data_dir, "location_data")

# location of the mammal capture data
mammal_path <- file.path(neon_data_dir, "mammal-trap-data.rds")

# directory to save the output file
out_dir <- "activity-timing/data/ibuttons"

# path to the output files
cap_out <- file.path(out_dir, "neon-captures-with-ibutton-pairs_2024.rds")

# path to the trap locaton meta data.
trap_location_path <- file.path(location_data_dir, "small-mammal-trap-locations.rds")

# should the output file be overwritten if it already exists?
overwrite = FALSE 

# ---- Load Data ----

# cleaned fob-trap info
fob_traps <- readRDS(fob_trap_path)

# location info
neon_locations <- readRDS(trap_location_path)

# keep only relevant columns
neon_locations <- neon_locations |> select(
  siteID, plotID, trapCoordinate, time_zone,
  adjDecimalLatitude, adjDecimalLongitude, adjElevation
)

# get the downloaded NEON traping data
neon_trappings <- readRDS(file = mammal_path)

# pull out the capture table and clean it up a bit
capture_dat <- neon_trappings$mam_pertrapnight |> 
  tibble() |> 
  left_join(neon_locations, by = c("siteID", "plotID", "trapCoordinate")) 

# add in the relevant location info
  
capture_dat |>   
  mutate(
    collectDate = ymd(collectDate), 
    endDate = ymd(endDate)
  ) 
  
# # non-captures should not have a collect date
# capture_dat$collectDate[is.na(capture_dat$tagID)] <- NA

# ---- Combine with capture data ----

# join on night ID, tag ID, and trap
joined_traps <- left_join(
  fob_traps, capture_dat, 
  by = c("nightuid", "tagID", "trapCoordinate"), 
  na_matches = "na"
) |> 
  filter(!is.na(tagID))


# ---- Assign trapping events (bouts) for the year ----

# add in within-plot trapping bouts
joined_traps <- joined_traps |> 
  arrange(siteID, plotID, collectDate) |> 
  mutate(
    abs_lagdiff = lag(collectDate, default = first(collectDate)) %>% 
      difftime(collectDate, ., units = "days") |> 
      abs(),
    year_bout = cumsum(abs_lagdiff > 7) + 1
  ) |> 
  select(-"abs_lagdiff")

# ---- Calculate the within-plot bouts ----

# highest bout number
max_bout = max(joined_traps$year_bout, na.rm = TRUE)
max_bout

# bout lookup table, with 1:4
plot_bout_table <- tibble(
  year_bout = 1:max_bout, plot_bout = rep(1:4, times = max_bout/4)
)

# index of the matched year bouts to site bouts
match_ind <- match(joined_traps$year_bout, plot_bout_table$year_bout)

# assign within-site bout numbers
joined_traps$plot_bout <- plot_bout_table$plot_bout[match_ind]


# save the file
if (overwrite | !file.exists(cap_out)) {
  saveRDS(joined_traps, file = cap_out)
} else {
  warning("file already exists")
}

