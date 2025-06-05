# ----- Script packages -----

# load packages
library(dplyr)
library(lutz)

# ---- Script parameters ----

# data where the relevant NEON data are stored
# data_dir <- "R:/morrow5_data_repo/NEON-products/eight_sites_data"
data_dir <- "data-curation/data/eight_sites"
# where should the output be saved?
# out_dir <- "R:/morrow5_data_repo/NEON-products/eight_sites_data/location_data"
out_dir <- "data-curation/data/eight_sites/location_data"
# path to of the mammal trapping data file
mammal_path <- file.path(data_dir, "mammal-trap-data.rds")
# api token string
# api_token <- readLines("docs/NEON-API-token-morrow5.txt", n = 1, warn = FALSE)
source("data-curation/R/00_NEON-API-token.R")
.setNeonTokenGlobal(file = "NEON-API-token-morrow5.txt")
# should things be rebuilt even if they exists?
force_build = FALSE 

# ---- Create save directory----

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# ---- Load small mammal capture product ----

# get the downloaded NEON traping data
neon_trappings <- readRDS(file = mammal_path)

# extract the captures per trap night table
trap_dat <- neon_trappings$mam_pertrapnight |> 
  tibble()

# get the metadata for the trap data
trap_meta <- neon_trappings$variables_10072 |> 
  tibble() |> 
  filter(table == "mam_pertrapnight")

# ---- Get NEON site and plot Coordinates ----

# path of the site location save file
site_loc_save_path <- file.path(out_dir, "site-centroid-locations.rds")

if (force_build | !file.exists(site_loc_save_path)) {
  # all sites
  sites = unique(trap_dat$siteID)
  
  # get the site locations
  site_location_table <- sapply(
    X = sites,
    FUN = function(x){geoNEON::getLocBySite(x, token = NEON_TOKEN)}
  ) |> bind_rows() |> 
    tibble()
  
  site_location_table <- site_location_table |> 
    mutate(
      across(
        c(decimalLatitude:northing, utmZoneNumber:zOffset), 
        ~as.numeric(.x)
      ),
      time_zone = tz_lookup_coords(
        lat = decimalLatitude, lon = decimalLongitude, method = "accurate"
      )
    )
  
  # save the site locations
  saveRDS(object = site_location_table, file = site_loc_save_path)
} else {
  # load the site locations instead
  site_location_table = readRDS(site_loc_save_path)
}

# path of the plot location to save the file
plot_loc_save_path <- file.path(out_dir, "plot-centroid-locations.rds")

if (force_build | !file.exists(plot_loc_save_path)) {
  # get the location information for each named location (1 per plotID)
  plot_location_table <- geoNEON::getLocByName(
    data = data.frame(namedLocation = unique(trap_dat$namedLocation)), 
    locCol = "namedLocation", 
    locOnly = TRUE, history = FALSE ,token = NEON_TOKEN
  ) |> tibble()
  
  plot_location_table <- plot_location_table |> 
    mutate(
      across(
        c(decimalLatitude:northing, utmZoneNumber:zOffset), 
        ~as.numeric(.x)
      ),
      time_zone = tz_lookup_coords(
        lat = decimalLatitude, lon = decimalLongitude, method = "accurate"
      )
    )
  
  # save plot locations
  saveRDS(object = plot_location_table, file = plot_loc_save_path)
} else {
  # load the table from disk instead
  plot_location_table = readRDS(file = plot_loc_save_path)
}

# ---- Get mammal trap locations ----

# path to save the mammal trap location at
trap_save_path <- file.path(out_dir, "small-mammal-trap-locations.rds")

if (force_build | !file.exists(trap_save_path)) {
  # table of the unique traps
  trap_locations <- trap_dat |> 
    select(namedLocation, siteID, plotID, trapCoordinate) |> 
    distinct()
  
  # get the locations for the traps
  trap_locations <- geoNEON::getLocTOS(
    trap_locations, "mam_pertrapnight", token = NEON_TOKEN
  ) |> tibble()
  
  # add time zones
  trap_locations <- trap_locations |> 
    mutate(
      across(
        c(adjNorthing:adjEasting, adjDecimalLatitude:adjElevation), 
        ~as.numeric(.x)
      ),
      time_zone = tz_lookup_coords(
        lat = adjDecimalLatitude, lon = adjDecimalLongitude, method = "accurate"
      )
    )
  
  # replace "points" in the column names
  names(trap_locations) <- gsub("^points&", "trapID", names(trap_locations))
    
  # save trap locations
  saveRDS(object = trap_locations, file = trap_save_path)
} else {
  # load trap locations instead
  trap_locations = readRDS(trap_save_path)
}

# check tz
site_location_table |> group_by(siteID, time_zone) |> tally()
plot_location_table |> group_by(siteID, time_zone) |> tally()
trap_locations |> group_by(siteID, time_zone) |> tally() |> 
  filter(!is.na(time_zone))