library(tidyverse)

# mammal trap data, to line up
mammals <- readRDS(
  file.path("neon-data/data/eight_sites/mammal-trap-data.rds")
)$mam_pertrapnight %>% 
  tibble() %>% 
  mutate(
    date = date(collectDate), year = year(collectDate),
    month = month(collectDate), DOY = yday(collectDate)
  )

temperature_dir = "neon-data/data/eight_sites/biological-temperatures/"

airtemp_dir = "neon-data/data/eight_sites/air-temperatures/"

temperature_files <- list.files(file.path(temperature_dir), full.names = TRUE)
airtemp_files <- list.files(file.path(airtemp_dir), full.names = TRUE)

this_file <- temperature_files[1]
this_airtemp <- airtemp_files[1]
# load the product for this site
prod <- readRDS(this_file)
airprod <- readRDS(this_airtemp)

# list the different tables that are in the product
names(prod) 

# look at the readme
prod$readme_00005

# get the temperature data
df <- prod$IRBT_30_minute %>% 
  tibble() %>% 
  mutate(
    HOR.VER = paste(horizontalPosition, verticalPosition, sep = "."),
    date = date(startDateTime), year = year(startDateTime), 
    month = month(startDateTime), DOY = yday(startDateTime)
  ) |> 
  left_join(prod$sensor_positions_00005 |> select(-publicationDate))

daily_temps <- df |> 
  group_by(
    siteID, referenceLocationID, sensorLocationID, HOR.VER, year, month, DOY, 
    date
  ) |> 
  summarize(
    daily_min = min(bioTempMinimum, na.rm = TRUE),
    daily_mean = mean(bioTempMean, na.rm = TRUE),
    daily_sd = sd(bioTempMean, na.rm = TRUE),
    daily_max = max(bioTempMinimum, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(
    daily_min = if_else(is.finite(daily_min), daily_min, NA),
    daily_mean = if_else(is.finite(daily_mean), daily_mean, NA),
    daily_sd = if_else(is.finite(daily_sd), daily_sd, NA),
    daily_max = if_else(is.finite(daily_max), daily_max, NA),
  ) |> group_by(year, HOR.VER) |> 
  arrange(year, DOY) # |> 
  # filter(min(DOY) == 1) |> 
  # mutate(
  #   GDD = degday::dd_calc(
  #     daily_min = daily_min, daily_max = daily_max, nextday_min = NA,
  #     thresh_low = 32, thresh_up = 80, method = "sng_tri", cumulative = FALSE,
  #     interpolate_na = TRUE
  #   ),
  #   cGDD = cumsum(GDD),
  #   tick_GDD = degday::dd_calc(
  #     daily_min = daily_min, daily_max = daily_max, nextday_min = NA,
  #     thresh_low = 45, thresh_up = 80, method = "sng_tri", cumulative = FALSE,
  #     interpolate_na = TRUE
  #   ),
  #   tick_cGDD = cumsum(tick_GDD)
  # )

# daily_temps |> 
#   ggplot(aes(x = DOY, y = cGDD, col = year)) +
#   geom_line(aes(group = year)) +
#   facet_wrap(~ sensorLocationID)
# 
# 
# daily_temps |> 
#   ggplot(aes(x = DOY, y = daily_mean, col = year)) +
#   geom_line(aes(group = year)) +
#   facet_wrap(~ sensorLocationID)

## TODO: The temperatures that we're interested in are:
# 1) temperature on the day/night the individual was captured (mean, min, max)
# 2) temperature at the moment the individual was captured (closest 30 min)
# 3) overwinter temperature (for survival, demographics, etc.)
# 4) temperatures on the days/nights of tick drags

# # get all the trap days for each site
# unique_siteDays <- mammals %>% 
#   select(siteID, date:DOY) %>% 
#   distinct()
# 
# # get all the temperatures for days and locations where traps are
# trapnight_temperatures <- unique_siteDays %>% 
#   inner_join(df) %>% 
#   relocate(startDateTime, endDateTime, .after = DOY)
