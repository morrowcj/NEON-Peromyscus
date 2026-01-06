library(tidyverse)

# read the peromyscus data - for capture times
Peros <- readRDS("infection-modeling/data/peromyscus-model-data.rds")

# get a table with just the capture dates and times
captime_tab <- Peros %>%
  filter(!is.na(capture_time)) %>%
  select(siteID, date, capture_time) %>%
  mutate(capture_30 = round_date(capture_time, "30 mins"))


# file path to the combined 30-min precip data
out_path = "neon-data/data/eight_sites/30-min-bulkprecip.rds"

# save or load the precip data
if (!file.exists(out_path)) {

  # path to precip folder
  precip_dir = "neon-data/data/eight_sites/precipitation/"
  # paths to individual precip files
  precip_files <- list.files(file.path(precip_dir), full.names = TRUE)

  # loop through files and combine all the 30-minute weather tables
  precip_tab <- lapply(precip_files, function(f){
    rds = readRDS(f)
    data = rds$THRPRE_30min
  }) %>% bind_rows()

  # save the file
  saveRDS(precip_tab, out_path)
} else {
  # or load it
  precip_tab <- readRDS(out_path)
}

# reduce the number of rows before doing calculations

current_precip <- precip_tab %>%
  mutate(date = date(endDateTime)) %>%
  inner_join(
    captime_tab %>% select(siteID, date), by = c("siteID", "date"),
    relationship = "many-to-many"
  )

prev_precip <- precip_tab %>%
  mutate(date = date(endDateTime)) %>%
  inner_join(
    captime_tab %>% mutate(date = date - days(1)) %>% select(siteID, date),
    by = c("siteID", "date"),
    relationship = "many-to-many"
  )

precip_tab <- bind_rows(current_precip, prev_precip) %>%
  mutate(
    endDateTime = if_else(
      is.na(ymd_hms(endDateTime)), date(endDateTime), ymd_hms(endDateTime)
    )
  )


#' Add up the day's precip (up to a specific time)
#'
#' @param site a site
#' @param dt a date time
#'
#' @returns sum of precips from the start of the day (if captured after noon)
#' or the previous day (if captured before noon) through the capture time
calc_cap_precip <- function(site, dt) {
  the_date = date(dt)
  caught_before_noon = dt <= (the_date + hours(12))
  start_dt = if_else(caught_before_noon, the_date - days(1), the_date)
  precip_tab %>%
    filter(
      siteID == site, # at this site
      date == the_date, # from the start of the day (or previous day)
      endDateTime <= dt # until captured
    ) %>%
    pull(TFPrecipBulk) %>% sum()
}

#' Add up the day's precip
#'
#' @param site a site
#' @param dt a date time
#'
#' @returns sum of precips from the start of the day (if captured after noon)
#' or the previous day (if captured before noon)
calc_day_precip <- function(site, dt) {
  the_date = date(dt)
  caught_before_noon = dt <= (the_date + hours(12))
  start_dt = if_else(caught_before_noon, the_date - days(1), the_date)
  precip_tab %>%
    filter(
      siteID == site, # at this site
      date == start_dt # from the start of the day (or previous day)
    ) %>%
    pull(TFPrecipBulk) %>% sum()
}

# add the estimates to the capture time table
captime_tab <- captime_tab %>%
  rowwise() %>%
  mutate(
    capture_precip = calc_cap_precip(siteID, capture_30),
    day_precip = calc_day_precip(siteID, capture_30)
  )

# how correlated are these metrics?
captime_tab %>%
  select(capture_precip, day_precip) %>%
  filter(!is.na(capture_precip), !is.na(day_precip)) %>%
  cor()

# These values do not make any sense...
ggplot(captime_tab, aes(x = day_precip, y = capture_precip, col = hour(capture_30))) +
  geom_point() +
  # geom_smooth(method = "lm") +
  # geom_smooth(method = "lm", aes(group = 1), col = "black") +
  annotate(x = 5000, y = 9000, geom = "text", label = "r = 0.86") +
  labs(x = "Daily precip (mm)", y = "Daily precip until capture (mm)",
       col = "capture hour") +
  theme_bw() +
  scale_color_gradient(low = "blue", high = "orange", breaks = c(0, 6, 12, 18, 24))
