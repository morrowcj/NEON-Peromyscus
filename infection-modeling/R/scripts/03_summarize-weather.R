library(tidyverse)

## ---- Temperature ----

# want to make sure all data can be found in the same folder
file.copy(
  from = "neon-data/data/eight_sites/daily-air-temperatures.rds",
  to = "infection-modeling/data/daily-air-temperatures.rds",
)

# Load temperature data
daily_temps <- readRDS("infection-modeling/data/daily-air-temperatures.rds")

## TODO work out degree day calculations
# daily_temps %>%
#   select(year, siteID, date, temp_day_min, temp_day_max, temp_day_mean) %>%
#   filter(year %in% 2017:2024) %>%
#   group_by(year, siteID) %>%
#   arrange(year, siteID, date) %>%
#   mutate(
#     dd_sng_tri = degday::dd_calc(
#       daily_min = mean(temp_day_min, na.rm = TRUE),
#       daily_max = mean(temp_day_max, na.rm = TRUE),
#       thresh_low = 10, thresh_up = 23, cumulative = FALSE,
#       interpolate_na = TRUE
#     )
#   )

# calculate yearly and monthly summaries of daily temperature data
temp_summaries <- daily_temps %>% 
  filter(year %in% 2017:2024) %>% 
  ## month level ...
  group_by(siteID, year, month) %>% 
  mutate(
    ### daily
    month_min_daily_temp = min(temp_day_mean, na.rm = TRUE),
    month_avg_daily_temp = mean(temp_day_mean, na.rm = TRUE),
    month_max_daily_temp = max(temp_day_mean, na.rm = TRUE),
    month_sd_daily_temp = sd(temp_day_mean, na.rm = TRUE),
  ) %>% 
  ## year level ...
  group_by(siteID, year) %>% 
  mutate(
    ### daily
    year_avg_daily_temp = mean(temp_day_mean, na.rm = TRUE),
    year_min_daily_temp = min(temp_day_mean, na.rm = TRUE),
    year_max_daily_temp = max(temp_day_mean, na.rm = TRUE),
    year_sd_daily_temp = sd(temp_day_mean, na.rm = TRUE),
    ### monthly
    year_min_monthly_temp = min(month_avg_daily_temp, na.rm = TRUE),
    year_avg_monthly_temp = mean(month_avg_daily_temp, na.rm = TRUE),
    year_max_monthly_temp = max(month_avg_daily_temp, na.rm = TRUE),
    year_sd_monthly_temp = sd(month_avg_daily_temp, na.rm = TRUE),
  ) %>%
  ## site level ...
  group_by(siteID) %>% 
  mutate(
    ## daily
    site_min_daily_temp = min(temp_day_mean, na.rm = TRUE),
    site_avg_daily_temp = mean(temp_day_mean, na.rm = TRUE),
    site_max_daily_temp = max(temp_day_mean, na.rm = TRUE),
    site_sd_daily_temp = sd(temp_day_mean, na.rm = TRUE),
    ## monthly
    site_min_monthly_temp = min(month_avg_daily_temp, na.rm = TRUE),
    site_avg_monthly_temp = mean(month_avg_daily_temp, na.rm = TRUE),
    site_max_monthly_temp = max(month_avg_daily_temp, na.rm = TRUE),
    site_sd_monthly_temp = sd(month_avg_daily_temp, na.rm = TRUE),
    site_CV_monthly_temp = site_sd_monthly_temp / site_avg_monthly_temp,
    ## yearly
    site_min_yearly_temp = min(year_avg_daily_temp, na.rm = TRUE),
    site_avg_yearly_temp = mean(year_avg_daily_temp, na.rm = TRUE),
    site_max_yearly_temp = max(year_avg_daily_temp, na.rm = TRUE),
    site_sd_yearly_temp = sd(year_avg_daily_temp, na.rm = TRUE),
  ) %>% 
  ungroup() %>% 
  select(
    siteID, year, month, 
    starts_with("site_"), starts_with("year_"), starts_with("month_")
  ) %>% 
  mutate(
    across(
      -c(siteID, year, month), 
      ~if_else(is.finite(.x), .x, NA)
    )
  ) %>% 
  distinct()

# save the summary
saveRDS(temp_summaries, "infection-modeling/data/temperature-summaries.rds")

## ---- Precipitations ----

# copy the file to the local folder
file.copy(
  from = "neon-data/data/eight_sites/all-daily-precips.rds",
  to = "infection-modeling/data/all-daily-precips.rds",
)

# Load precipitation data
daily_precips <- readRDS("infection-modeling/data/all-daily-precips.rds")

# calculate yearly and monthly summaries of precip data.
## Note that this averages across the different precip colleciton methods
precip_summaries <- daily_precips %>% 
  filter(year %in% 2017:2024) %>% 
  group_by(siteID, date, year, month) %>% 
  group_by(siteID, year, month) %>% 
  mutate(
    # total
    month_total_precip = sum(daily_precip, na.rm = TRUE),
    # daily
    month_avg_daily_precip = mean(daily_precip, na.rm = TRUE),
    month_sd_daily_precip = sd(daily_precip, na.rm = TRUE),
    month_pct_daily_raindays = mean(daily_precip > 0, na.rm = TRUE),
    month_sd_daily_raindays = sd(daily_precip > 0, na.rm = TRUE)
  ) %>% 
  group_by(siteID, year) %>% 
  mutate(
    # total
    year_total_precip = sum(daily_precip, na.rm = TRUE),
    # daily
    year_avg_daily_precip = mean(daily_precip, na.rm = TRUE),
    year_sd_daily_precip = sd(daily_precip, na.rm = TRUE),
    year_pct_daily_raindays = mean(daily_precip > 0, na.rm = TRUE),
    year_sd_daily_raindays = sd(daily_precip > 0, na.rm = TRUE),
    # monthly
    year_avg_monthly_precip = mean(month_avg_daily_precip, na.rm = TRUE),
    year_sd_monthly_precip = sd(month_avg_daily_precip, na.rm = TRUE),
    year_pct_monthly_raindays = mean(month_pct_daily_raindays, na.rm = TRUE),
    year_sd_monthly_raindays = sd(month_pct_daily_raindays, na.rm = TRUE),
  ) %>% 
  group_by(siteID) %>% 
  mutate(
    # total
    site_total_precip = sum(daily_precip, na.rm = TRUE),
    # daily
    site_avg_daily_precip = mean(daily_precip, na.rm = TRUE),
    site_sd_daily_precip = sd(daily_precip, na.rm = TRUE),
    site_pct_daily_raindays = mean(daily_precip > 0, na.rm = TRUE),
    site_sd_daily_raindays = sd(daily_precip > 0, na.rm = TRUE),
    # monthly
    site_avg_monthly_precip = mean(month_avg_daily_precip, na.rm = TRUE),
    site_sd_monthly_precip = sd(month_avg_daily_precip, na.rm = TRUE),
    site_CV_monthly_precip = site_sd_monthly_precip / site_avg_monthly_precip,
    site_pct_monthly_raindays = mean(month_pct_daily_raindays, na.rm = TRUE),
    site_sd_monthly_raindays = sd(month_pct_daily_raindays, na.rm = TRUE),
    site_CV_monthly_raindays = site_sd_monthly_raindays / site_pct_monthly_raindays,
    # yearly
    site_avg_yearly_precip = mean(year_avg_daily_precip, na.rm = TRUE),
    site_sd_yearly_precip = sd(year_avg_daily_precip, na.rm = TRUE),
    site_pct_yearly_raindays = mean(year_pct_daily_raindays, na.rm = TRUE),
    site_sd_yearly_raindays = sd(year_pct_daily_raindays, na.rm = TRUE),
  ) %>% 
  ungroup() %>% 
  select(
    siteID, year, month, 
    starts_with("site_"), starts_with("year_"), starts_with("month_")
  ) %>% 
  mutate(
    across(
      -c(siteID, year, month), 
      ~if_else(is.finite(.x), .x, NA)
    )
  ) %>%
  distinct()

# save the summary
saveRDS(
  precip_summaries, "infection-modeling/data/precipitation-summaries.rds"
)

## ---- Humidity ----

# copy the file to the local folder
file.copy(
  from = "neon-data/data/eight_sites/daily-relative-humidities.rds",
  to = "infection-modeling/data/daily-relative-humidities.rds",
)

# Relative humidity
daily_humids <- readRDS(
  "infection-modeling/data/daily-relative-humidities.rds"
)

humid_summaries <- daily_humids %>% 
  filter(year %in% 2017:2024) %>% 
  group_by(siteID, year, month) %>% 
  mutate(
    # daily
    month_min_daily_RH = min(RH_day_mean, na.rm = TRUE),
    month_avg_daily_RH = mean(RH_day_mean, na.rm = TRUE),
    month_max_daily_RH = max(RH_day_mean, na.rm = TRUE),
    month_sd_daily_RH = sd(RH_day_mean, na.rm = TRUE),  
  ) %>% 
  group_by(siteID, year) %>% 
  mutate(
    # daily
    year_min_daily_RH = min(RH_day_mean, na.rm = TRUE),
    year_avg_daily_RH = mean(RH_day_mean, na.rm = TRUE),
    year_max_daily_RH = max(RH_day_mean, na.rm = TRUE),
    year_sd_daily_RH = sd(RH_day_mean, na.rm = TRUE),  
    # monthly
    year_min_monthly_RH = min(month_avg_daily_RH, na.rm = TRUE),
    year_avg_monthly_RH = mean(month_avg_daily_RH, na.rm = TRUE),
    year_max_monthly_RH = max(month_avg_daily_RH, na.rm = TRUE),
    year_sd_monthly_RH = sd(month_avg_daily_RH, na.rm = TRUE),  
  ) %>% 
  group_by(siteID) %>% 
  mutate(
    # daily
    site_min_daily_RH = min(RH_day_mean, na.rm = TRUE),
    site_avg_daily_RH = mean(RH_day_mean, na.rm = TRUE),
    site_max_daily_RH = max(RH_day_mean, na.rm = TRUE),
    site_sd_daily_RH = sd(RH_day_mean, na.rm = TRUE),  
    # monthly
    site_min_monthly_RH = min(month_avg_daily_RH, na.rm = TRUE),
    site_avg_monthly_RH = mean(month_avg_daily_RH, na.rm = TRUE),
    site_max_monthly_RH = max(month_avg_daily_RH, na.rm = TRUE),
    site_sd_monthly_RH = sd(month_avg_daily_RH, na.rm = TRUE),
    site_CV_monthly_RH = site_sd_monthly_RH / site_avg_monthly_RH,
    # yearly
    site_min_yearly_RH = min(year_avg_daily_RH, na.rm = TRUE),
    site_avg_yearly_RH = mean(year_avg_daily_RH, na.rm = TRUE),
    site_max_yearly_RH = max(year_avg_daily_RH, na.rm = TRUE),
    site_sd_yearly_RH = sd(year_avg_daily_RH, na.rm = TRUE),
  ) %>% 
  ungroup() %>% 
  select(
    siteID, year, month, 
    starts_with("site_"), starts_with("year_"), starts_with("month_")
  ) %>% 
  mutate(
    across(
      -c(siteID, year, month), 
      ~if_else(is.finite(.x), .x, NA)
    )
  ) %>%
  distinct()

# save the summary
saveRDS(
  humid_summaries, "infection-modeling/data/humidity-summaries.rds"
)

## ---- All weather variables ----

# simplify to important temperature data
daily_temps <- daily_temps %>% 
  filter(year %in% 2017:2024) %>% 
  group_by(siteID, date) %>% 
  mutate(
    temp_min = mean(temp_day_min, na.rm = TRUE),
    temp_mean = mean(temp_day_mean, na.rm = TRUE),
    temp_max = mean(temp_day_max, na.rm = TRUE), 
  ) %>% 
  ungroup() %>% 
  select(siteID, date, temp_min:temp_max) %>% 
  distinct() %>% 
  mutate(across(temp_min:temp_max, ~if_else(is.finite(.x), .x, NA))) %>% 
  distinct()

# # calculate degree days -- not working
# daily_temps %>% 
#   mutate(year = year(date)) %>% 
#   group_by(siteID) %>% 
#   arrange(siteID, year, date) %>% 
#   mutate(
#     dd = degday::dd_calc(
#       daily_min = temp_min, daily_max = temp_max, cumulative = TRUE, 
#       interpolate_na = TRUE, thresh_low = 0, thresh_up = 30   
#     )
#   )


# simplify important precipitation data
daily_precips <- daily_precips %>% 
  filter(year %in% 2017:2024) %>% 
  group_by(siteID, date) %>% 
  mutate(
    precip = mean(daily_precip, na.rm = TRUE), 
  ) %>% 
  ungroup() %>% 
  select(siteID, date, precip, year) %>% 
  distinct() %>% 
  group_by(siteID, year) %>% 
  arrange(siteID, year, date) %>% 
  mutate(
    rained_today = as.numeric(precip > 0),
    rained_today = replace_na(rained_today, 0),
    precip_num = replace_na(precip, 0),
    cumu_year_precip = cumsum(precip_num),
    cumu_year_raindays = cumsum(rained_today),
  ) %>% 
  ungroup() %>% 
  select(-c(rained_today, year, precip_num)) %>% 
  mutate(across(precip:cumu_year_raindays, ~if_else(is.finite(.x), .x, NA))) %>% 
  distinct()

# simplify important humidity data
daily_humids <- daily_humids %>% 
  filter(year %in% 2017:2024) %>% 
  group_by(siteID, date) %>% 
  mutate(
    RH = mean(RH_day_mean, na.rm = TRUE),
    RH_temp = mean(tempRH_day_mean, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  select(siteID, date, RH, RH_temp) %>% 
  distinct() %>% 
  mutate(across(c(RH, RH_temp), ~if_else(is.finite(.x), .x, NA)))

# combine the daily weather summaries
daily_weather <- daily_temps %>% 
  full_join(daily_precips, by = c("siteID", "date")) %>% 
  full_join(daily_humids, by = c("siteID", "date"))

# save the daily weather summaries
saveRDS(daily_weather, "infection-modeling/data/daily-weather-vars.rds")

# # combine all daily weather -- Not useful
# daily_weather <- daily_temps %>% 
#   full_join(daily_precips) %>% 
#   full_join(daily_humids)
# 
# # save daily weather data
# saveRDS(daily_weather, "infection-modeling/data/all-daily-weather.rds")

# combine all weather summaries
weather_summaries <- temp_summaries %>% 
  full_join(precip_summaries, by = c("siteID", "year", "month")) %>% 
  full_join(humid_summaries, by = c("siteID", "year", "month"))

# save weather summaries
saveRDS(weather_summaries, "infection-modeling/data/all-weather-summaries.rds")

## ---- Climate PCA ----

# data for site-level climate PCA
cpca_dat <- weather_summaries %>% 
  select(siteID,
    matches("^site_(CV|avg|pct)_monthly_(temp|precip|RH|raindays)"),
    -site_CV_monthly_raindays , -site_CV_monthly_RH
    # matches("^site_(sd|avg|pct)_monthly_(temp|precip|RH|raindays)"),
    # -site_sd_monthly_raindays , -site_sd_monthly_RH
  ) %>%
  filter(complete.cases(.)) %>% 
  distinct()

## TODO: alter variables in the above snippet to tweak the PCs
## Currently, they include variability metrics (CV)

# conduct climate PCA
cpca <- cpca_dat %>% select(-siteID) %>% 
  vegan::pca(scale = TRUE)

# # summarize
# summary(cpca)

# extract loadings
cpca_scores <- scores(cpca)

# species scores
cpca_sp <- cpca_scores$species %>%  
  data.frame() %>% 
  rownames_to_column("variable") %>% 
  tibble()

# site scores
cpca_st <- bind_cols(
  cpca_dat,
  cpca_scores$sites %>% data.frame()
) %>% tibble()

# build a list of PCA objects to save
cpca_list <- list(
  site_scores = cpca_st %>% 
    select(siteID, clim_PC1 = PC1, clim_PC2 = PC2),
  species_scores = cpca_sp %>% 
    select(variable, clim_PC1 = PC1, clim_PC2 = PC2),
  pca_obj = cpca,
  data = cpca_dat
)

# # rough plot of the PCA
# ordiplot(cpca) %>% 
#   orditorp(display = "species")

# save the list
saveRDS(cpca_list, "infection-modeling/data/climate-PCA.rds")

## ---- Weather PCA ----

# read mammal data, to help filter
mammals <- readRDS("infection-modeling/data/mammal-data.rds")

# weather data for the PCA
wpca_dat <- daily_weather %>% 
  select(-c(RH_temp, cumu_year_raindays)) %>% 
  semi_join(mammals, by = c("siteID", "date")) %>% 
  filter(complete.cases(.))

# fit weather PCA
wpca <- wpca_dat %>% 
  select(-c(siteID, date)) %>% 
  vegan::pca(scale = TRUE)

# # summarize the PCA
# summary(wpca)

# extract scores
wpca_scores <- scores(wpca)

# species scores
wpca_sp <- wpca_scores$species %>% data.frame() %>% 
  rownames_to_column("variable") %>% 
  tibble()

# site scores
wpca_st <- bind_cols(
  wpca_dat, 
  wpca_scores$sites %>% data.frame()
)

# ordiplot(wpca) %>% 
#   orditorp(display = "species")

wpca_list <- list(
  site_scores = wpca_st %>% 
    select(siteID, date, wthr_PC1 = PC1, wthr_PC2 = PC2),
  species_scores = wpca_sp %>% 
    select(variable, wthr_PC1 = PC1, wthr_PC2 = PC2),
  pca_obj = wpca,
  data = wpca_dat
)

# save the list
saveRDS(wpca_list, "infection-modeling/data/weather-PCA.rds")
