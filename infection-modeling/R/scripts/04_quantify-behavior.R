## TODO: Should probably move the capture time section from 
## the 01_prepare-mammal-data.R file into this one

library(tidyverse)
library(lubridate)

warning("Something about this (behavior) script is messed up.")

mammals <- readRDS("infection-modeling/data/mammal-data.rds")

plot_nights <- readRDS("infection-modeling/data/plotnight-metadata.rds")

plot_nights <- plot_nights %>% mutate(date = date(collectDate))

# plot_locations <- readRDS(
#   "neon-data/data/eight_sites/location_data/plot-centroid-locations.rds"
# )

## ---- Capture Time ----

# load the capture time data
cap_times <- readRDS(
  "activity-timing/data/ibuttons/capture-time-trapping-data_2022-2024.rds"
)

# replace all the ID columns with the uid
cap_times <- mammals %>%
  select(uid, year, siteID, plotID, collectDate, trapCoordinate, tagID) %>%
  right_join(
    cap_times,
    by = c("year", "siteID", "plotID", "collectDate", "trapCoordinate", "tagID")
  ) %>%
  select(
    uid, time_zone, ibutton_pair, local_midnight, sunset, sunrise, 
    trapping_interval, capture_time, night_length, cap_after_sunset, 
    cap_before_sunrise, cap_prop_night
  ) %>% 
  relocate(night_length, .after = trapping_interval) %>% 
  # select(-c(year, siteID, plotID, collectDate, trapCoordinate, tagID)) %>%
  distinct()

# save the results
saveRDS(cap_times, "infection-modeling/data/capture-times.rds")

## ---- Individual ID (iid) assessment ----

## TODO: remove this section, at let it be handled by "prepare-Peromyscus-data.R"

# # group by site x tagID
# site_tag_tab <- mammals %>% 
#   filter(!is.na(tagID)) %>% 
#   select(
#     siteID, plotID, tagID, year, eventID, date, recap = recapture, taxonID, sex_male, 
#     sex_mature, weight
#   ) %>% 
#   mutate(
#     tagID = gsub(pattern = "NEON\\.MAM\\.", replacement = "", x = tagID),
#     tmp_iid = as.numeric(factor(paste(siteID, tagID)))
#   ) %>% 
#   group_by(siteID, tagID) %>% 
#   mutate(
#     cap_num = row_number(),
#     tot_caps = max(cap_num, na.rm = TRUE),
#     prev_cap_days = time_length(date - lag(date), unit = "days"),
#     male_prop = mean(sex_male, na.rm = TRUE) %>% round(2),
#     mature_prop = mean(sex_mature, na.rm = TRUE) %>% round(2),
#     PELE_prop = mean(taxonID == "PELE", na.rm = TRUE) %>% round(2),
#     weight_sd = sd(weight, na.rm = TRUE) %>% round(2)
#   ) %>% 
#   relocate(tmp_iid, cap_num, tot_caps, .before = 0) %>% 
#   relocate(prev_cap_days, .after = date) %>% 
#   arrange(desc(tot_caps), siteID, tagID, date)
# 
# top_sitetags <- unique(site_tag_tab$tmp_iid)[1:10]
# 
# site_tag_tab %>% 
#   filter(tot_caps > 10) %>% 
#   pull(tmp_iid) %>% 
#   unique() %>% 
#   length()
# 
# site_tag_subs <- site_tag_tab %>% 
#   filter(tmp_iid %in% top_sitetags) 
# 
# site_tag_subs %>% 
#   write.csv("infection-modeling/data/site-tagID_top-ten-most-captured.csv")
# 
# # group by plotID x tagID
# plot_tag_tab <- mammals %>% 
#   filter(!is.na(tagID)) %>% 
#   select(
#     siteID, plotID, tagID, year, eventID, date, ecap = recapture, taxonID, sex_male, 
#     sex_mature, weight
#   ) %>% 
#   mutate(
#     tagID = gsub(pattern = "NEON\\.MAM\\.", replacement = "", x = tagID),
#     tmp_iid = as.numeric(factor(paste(plotID, tagID)))
#   ) %>% 
#   group_by(plotID, tagID) %>% 
#   mutate(
#     cap_num = row_number(),
#     tot_caps = max(cap_num, na.rm = TRUE),
#     prev_cap_days = time_length(date - lag(date), unit = "days"),
#     male_prop = mean(sex_male, na.rm = TRUE) %>% round(2),
#     mature_prop = mean(sex_mature, na.rm = TRUE) %>% round(2),
#     PELE_prop = mean(taxonID == "PELE", na.rm = TRUE) %>% round(2),
#     weight_sd = sd(weight, na.rm = TRUE) %>% round(2)
#   ) %>% 
#   relocate(tmp_iid, cap_num, tot_caps, .before = 0) %>% 
#   relocate(prev_cap_days, .after = date) %>% 
#   arrange(desc(tot_caps), plotID, tagID, date)
# 
# top_plottags <- unique(plot_tag_tab$tmp_iid)[1:10]
# 
# plot_tag_subs <- plot_tag_tab %>% 
#   filter(tmp_iid %in% top_plottags) 
# 
# plot_tag_tab %>% 
#   filter(tot_caps > 10) %>% 
#   pull(tmp_iid) %>% 
#   unique() %>% 
#   length()
# 
# plot_tag_subs %>% 
#   write.csv("infection-modeling/data/plot-tagID_top-ten-most-captured.csv")


# site_tag_subs %>% 
#   mutate(is_PELE = as.numeric(taxonID == "PELE")) %>% 
#   pivot_longer(
#     c(is_PELE, sex_male, sex_mature, weight), names_to = "variable"
#   ) %>% 
#   ggplot(
#     aes(x = cap_num, y = value, group = factor(tmp_iid), col = factor(tmp_iid))
#   ) + 
#   facet_wrap(~variable, scales = "free_y") + 
#   # geom_point() + 
#   geom_line() + 
#   theme_bw() + 
#   theme(strip.background = element_blank())

## ---- Trapability ----
## trapabillity = captures / total trap nights between first and last capture.

warning("This is where the issue is: group_by(iid) only")

# summarize captures to the individual level
recap_tab <- mammals %>% 
  filter(!is.na(tagID)) %>% 
  # group_by(year, plotID, tagID) %>% 
  # arrange(year, plotID, tagID, date) %>% 
  # mutate(iid = paste(plotID, tagID, sep = "_")) %>% 
  group_by(siteID, iid) %>%
  arrange(siteID, iid) %>%
  summarize(
    n_captures = n(), 
    first_cap = first(date), last_cap = last(date),
    days_between = lubridate::time_length(last_cap - first_cap, unit = "days"),
    first_event = first(eventID), last_event = last(eventID),
    first_plot = first(plotID), last_plot = last(plotID),
    avg_sex = mean(sex_male, na.rm = TRUE),
    avg_pele = mean(taxonID == "PELE", na.rm = TRUE),
    .groups = "drop"
  )

# calculate the number of trap nights between first and last captures
recap_tab <- recap_tab %>% 
  rowwise() %>% 
  mutate(
    elapsed_nights = plot_nights %>%
      filter(
        date >= .env$first_cap, date <= .env$last_cap,
        siteID == .env$siteID,
        # plotID == .data$plotID,
      ) %>% pull(date) %>% unique() %>% length()
  ) %>% 
  ungroup() %>% 
  mutate(
    trapability = n_captures / elapsed_nights,
    ncap_weight = n_captures / max(n_captures, na.rm = TRUE),
    weighted_trapability = trapability * ncap_weight
  )

## Check the observations with trapability > 1.0
checks <- recap_tab %>% 
  filter(trapability > 1) %>% arrange(iid, siteID)
checks

## This demonstrates that individuals are running into a new trap
mammals %>% 
  filter(iid %in% checks$iid) %>% 
  arrange(iid, date, plotID) %>% 
  select(iid, plotID, date, weight, sex, testes, nipples, vagina)

## Visualize the relationship between captures and weighted trapability
recap_tab %>% 
  select(n_captures, weighted_trapability) %>% 
  distinct() %>% 
  ggplot(aes(x = n_captures, y = weighted_trapability)) + 
  geom_point() + 
  geom_smooth(method = "lm")

## Visualize relationship between "raw" trapability vs the weighted version
recap_tab %>% 
  select(trapability, weighted_trapability, n_captures) %>% 
  distinct() %>% 
  ggplot(aes(x = trapability, y = weighted_trapability)) +
  geom_smooth(method = "lm") + 
  geom_point(aes(fill = n_captures, size = n_captures), shape = 21, alpha = 1) + 
  scale_fill_gradient(
    # midpoint = 10, mid = "grey80", 
    low = "forestgreen", high = "orange"
  ) + 
  theme_bw()

## ---- Recaptures in different traps ----
## average move distance = average distance (m) between consecutive captures
## trap diversity = total number of unique traps visited / total captures

# calculate metrics for consecutive trapping events for each indivivdual
move_record <- mammals %>% 
  # left_join(trap_locations, by = c("plotID", "trapCoordinate")) %>% 
  filter(!is.na(tagID)) %>% 
  # mutate(iid = paste(plotID, tagID, sep = "_")) %>%
  relocate(iid, .before = 0) %>%
  group_by(iid) %>%
  arrange(iid, date) %>%
  reframe(
    capday_i = date, capday_j = lag(date),
    trap_i = trapCoordinate, trap_j = lag(trapCoordinate),
    coords_i = cbind(trapLon, trapLat), 
    coords_j = cbind(lag(trapLon), lag(trapLat)),
    dist_ij = geosphere::distGeo(coords_i, coords_j),
    dist_ij = replace_na(dist_ij, 0)
  ) %>% distinct()

# summarize distance between captures, distance between the two furthest traps,
# and the unique traps an individual was caught in.
ind_trap_summaries <- move_record %>% 
  group_by(iid) %>% 
  summarize(
    avg_move_dist = mean(dist_ij, na.rm = TRUE),
    max_travel_dist = max(geosphere::distm(coords_i), na.rm = TRUE),
    unique_traps = length(unique(trap_i))
  ) %>% distinct()

behavior <- left_join(recap_tab, ind_trap_summaries, by = "iid") %>% 
  select(
    iid, n_captures, 
    known_alive_days = days_between, avg_sex, avg_pele, 
    residence_trapnights = elapsed_nights,
    trapability:unique_traps
  ) %>% 
  mutate(
    weighted_trap_diversity = unique_traps*ncap_weight
  ) %>% 
  distinct()

## Visualize the relationship between weighted trap diversity and captures
behavior %>% 
  ggplot(aes(x = n_captures, y = weighted_trap_diversity)) + 
  geom_point() + 
  geom_smooth(
    method = lm, formula = y ~ x, aes(col = "x"), linetype = "dashed"
  ) + 
  geom_smooth(
    method = lm, formula = y ~ poly(x, 2), 
    aes(col = "x^2")
  ) + 
  theme_bw() + 
  labs(color = NULL)

## Visualize the relationship between unique traps and weighted trap diversity
behavior %>% 
  ggplot(aes(x = unique_traps, y = weighted_trap_diversity)) +
  geom_point() + 
  geom_smooth(
    method = lm, formula = y ~ x, aes(col = "x"), linetype = "dashed"
  ) + 
  geom_smooth(
    method = lm, formula = y ~ poly(x, 2), 
    aes(col = "x^2")
  ) + 
  theme_bw() + 
  labs(color = NULL)

## ---- Save results ----

saveRDS(behavior, "infection-modeling/data/individual-behavior-data.rds")
