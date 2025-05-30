# ---- Setup ----

# load packages
library(dplyr)

# load the functions
source("code/functions/estimate-capture-time.R")

# ---- Load Files ----

# metadata about the ibuttons
fob_info <- readRDS("data/ibutton_pairs_2024.rds")

# get the NEON trap data that has been joined with the fobs

joined_traps <- readRDS("data/activity-timing-captures_2024.rds")

# ---- Get ibutton data 

# something wrong with these data...
ibutton_data <- readRDS("data/raw_iButton_data_2024_combined.rds") 

# tmp <- joined_traps[100, ]
# 
# tmp <- tmp |> 
#   select(
#     nightuid, siteID, trapCoordinate, pairID, in_fob, out_fob, taxonID, 
#     scientificName, collectDate, endDate
#   )
# 
# temp_data |> filter(collectDate %in% tmp$collectDate, site %in% tmp$siteID)










# ---- OLDER ----
# # create a unique linking variable
# trap_sub <- trap_dat |>
#   mutate(link = paste0(nightuid, trapCoordinate)) |>
#   filter(link %in% fob_traps$link)
# 
# # join the fob info to the traps
# trappings <- left_join(
#   fob_traps, trap_sub,
#   by = c("link", "nightuid", "tagID", "trapCoordinate", "endDate"),
#   relationship = "many-to-many"
# ) |>
#   # mutate(collectDate = ymd(collectDate, tz = "GMT")) |>
#   filter(!is.na(trapStatus)) |>
#   distinct()
# 
# # rm(neon_trappings)
# 
# # read temperature data for the site
# fob_temps <- read.csv(
#   "data/raw-iButton-data/BLAN_04_01_000s_dowloaded_04_19.csv",
#   check.names = FALSE
# ) |>
#   rename(DateTime = "Time")
# 
# # convert the temperature data into the "long" format, for usability
# long_data <- fob_temps |>
#   pivot_longer(-DateTime, names_to = "SerialNumber", values_to = "temp") |>
#   # fix the Serial Number and pull in some of the meta data
#   mutate(
#     corrected_SN = factor(gsub("\\*", "", SerialNumber)),
#     matched_row = match(corrected_SN, fob_info$SerialNumber),
#     pairID = fob_info$pair[matched_row],
#     name = fob_info$name[matched_row],
#     newpair = fob_info$new_pair[matched_row],
#     newpair = ifelse(grepl(".{1,}", newpair), newpair, NA)
#   ) |>
#   select(-matched_row) |>
#   relocate(
#     c("corrected_SN", "pairID", "name", "newpair"),
#     .before = "temp"
#   ) |>
#   mutate(DateTime = lubridate::as_datetime(DateTime)) |> 
#   arrange(DateTime, pairID)
# 
# # ---- Estimate capture times ----
# 
# # table of all instances where the buttons differ by 1C for 10 (3 min) cycles
# cap_ests <- estimate_capture_data(
#   temp = long_data$temp, time = long_data$DateTime,
#   IID = long_data$corrected_SN, PID = long_data$pairID,
# )
# 
# 
# 
# # for each pair of buttons...
# for (p in unique(cap_ests$PID)[1]) {
#   # get the trap data for this pair
#   trap_p <- trappings |> filter(iButtonLabel == p)
# 
#   # get capture estimates for this pair, during dates when traps occurred
#   caps_p <- cap_ests |>
#     mutate(
#       date = ymd(as.Date(start), tz = "GMT"),
#       datetime = as_datetime(start, tz = "GMT"),
#       time = hms::as_hms(datetime)
#     ) |>
#     filter(PID == p, date %in% trap_p$collectDate)
# 
#   # get temperature data for this pair, during dates when traps occurred
#   data_p <- long_data |>
#     mutate(
#       DateTime = as_datetime(DateTime, tz = "GMT"),
#       time = hms::as_hms(DateTime),
#       date = ymd(as.Date(DateTime), tz = "GMT")
#     ) |>
#     filter(pairID == p, date %in% trap_p$collectDate)
# 
#   # get the number of actual captures per trap
#   catch_count <- trap_p |>
#     group_by(collectDate) |>
#     summarize(n_caps = n(), .groups = "drop") |> 
#     arrange(collectDate) |> 
#     mutate(trap_group = NA)
# 
#   # make sure consecutive days are treated as a single trapping instance
#   consec_days = c(FALSE, diff(catch_count$collectDate) == 1)
#   runs = data.frame(unclass(rle(consec_days)))
#   consec_runs = which(runs$values)
#   
#   # assign groups to traps if there were consecutive trap events
#   date_group = 1
#   start = 1
#   for (r in seq_len(nrow(runs))) {
#     val = runs$values[r]
#     len = runs$lengths[r]
#     if (val) { 
#       # all obs covered by the run, plus the previous obs
#       inx = seq(start - 1, length.out = (len + 1))
#       catch_count$trap_group[inx] <- date_group
#       # update the group ID
#       date_group = date_group + 1
#     } else {
#       # each member of this run get their own group
#       inx = seq(start, length = len)
#       new_groups <- date_group + seq(0, length.out = len)
#       catch_count$trap_group[inx] <- new_groups
#       # update the group ID
#       date_group = max(new_groups) + 1
#     }
#     # update the start position
#     start = max(inx) + 1
#   }
#     
#   warns <- {}
#   for (g in unique(catch_count$trap_group)) {
#     count_g <- catch_count |> filter(trap_group == g)
#     caps_g <- caps_p |> filter(date %in% count_g$collectDate)
#     count = sum(count_g$n_caps)
#     est_count = nrow(caps_g)
#     
#     if (est_count == 0) {
#       # no capture time detected
#       cap_times = NA
#     } else if (est_count == count) {
#       # use all estimated times as capture times
#       cap_times = caps_g$datetime
#     } else if (est_count < count){
#       # use all capture times, and assume some entered in close succession
#       cap_times = caps_g$datetime
#       # warn the user
#       msg <- paste(
#         "Group", g, "of pair", p,
#         "had fewer estimated capture times than actual captures."
#       )
#       warning(msg)
#     } else {
#       # likely caused by animal movement - the trickiest case
#       msg = paste("Group", g, "of pair", p,
#                   "had more estimated capture times than actual captures.")
#       if (count == 1) {
#         # use the first estimated capture for this trap night
#         cap_times = caps_g$datetime[1]
#         msg = paste(msg, "The first estimated capture time was used.")
#       } else {
#         # TODO: Something else is needed here
#         cap_times = caps_g
#       }
#     }
#     
#   }
#   
#   # narrow down the estimated capture
#   for (c in seq_len(nrow(catch_count))){
#     # get the date and count
#     date_c = catch_count$collectDate[c]
#     count_c = catch_count$n_caps[c]
#     
#     # get the estimated captures for this date
#     caps_pc = caps_p |> filter(date == date_c)
#     est_count = nrow(caps_pc)
#     
#     if (est_count == count_c) { 
#       # use all estimated capture times
#       cap_times = caps_pc$datetime
#     } else if (est_count > count_c){
#       
#     }
#     
#     
#   }
# 
#   # plot the data
#   ggplot(data_p, aes(
#     x = DateTime, y = temp, col = corrected_SN,
#     group = corrected_SN
#   )) +
#     facet_wrap(~date) +
#     geom_vline(data = caps_p, aes(xintercept = datetime), linetype = "dashed") +
#     geom_line(linewidth = 0.75) +
#     scale_x_datetime(date_labels = "%H:%M") +
#     theme_classic() +
#     theme(legend.position = "none", strip.background = element_blank())
# }
# 
# ## TODO
# ## 1. establish how many individuals were trapped to compare with capture times
# ## a) if only 1 was caught - use the first estimated capture time
# ## b) if multiple individuals were caught, AND the number of estimated captures
# ## matches that number, use all the estimates
# ## c) if there are fewer estimates than trappings, ??? NA??
