# ---- Script packages ----
library(lutz)
library(bioRad)
library(lemon)
library(lubridate)
library(tidyverse)

# custom functions for reading iButton data
source("activity-timing/code/functions/estimate-capture-time.R")

# ---- Script parameters ----

# directory to look for data in
ibutton_data_dir = file.path("activity-timing/data/ibuttons")

# path to the file containing NEON captures, with ibutton serial numbers
captures_path = file.path(
  ibutton_data_dir, "neon-captures-with-ibutton-pairs_2024.rds"
)

update_captures_path <- file.path(
  ibutton_data_dir, "neon-captures-with-estimated-cap-times_2024.rds"
)

# path to the de-duplicated temperature data
deduped_temps_path <- file.path(
  ibutton_data_dir, "cleaned-temperatures", 
  "deduped-temperature-data-list_2024.rds"
)

# path to the temperature file lookup table
temp_file_lookup_path <- file.path(
  ibutton_data_dir, "raw-temperatures",
  "r-objects",
  "bout-file-lookup-table_2024.rds"
)

# path to the output file
out_file_path <- "activity-timing/data/ibuttons/capture_time_estimates_2024.rds"

# path to duplicated columns list
dup_path <- file.path(
  ibutton_data_dir, "raw-temperatures", 
  "r-objects", "temperature-duplicate-columns-list.rds"
)

graphics_dir <- "activity-timing/graphics/trapping-bout_temperature-data"

if (!dir.exists(graphics_dir)) {
  dir.create(graphics_dir, recursive = TRUE)
}

# ---- Load data ----

file_lookup <- readRDS(temp_file_lookup_path)

# data (that contains ibutton serial numbers)
captures <- readRDS(captures_path)

# the temperature data
temp_streams_list <- readRDS(deduped_temps_path)

# read in the duplicated ibutton list
dup_pairs <- readRDS(dup_path)

# ---- match diagnostics ----

# get a list of the proportion of SN of a capture event present in the expected
## temperature data files.
matches_list <- lapply(seq_len(nrow(file_lookup)), function(x) {
  info = file_lookup[x, ]
  caps <- captures |> 
    filter(siteID %in% info$site & plot_bout == info$plot_bout)
  out <- lapply(temp_streams_list, function(y) {
    in_sum = sum(caps$SN_In %in% names(y))
    out_sum = sum(caps$SN_Out %in% names(y))
    prop = (in_sum + out_sum) / (nrow(caps) * 2)
  }) |> unlist()
  out
})
names(matches_list) <- file_lookup$label

# filter this by only the bout in question (bout-bout matches)
bout_match_prop <- lapply(file_lookup$label, function(x){
  matches_list[[x]][x]
}) |> unlist() |> round(2)

# look at the bouts that had duplicates removed 
problem_bouts <- bout_match_prop[names(dup_pairs)]
# # ORNL_2 ORNL_4 STEI_1 UNDE_2 UNDE_3 
# # 1.00   0.93   0.64   0.89   0.63 

# ---- data stream by bout ----

# get the time zone for Madison WI
MSN_tz <- tz_lookup_coords(
  lat = 43.073051, lon = -89.401230, method = "accurate"
)

# create a rowwise capture table
capture_data <- captures |> select(
  "siteID", "plotID", "nightuid", "trapCoordinate", "tagID", "uid", 
  "scientificName", "trapStatus", "collectDate", "time_zone",
  "ibutton_pair", "SN_In", "SN_Out", "plot_bout", 
  "decimalLatitude", "decimalLongitude"
) |> 
  mutate(cap_num = row_number()) |> 
  rowwise() |> 
  mutate(
    collectDate = as_date(collectDate),
    local_midnight = as_datetime(collectDate, tz = .data$time_zone),
    bout_label = paste(siteID, plot_bout, sep = "_")
  ) |> 
  relocate(cap_num, bout_label, .before = 1)

# Calculate sunset and sunrise at each location
capture_data <- capture_data |> 
  select(siteID, local_midnight, decimalLongitude, decimalLatitude, time_zone) |> 
  distinct() |> rowwise() |> mutate(
    sunset = sunset(
      local_midnight - days(1), decimalLongitude, decimalLatitude,
      tz = .data$time_zone, force_tz = FALSE
    ),
    sunrise = sunrise(
      local_midnight,  decimalLongitude, decimalLatitude, tz = .data$time_zone,
      force_tz = FALSE
    ),
    trapping_interval = interval(
      start = sunset - hours(3), end = sunrise + hours(3)
    )
  ) |> 
  left_join(capture_data)

# get the temperature data for each pair
capture_data <- capture_data |> rowwise() |> 
  mutate(
    ibutton_data = list(
      temp_streams_list[[bout_label]] |> 
        mutate(
          madison.DTime = as_datetime(DateTime, tz = MSN_tz),
          local.DTime = with_tz(madison.DTime, tz = time_zone)
        ) |> 
        filter(local.DTime %within% trapping_interval) |> 
        select(-DateTime, -Time) |> 
        relocate(madison.DTime, local.DTime, .before = 1) |>  
        pivot_longer(
          -c(madison.DTime:local.DTime), names_to = "SN", values_to = "tempC"
        ) |> 
        mutate(
          position = case_when(
            SN %in% SN_In ~ "inside",
            SN %in% SN_Out ~ "outside",
            .default = NA
          ),
          pairID = case_when(
            position == "inside" ~ .env$ibutton_pair[match(SN, SN_In)],
            position == "outside" ~ .env$ibutton_pair[match(SN, SN_Out)],
            .default = NA
          )
        ) |> 
        filter(!is.na(pairID)) |> 
        arrange(pairID, madison.DTime) |> 
        pivot_wider(names_from = position, values_from = c(tempC, SN))
    ) 
  )

# look at the data
capture_data |> 
  filter(nrow(ibutton_data) > 0) |> 
  select(bout_label, ibutton_pair, collectDate, ibutton_data)

# apply the function to each row...
capture_data <- capture_data |> 
  rowwise() |> 
  filter(nrow(ibutton_data) > 0, ncol(ibutton_data) == 7) |> 
  mutate(
    possible_cap_times = list(
      try(
        estimate_capture_time(
          x = .data$ibutton_data$tempC_inside, 
          y = .data$ibutton_data$tempC_outside, 
          time = .data$ibutton_data$madison.DTime, k = 10
        )
      )
    )
  ) |>
  full_join(capture_data)
## Some warnings are generated

# flag the messed up pairs
capture_data <- capture_data |> 
  filter(
    is.null(possible_cap_times) | 
      nrow(possible_cap_times) == 0 || 
      length(possible_cap_times) == 0 ||
      !is_tibble(possible_cap_times)
  ) |> 
  select(
    siteID, plot_bout, ibutton_pair, collectDate, possible_cap_times, 
    ibutton_data
  ) |>
  rowwise() |> 
  mutate(
    captime_issue = list(case_when(
      class(possible_cap_times) == "try-error" ~ "cap time error",
      nrow(ibutton_data) == 0 ~ "missing pair",
      ncol(ibutton_data) == 5 ~ "unpaired button",
      ncol(ibutton_data) == 7 ~ "no cap detected",
      .default = NA
    )),
  ) |> 
  unnest(captime_issue) |> 
  full_join(capture_data)

# See which rows caused try() errors
error_rows <- which(capture_data$captime_issue == "cap time error")

# replace missing values (including the try error) with an empty tibble
capture_data$possible_cap_times <- lapply(
  capture_data$possible_cap_times, 
  function(df) {
    if (is_tibble(df)) {return(df)} else {return(tibble())}
  }
)

# get only the rows that have successful capture estimates
complete_captures <- capture_data |> 
  filter(is.na(captime_issue)) |> 
  # filter(!notibble_captimes) |>
  rowwise() |> 
  mutate(
    which_captime = which(possible_cap_times$pick),
    capture_time = possible_cap_times$start_time[which_captime]
  )

# ---- Save plots ----
fig_save_dir = file.path(graphics_dir)  
for (bout_i in unique(complete_captures$bout_label)) {
  # make the save path
  save_path = file.path(
    fig_save_dir, 
    paste0("capture-paired-temperatures_", bout_i, ".jpg")
  )
  
  # get the capture data for this bout
  cap_bout = complete_captures |> filter(.data$bout_label == .env$bout_i)
  
  # combine the capture time tables for this bout
  cap_times <- cap_bout |> rowwise() |> 
    mutate(
      possible_cap_times = list(
        possible_cap_times |> 
          mutate(pair = .env$ibutton_pair, collectDate = .env$collectDate)
      )
    ) |> 
    pull(possible_cap_times) |> 
    bind_rows()
  
  # combine the ibutton temperature data for this week
  temp_data <- cap_bout |> rowwise() |> 
    mutate(
      ibutton_data = list(
        ibutton_data |> 
          mutate(
            pair = .env$ibutton_pair, collectDate = .env$collectDate 
          )
      )
    ) |> 
    pull(ibutton_data) |> 
    bind_rows()
  
  # build the figure
  out_fig <- ggplot(temp_data, aes(x = madison.DTime, group = collectDate)) + 
    facet_rep_wrap(~pair, scales = "free_y") +
    geom_vline(
      data = cap_times, linetype = "dashed", linewidth = 0.5,
      col = ifelse(cap_times$pick, "purple", "grey80"),
      aes(xintercept = start_time)
    ) + 
    geom_rect(
      data = cap_times, alpha = 0.08, inherit.aes = FALSE,
      fill = ifelse(cap_times$pick, "purple", "grey80"),
      aes(xmin = start_time, xmax = end_time, ymin = -Inf, ymax = Inf)
    ) + 
    geom_line(aes(y = tempC_inside, col = "inside"), linewidth = 0.2) + 
    geom_line(aes(y = tempC_outside, col = "outside"), linewidth = 0.2) + 
    geom_text(aes(x = min(madison.DTime), y = Inf, label = pair),
              vjust = 1.5, hjust = 0) + 
    theme_bw() +
    labs(
      x = "Trap night", y = "Temperature (C)", title = bout_i
    ) + 
    scale_x_datetime(
      date_labels = "%b-%d", breaks = scales::date_breaks("1 day")
    ) +
    scale_color_manual(values = c('orange', "forestgreen")) + 
    theme(
      strip.placement = "inside", strip.background = element_blank(),
      strip.text = element_blank(),
      legend.position = "none",
      panel.spacing.y = unit(-0.5, "lines"),
      title = element_text(size = 10)
    )
  
  # save the figure
  ggsave(filename = save_path, plot = out_fig, width = 14, height = 9)
}

#  ---- Flag additional problems ----

fix_these <- tribble(
  ~bout_label, ~ibutton_pair, ~collectDate, ~captime_issue,
  # "UNDE_4", "390", "2024-08-30", "too late",
  # "UNDE_4", "315", "2024-08-30", "too late",
  # 
  # "UNDE_3", "550", "2024-08-03", "too early",
  # "UNDE_3", "197", "2024-08-03", "too early",
  # 
  # "UNDE_1", "269", "2024-05-15", "too early",
  # 
  # "STEI_4", "203", "2024-09-06", "too early",
  # "STEI_3", "006", "2024-07-26", "too early",
  # 
  # "STEI_2", "002", "2024-07-03", "too late",
  # 
  # "SCBI_4", "180", "2024-09-04", "too late",
  # 
  # "SERC_2", "376", "2024-08-08", "too_late",
  # "SERC_2", "378", "2024-06-04", "bad pair",
  # "SERC_2", "303", "2024-06-05", "bad pair",
  # "SERC_2", "396", "2024-06-05", "too late",
  # 
  # "ORNL_4", "251", "2024-10-08", "too late",
  # 
  # "ORNL_3", "213", "2024-06-05", "too late",
  # "ORNL_3", "244", "2024-06-05", "too late",
  # 
  # "ORNL_2", "074", "2024-05-02", "too late",
  # 
  # "ORNL_1", "381", "2024-03-12", "too early",
  # 
  # "MLBS_4", "009", "2024-09-26", "too early",
  # "MLBS_4", "070", "2024-09-26", "too late",
  # 
  # "MLBS_3", "467", "2024-08-07", "too early",
  # "MLBS_3", "495", "2024-08-07", "too late",
  # 
  # "MLBS_2", "279", "2024-07-02", "too early",
  # "MLBS_2", "279", "2024-07-03", "too early",
  # "MLBS_2", "290", "2024-07-02", "too early",
  # "MLBS_2", "290", "2024-07-03", "too early",
  # "MLBS_2", "293", "2024-07-03", "too early",
  # 
  # "MLBS_1", "524", "2024-05-16", "bad button",
  # "MLBS_1", "507", "2024-05-14", "too early",
  # "MLBS_1", "508", "2024-05-14", "too early",
  # 
  # "HARV_3", "455", "2024-08-27", "too late",
  # "HARV_3", "508", "2024-08-27", "bad button",
  # "HARV_3", "508", "2024-08-28", "bad button",
  # "HARV_3", "584", "2024-08-27", "too late",
  # 
  # "BLAN_4", "121", "2024-09-26", "too late",
  # "BLAN_4", "142", "2024-09-25", "too late",
  # "BLAN_4", "149", "2024-09-26", "too late",
  # "BLAN_4", "148", "2024-09-26", "too late",
  # "BLAN_4", "169", "2024-09-26", "too late",
  # "BLAN_4", "175", "2024-09-26", "too late",
  # "BLAN_4", "178", "2024-09-24", "too late",
  # "BLAN_4", "190", "2024-09-25", "too late",
  # "BLAN_4", "194", "2024-09-25", "too late",
  # "BLAN_4", "198", "2024-09-25", "too late",
  # 
  # "BLAN_2", "002", "2024-05-29", "too early",
  # "BLAN_2", "095", "2024-09-29", "too late",
  # 
  # "BLAN_1", "020", "2024-04-03", "too late",
  # "BLAN_1", "037", "2024-04-03", "too late",
) |>
  mutate(trap_night = as_date(collectDate))


# ---- Update the selected capture event, if needed ----

# merge the complete captures back into the table
joined_captures <- full_join(capture_data, complete_captures) |> 
  distinct()

# check the proportion of rows with issues
bad_prop = nrow(joined_captures |> filter(!is.na(captime_issue))) / 
  nrow(joined_captures)
good_prop = 1 - bad_prop

# update the full capture data
updated_captures <- full_join(captures, joined_captures) |> 
  distinct()

# ---- Save the output ----
saveRDS(joined_captures, file = out_file_path)
saveRDS(updated_captures, update_captures_path)

# ---- Preliminary plot of the distros ----

joined_captures |> 
  filter(!is.na(capture_time)) |> 
  mutate(diff_time = difftime(capture_time, local_midnight, units = "hours")) |> 
  ggplot(aes(y = siteID, x = diff_time)) + 
  geom_violin(draw_quantiles = c(0.25)) + 
  labs(x = "Capture time (hours after midnight)", y = "site") +
  scale_x_continuous()
