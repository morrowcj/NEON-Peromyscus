# ---- Script packages ----
library(stringr)
library(lubridate)
library(dplyr)

# ---- Script parameters ----

# ibutton data directory
ibutton_dir <- file.path("activity-timing/data/ibuttons")

# directory to look for raw data in
raw_temps_dir <- file.path(ibutton_dir, "raw-temperatures")

# and the directory to output files
cleaned_temps_dir <- file.path(ibutton_dir, "cleaned-temperatures")

# ibutton temperature data directory
ibutton_dir <- file.path(raw_temps_dir, "raw/ibutton-data")

# ibutton-linked trap data path
captures_path = file.path(
  "activity-timing/data/ibuttons/neon-captures-with-ibutton-pairs_2024.rds"
)

# ---- Load data ----

# get the time zone for Madison WI
MSN_tz <- tz_lookup_coords(
  lat = 43.073051, lon = -89.401230, method = "accurate"
)

# NEON captures with our ibutton serial numbers
captures <- readRDS(captures_path)

# ---- Handle raw temperature files ----

# names of all the files in the raw temperature directory
raw_file_names <- list.files(path = raw_temps_dir, pattern = "\\.csv$")

# split each file name by dashes and underscores
name_split_list <- str_split(raw_file_names, pattern = "_|-")

# build a table from the file name pattern including site, date, and bout
file_meta_tab <- lapply(
  X = name_split_list,
  FUN = function(x) tibble(site = x[1], month = x[2], day = x[3])
) |> bind_rows() |> 
  mutate(
    month = as.integer(month), day = as.integer(day), 
    plot_bout = as.integer(NA), file_name = raw_file_names
  ) |> arrange(site, month, day) |> 
  group_by(site) |> 
  mutate(
    plot_bout = row_number(),
    label = paste(site, plot_bout, sep = "_")
  ) |> ungroup() |> 
  relocate("label", .before = "file_name")

# read all the files into a single list object
temp_data_list <- apply(
  X = file_meta_tab, MARGIN = 1, 
  FUN = function(x){
    dat <- read.csv(
      file = file.path(raw_temps_dir, x["file_name"]), 
      check.names = FALSE
    )
    # dat$site <- x["site"]
    # dat$plot_bout <- x["plot_bout"]
    return(dat)
  }
)
names(temp_data_list) <- file_meta_tab$label

# get column names for each file
temp_data_cols <- lapply(temp_data_list, colnames)

# get index of columns that are duplicates of previous ones
duplicate_col_list <- lapply(
  temp_data_list, function(x) which(duplicated(t(x)))
)

# get a boolean vector of the files that have duplicate columns
file_meta_tab$has_duplicates <- lapply(
  X = duplicate_col_list, 
  FUN = function(x) {length(x) > 0}
  ) |> unlist()

# get index of columns that *have been* duplicated by latter ones
duplicated_col_list <- lapply(
  temp_data_list, function(x) which(duplicated(t(x), fromLast = TRUE))
)

# get a paired list of duplicate and duplicated columns
dup_pairs <- lapply(which(file_meta_tab$has_duplicates), function(x){
  col_names <- colnames(temp_data_list[[x]])
  dupes <- duplicate_col_list[[x]]
  duped <- duplicated_col_list[[x]]
  return(
    tibble(duped = col_names[duped], dupe = col_names[dupes])
  )
})

# # the only file where duplicates are a real issue is "STEI_1"
# ## and the columns are fully duplicated (equivalent names and values)
# dup_pairs[["STEI_1"]]

# remove any duplicated columns from each data set
deduped_data_list <- lapply(
  X = temp_data_list, 
  FUN = function(x) x[, !duplicated(t(unname(x)))] |> tibble()
)

# ---- Clean the serial numbers ----

# deduped data
clean_deduped <- lapply(deduped_data_list, function(x){
  new_names <- gsub("(\\..*)|(\\*)$", "", names(x)) # ends with .X or *
  new_names <- gsub("^X", "", new_names) # starting with "X"
  names(x) <- new_names
  x
})

clean_deduped <- lapply(clean_deduped, function(x) {
  # index the columns to be removed
  dupes_ind = which(duplicated(names(x), fromLast = TRUE))
  duped_ind = which(duplicated(names(x)))
  # remove them if needed
  if (length(dupes_ind) > 0 & length(duped_ind) > 0) {
    x = x[, -unique(c(dupes_ind, duped_ind))]
  }
  return(tibble(x))
})

# ---- Clean the time ----
clean_deduped <- lapply(clean_deduped, function(x){
  # copy the time variable
  DateTime <- x$Time
  # convert "m/d/Y T" to "Y-M-D T" where relevant (STEI_1 - of course...)
  DateTime = gsub(
    "(\\d{1,2})/(\\d{1,2})/(\\d{2,4})(.*)", "\\3-\\1-\\2\\4", DateTime
  )
  # handle missing seconds
  # Note that these times are madison times
  new_time <- suppressWarnings(ymd_hms(DateTime, tz = MSN_tz))
  if (all(is.na(new_time))) {
    new_time <- suppressWarnings(ymd_hm(DateTime, tz = MSN_tz))
  }
  # copy into the data
  x$DateTime <- new_time
  
  # handle midnight formatting...
  # missing date index
  na_ind <- which(is.na(x$DateTime))
  # replace the missing date with the correct one
  x$DateTime[na_ind] <- suppressWarnings(
    ymd_hms(paste(DateTime[na_ind], "00:00:00"), tz = MSN_tz)
  )
  # reorder the variables
  x |> 
    tibble() |> 
    relocate("DateTime", .before = "Time")
})

# ---- Save objects ----

# save the duplicate rows list
saveRDS(
  dup_pairs,
  file.path(raw_temps_dir, "r-objects", "temperature-duplicate-columns-list.rds")
)

# the file lookup table
saveRDS(
  file_meta_tab, 
  file = file.path(
    raw_temps_dir, "r-objects", "bout-file-lookup-table_2024.rds"
  )
)

# the full unaltered data list
saveRDS(
  object = temp_data_list,
  file = file.path(
    raw_temps_dir, "r-objects", "all-bouts-temperature-data-list_2024.rds"
  )
)

# the de-duplicated data list
saveRDS(
  object = deduped_data_list, 
  file = file.path(
    raw_temps_dir, "r-objects", "deduped-temperature-data-list_2024.rds"
  )
)

# cleaned de-duped data
saveRDS(
  clean_deduped, 
  file.path(cleaned_temps_dir, "deduped-temperature-data-list_2024.rds")
)
