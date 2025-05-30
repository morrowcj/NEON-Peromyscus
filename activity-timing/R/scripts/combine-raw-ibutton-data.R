library(dplyr)
library(tidyr)

# metadata about the ibuttons
fob_info <- read.csv(
  "data/2024 - UW_Madison_Fob_Serial_Numbers.csv", colClasses = "character"
) |>
  setNames(
    c("pair", "IO", "name", "new_pair", "SerialNumber", "pairStatus",
             "OK", "Notes")
  ) |>
  mutate(pair = as.factor(pair), SerialNumber = as.factor(SerialNumber))

# get raw files
raw_ibutton_dir <- "data/raw-iButton-data/"
raw_files <- list.files(raw_ibutton_dir, pattern = "\\.csv")

# build a table from the file names
file_lookup <- stringr::str_split(raw_files, pattern = "_|-") |> 
  lapply(function(x){data.frame(site = x[1], month = x[2], day = x[3])}) |> 
  bind_rows() |> 
  mutate(month = as.numeric(month), 
         day = as.numeric(day),
         name = raw_files) |> 
  arrange(site, month, day) |> 
  group_by(site) |> 
  mutate(bout = row_number()) |> 
  relocate(bout, .before = "name") |> 
  ungroup()

# load all the ibutton files into memory
data_list <- apply(file_lookup, MARGIN = 1, function(x){
  dat <- read.csv(file = file.path(raw_ibutton_dir, x["name"]),
           check.names = FALSE)
  dat$site = x["site"]
  dat$site_bout = x["bout"]
  dat
})

# remove duplicated columns
data_list_deduped <- lapply(data_list, function(x){
  x[, !duplicated(t(x))]
})


# Fix the date and time
data_list_deduped <- lapply(data_list_deduped, function(x){
  
  # convert "m/d/Y T" to "Y-M-D T"
  x$Time = gsub(
    "(\\d{1,2})/(\\d{1,2})/(\\d{2,4})(.*)", "\\3-\\1-\\2\\4", x$Time
  )

  # handle missing seconds
  new_time <- suppressWarnings(ymd_hms(x$Time))
  if (all(is.na(new_time))) {
    new_time  <- suppressWarnings(ymd_hm(x$Time))
  }

  x$DateTime <- new_time
  
  # get the date
  x$Date <- as.Date(x$DateTime)

  # return the updated frame
  x  
})

# convert the data frames into long form
data_list_long <- lapply(data_list_deduped, function(x){
  x |> 
    rename(OG_DateTime = "Time") |> 
    pivot_longer(
      cols = c(-"DateTime", -"OG_DateTime", -"Date", -"site", -"site_bout"), 
      names_to = "SerialNumber",
      values_to = "temp"
    ) |> 
    mutate(
      corrected_SN = gsub("(\\..*)|(\\*)$", "", SerialNumber),
      corrected_SN = gsub("^X", "", corrected_SN)
    )
})

# stack into a single long data frame
full_data_long <- bind_rows(data_list_long)

# Look for missing serial numbers
unique_SN <- unique(full_data_long$corrected_SN)
missing_inx = which(!unique_SN %in% fob_info$SerialNumber)
missing_SN = unique_SN[missing_inx]



# ---- OLDER ----


# # remove a problematic file (ORNL bout 2)  
# bad_files <- file_lookup |> filter((site == "ORNL" & bout == 2),
#                                    (site == "STEI" & bout == 1))
# # TODO: Check on the above files ("ORNL_04_29_000s", "STEI_05_27_100s")?
# file_lookup <- file_lookup |> filter(!(site == "ORNL" & bout == 2),
#                                      !(site == "STEI" & bout == 1))

# combine into a single giant file  
# TODO - need to revisit this

# full_raw_ibutton <- lapply(seq_len(nrow(file_lookup)), function(x){
#   row = file_lookup[x, ]
#   read.csv(file = file.path(raw_ibutton_dir, row$name),
#                   check.names = FALSE) |> 
#     rename(DateTime = "Time") |> 
#     pivot_longer(-DateTime, names_to = "SerialNumber", values_to = "temp") |>
#     mutate(
#       corrected_SN = factor(gsub("\\*", "", SerialNumber)),
#       # corrected_SN = factor(gsub("\\..$", "", corrected_SN)),
#       matched_row = match(corrected_SN, fob_info$SerialNumber),
#       pairID = fob_info$pair[matched_row],
#       name = fob_info$name[matched_row],
#       newpair = fob_info$new_pair[matched_row],
#       newpair = ifelse(grepl(".{1,}", newpair), newpair, NA),
#       site = row$site,
#       file_month = row$month,
#       file_day = row$day,
#       file_name = row$name,
#       bout = row$bout,
#       collectDateTime = ymd_hms(DateTime, tz = "GMT"),
#       collectDate = ymd(collectDateTime),
#       collectTime = hms(collectDateTime)
#     ) |>
#     select(-matched_row) |>
#     relocate(
#       c("corrected_SN", "pairID", "name", "newpair", "site",
#         "file_month", "file_day", "bout", "file_name", "collectDate", 
#         "collectTime"),
#       .before = "temp"
#     ) |>
#     distinct() |> 
#     arrange(DateTime, pairID)
# }) |> 
#   bind_rows() |> 
#   distinct() |> 
#   filter(!is.na(collectDateTime))
# 
# # TODO: a small number (1.1%) of the ibuttons have unknown pairs
# unpaired_buttons <- full_raw_ibutton |> filter(is.na(pairID)) |> select(corrected_SN) |> 
#   unlist() |> unique() 
# # unpaired_buttons
# # length(unpaired_buttons) / length(unique(full_raw_ibutton$corrected_SN))
# 
# saveRDS(full_raw_ibutton, file = "data/raw_iButton_data_2024_combined.rds")
# 
# rm(full_raw_ibutton)
