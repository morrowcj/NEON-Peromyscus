# ---- Script ----
library(dplyr)
library(tidyr)
library(lubridate)

out_dir <- "activity-timing/data/ibuttons/cleaned-metadata"
rm_list = c()

# ---- iButton fob info ----
## Data pertaining to the ibuttons, including their serial number (SN)
## and pairings.

# path to the file
fob_info_path <- file.path(
  "activity-timing/data/ibuttons/raw-metadata", "2024 - UW_Madison_Fob_Serial_Numbers.csv"
)

# read in the data
fob_info <- read.csv(
  fob_info_path, colClasses = "character", na.strings = c("")
) |> 
  tibble()

# replacement column names
fob_info_cols <- c(
  "pair", "IO", "name", "new_pair", "SN", "status", "ok", "notes"
)

# set the names
colnames(fob_info) <- fob_info_cols

# cleanup the columns
fob_info <- fob_info |> mutate(
  pair = factor(pair),
  SN = factor(SN),
  IO = gsub("_", "", IO) |> factor(),
  ok = (!is.na(ok) & ok == "x"),
  is_repair = FALSE,
  sci_ntn_SN = grepl("\\+|\\-", SN) # TODO STUPID EXCEL!!!!
)

# build a table for the new "N" pairs
new_fobs_info <- fob_info |> 
  filter(!is.na(new_pair)) |> 
  mutate(
    status = paste("previously", name),
    pair = gsub("(.*)_(.*)", "\\1", new_pair) |> factor(),
    IO = gsub("(.*)_(.*)", "\\2", new_pair) |> factor(),
    name = new_pair,
    new_pair = NA,
    is_repair = TRUE
  )

# combine into a single table
all_fobs_info <- bind_rows(fob_info, new_fobs_info)

# add intermediates to the removal list
rm_list <- append(rm_list, "fob_info")
rm_list <- append(rm_list, "new_fobs_info")

# save the updated table
saveRDS(
  all_fobs_info, file = file.path(out_dir, "updated-ibutton-fob-info_2024.rds")
)

# reformat into a more useful format
old_pairs <- fob_info |> 
  filter(!is.na(pair)) |> 
  select("pair", "IO", "SN", "is_repair") |> 
  pivot_wider(names_from = "IO", values_from = "SN", names_prefix = "SN_")

# do the same for the new pairs
new_pairs <- new_fobs_info |> 
  filter(!is.na(pair)) |> 
  select("pair", "IO", "SN", "is_repair") |> 
  pivot_wider(names_from = "IO", values_from = "SN", names_prefix = "SN_")

# combine old and new pairs into single table
all_pairs <- bind_rows(old_pairs, new_pairs)  

# add intermediates to remove list
rm_list <- append(rm_list, "old_pairs")
rm_list <- append(rm_list, "new_pairs")

# save as R-object
saveRDS(all_pairs, file = file.path(out_dir, "ibutton_pairs_2024.rds"))

# ---- iButton-capture info ----
## Data pertaining to the trapping efforts and which ibutton (pairs) were used.

# path to the file
fob_trap_path <- "activity-timing/data/ibuttons/raw-metadata/ibuttons_2024L0.csv"

# load the data
fob_traps <- read.csv(fob_trap_path, na.strings = c("")) |> tibble()

# clean it up a bit
fob_traps <- fob_traps |> 
  mutate(
    ibutton_pair = stringr::str_pad(iButtonLabel, 3, "left", "0"),
    # TODO: is endDate redundant with nightuid?
    endDate = ymd(as.Date(endDate)), 
  ) |>
  select(
    -link, -iButtonLabel, -endDate
    )

# join with the ibutton pairs
SN_traps <- left_join(
  fob_traps, all_pairs |> select(-is_repair), by = c("ibutton_pair" = "pair")
)

# remove intermediaries
rm_list <- append(rm_list, "fob_traps")

# save as R-object
saveRDS(SN_traps, file.path(out_dir, "neon-trap-ibutton-pairs_2024.rds"))

# ---- remove all intermediaries ----
rm(list = rm_list)
rm(rm_list)
