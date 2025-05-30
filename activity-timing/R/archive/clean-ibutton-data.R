# ---- Join ibutton data (2024) ----

pacman::p_load(dplyr)

# get the metadata for each ibutton (SN info)
metadata <- read.csv(
  "data/2024 - UW_Madison_Fob_Serial_Numbers.csv",
  colClasses = "character" # to avoid rounding digits
) |>
  # rename the columns
  setNames(c(
    "pair", "IO", "name", "new_pair", "SerialNumber", "pairStatus",
    "OK", "Notes"
  )) |>
  # convert pair and SN to factors
  mutate(pair = as.factor(pair), SerialNumber = as.factor(SerialNumber))

# get info about where ibuttons were deployed
deployment <- read.csv("data/ibuttons_2024L0.csv") |>
  # pad pair labels with zeros on the left side, to match the metadata
  mutate(
    iButtonLabel = stringr::str_pad(
      string = iButtonLabel, width = 3, side = "left", pad = "0"
    )
  )

# get all deployed labels (pair ID) that are not present in metadata
unmatched_labels <- deployment$iButtonLabel[
  which(!deployment$iButtonLabel %in% metadata$pair)
]

# reformat the "new_pair" column into a useful lookup table
new_pairs <- metadata |>
  filter(grepl(".{1,}", new_pair)) |> # non-empty new_pair col
  select(pair, new_pair) # only "pair" and "new_pair" cols

# check if the unmatched pairs are contained in the new_pair list
unmatched_labels %in% new_pairs$new_pair # all TRUE

warning(paste(
  "Unmatched labels", paste(unmatched_labels, collapse = ", "),
  "that do not specify 'IN' or 'OUT'. Cannot be matched to table."
))
