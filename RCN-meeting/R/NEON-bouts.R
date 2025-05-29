library(tidyverse)
library(lubridate)

# ---- Setup ----
theme_set(
  theme_bw()
)

# path to the (external) data 
data_dir <- file.path("./data")

token = readLines("../neon-data-curation/docs/NEON-API-token-morrow5.txt")

## Path to the product subfolder
# in_dir = file.path(data_dir, "Raw-and-unstacked", "NEON_count-small-mammals")

# path to output objects
out_dir = file.path(data_dir, "R-objects")

# read the mammal data
stck <- readRDS(file.path(data_dir, "RAW-NEON_mammal-trap-data.rds"))$mam_pertrapnight

# ---- Calculate trapnight stats ----

# group the trapping efforts by bout and calculate bout info
trap_nights <- stck |> 
  mutate(date = date(collectDate), year = year(collectDate)) |> 
  select(year, plotID, date) |> 
  distinct() |> 
  arrange(year, plotID, date) |> 
  group_by(year, plotID) |> 
  mutate(
    trap_day = row_number(),
    prev_trapDate = lag(date, 1),
    elapsed_trapDays = time_length(date - first(date), unit = "days"),
    days_between = time_length(date - prev_trapDate, unit = "days") |> 
      replace_na(0),
    trap_bout = replace_na(days_between > 3, FALSE) |> cumsum(),
    trap_bout = trap_bout + 1,
  ) |> 
  group_by(trap_bout, .add = TRUE) |> 
  mutate(
    bout_start = first(date),
    bout_length = length(unique(date)),
    bout_span = 1 + time_length(last(date) - first(date), unit = "days")
  ) |> 
  select(-elapsed_trapDays, -days_between) |>
  ungroup()

## check output
# table of the length of bouts:
trap_nights |> 
  select(year, plotID, trap_bout, bout_length) |> 
  distinct() |> 
  pull(bout_length) |> table()
# and the span of the bout
trap_nights |>  
  select(year, plotID, trap_bout, bout_span) |> 
  distinct() |> 
  pull(bout_span) |> table()
# and in a modern plot
trap_nights |> filter(grepl("SCBI", plotID), year == 2021)

# get gaps between bouts:
between_bouts <- trap_nights |> 
  group_by(year, plotID) |> 
  select(year, plotID, bout_start) |> 
  distinct() |> 
  mutate(
    prev_bout_start = lag(bout_start, n = 1),
    days_beteween = time_length(bout_start - prev_bout_start, unit = "days")
  )

# check the distribution of the gaps between bouts
# between_bouts |> 
#   ggplot(aes(x = days_beteween)) +
#   geom_density(fill = "grey80", col = "black")
# 
# between_bouts$days_beteween |> summary()

saveRDS(trap_nights, "data/mammalTrapBoutInfo.rds")
