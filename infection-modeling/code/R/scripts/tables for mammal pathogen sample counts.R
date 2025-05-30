# tables for pathogen sample counts

library(tidyverse)
library(lubridate)

# read the NEON tick-borne pathogen product object
neon_data_dir <- "data-curation/data/eight_sites"

pathogen_prods <- readRDS(
  file.path(neon_data_dir, "rodent-tickborne-pathogen-status.rds")
)

# extract the main pathogen testing table
pathogens <- pathogen_prods$rpt2_pathogentesting |> tibble() |> 
  # add year, month, and DOY columns
  mutate(
    year = year(collectDate), month = month(collectDate), DOY = yday(collectDate)
  )

sites = unique(pathogens$siteID)

# count unique samples per year
pathogens |> 
  select(year, month, siteID, plotID, sampleID) |> 
  distinct() |> 
  mutate(
    ear = grepl(".*\\.E", sampleID),
    sample = gsub("(.*)\\..", "\\1", sampleID)
  ) |> 
  pivot_wider(names_from = ear, values_from = sampleID) |> 
  group_by(year) |> 
  summarize(
    ear = sum(!is.na(`TRUE`)), 
    blood = sum(!is.na(`FALSE`)),
    total = n()
  )

# count plots per month sampled
pathogens |> 
  filter(year >= 2022) |> 
  mutate(month = factor(month.name[month], levels = month.name)) |> 
  select(year, month, siteID, plotID) |> 
  distinct() |> 
  group_by(year, month, siteID, plotID) |> 
  tally() |> ungroup() |> 
  complete(year, month, siteID, plotID) |> 
  group_by(siteID, year, month) |> 
  summarise(n = sum(!is.na(n))) |> 
  pivot_wider(names_from = month, values_from = n) |> 
  select(March:September) |> 
  print(n = 100)


# load the small mammal trapping data products
mammal_prods <- readRDS(file.path(neon_data_dir, "mammal-trap-data.rds"))

# Extract a tibble of the "mammals per trap night" data object
mammals <- mammal_prods$mam_pertrapnight |> tibble() |> 
  mutate(
    year = year(collectDate), month = month(collectDate), DOY = yday(collectDate)
  )


mammals |> 
  filter(year %in% 2021:2024, siteID %in% .env$sites) |> 
  group_by(year, siteID) |> 
  tally() |> 
  pivot_wider(names_from = siteID, values_from = n) |> 
  mutate(total = rowSums(across(BLAN:UNDE)))

