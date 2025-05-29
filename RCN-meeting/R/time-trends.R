library(tidyverse)
library(lubridate)

# ---- Setup ----
# load trap nights, which contain bout information
trap_nights <- readRDS("data/mammalTrapBoutInfo.rds")

# load mammal data
mammals <- readRDS("data/RAW-NEON_mammal-trap-data.rds")$mam_pertrapnight

# how many rows are in the mammal data?
mammal_rows <- nrow(mammals)

# merge the trap night info into the mammal data
mammals <- mammals |> 
  mutate(
    year = year(collectDate), date = date(collectDate)
  ) |> 
  left_join(trap_nights)

# check the merge
stopifnot(nrow(mammals) == mammal_rows)

# ---- Mass (raw) patterns through time

pero_cycles <- mammals |> 
  filter(
    !is.na(scientificName),
    # grepl("Peromyscus", scientificName)
    taxonID %in% c("PELE", "PEMA")
  ) |> 
  group_by(year, siteID, plotID, trap_bout, scientificName) |> 
  summarize(
    lat = unique(decimalLatitude), lon = unique(decimalLongitude),
    mass_n = length(!is.na(weight)),
    mean_mass = mean(weight, na.rm = TRUE), sd_mass = sd(weight, na.rm = TRUE),
    captured_n = n()
  ) |> 
  group_by(siteID, scientificName) |> 
  mutate(n_years = length(unique(year))) |> 
  filter(n_years >= 5) |> 
  ungroup() |> 
  # arrange by lat*long
  arrange(lat*lon) |> 
  mutate(siteID = factor(siteID, levels = unique(siteID)))
  

pero_cycles |> 
  # filter(mass_n > 5) |> 
  ggplot(aes(x = year, y = captured_n, col = scientificName)) + 
  facet_wrap(~siteID, scales = "free_y") +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 1) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(2000, 2025, by = 3))

# scaled
pero_cycles |> 
  group_by(siteID, scientificName) |> 
  mutate(captured_n = scale(captured_n)) |> 
  # filter(mass_n > 5) |> 
  ggplot(aes(x = year, y = captured_n, col = scientificName)) + 
  facet_wrap(~siteID, scales = "free_y") +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 1) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(2000, 2025, by = 3))


pero_cycles |> 
  # filter(mass_n > 5) |> 
  ggplot(aes(x = year, y = mean_mass, col = scientificName)) + 
  facet_wrap(~siteID, scales = "free_y") +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 1) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(2000, 2025, by = 3))


# scaled
pero_cycles |> 
  group_by(siteID, scientificName) |> 
  mutate(mean_mass = scale(mean_mass)) |> 
  # filter(mass_n > 5) |> 
  ggplot(aes(x = year, y = mean_mass, col = scientificName)) + 
  facet_wrap(~siteID, scales = "free_y") +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 1) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(2000, 2025, by = 3))
