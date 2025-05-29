library(tidyverse)

# path to the (external) data 
data_dir <- file.path("D:/NEON_Data")


token = readLines("../neon-data-curation/docs/NEON-API-token-morrow5.txt")

## Path to the product subfolder
in_dir = file.path(data_dir, "Raw-and-unstacked", "NEON_count-small-mammals")

out_dir = file.path(data_dir, "R-objects")

# if (!dir.exists(out_dir)) {
#   dir.create(out_dir, recursive = TRUE)
# }
# 
# # list of subdirectories
# subdirs = list.files(in_dir)
# 
# # ID pattern of subdirs
# ID_pattern = "NEON\\.D\\d*\\.([A-Z]*).*(\\d{4}-\\d{2}).*" # TODO: extend pattern?
# 
# # get siteID from the ID_pattern
# extract_ID <- function(x, pat = ID_pattern) {
#   site = gsub(pattern = pat, x = x, replacement = "\\1")
#   date = gsub(pattern = pat, x = x, replacement = "\\2")
#   paste(site, date, sep = "_")
# }
# 
# # IDs for all tlhe files
# IDs = extract_ID(subdirs)
# 
# # pattern to mtch for the table of interest for each sub folder
# table_pattern = "mam_pertrapnight"
# 
# # function to apply to the table
# FUN = function(df){
#   tibble(df)
# }
# 
# # function to combine the tables
# .combine = bind_rows
# 
# stck <- tibble()
# for (f in seq_len(length(subdirs))) {
#   # list all of the files in the subdirector
#   file_path = file.path(in_dir, subdirs[f])
#   
#   # get the path to this file
#   table_path = list.files(file_path, pattern = table_pattern, full.names = TRUE)
#   
#   # get the ID
#   ID = IDs[f]
#   
#   # read the table
#   df <- read.csv(table_path)
#   
#   # apply the function to the data frame
#   df <- do.call(what = FUN, args = list(df = df))
#   
#   # combine the df with the full stack
#   stck <- do.call(.combine, list(stck, df))
# }
# 
# # path to save the output
# out_path = file.path(out_dir, "small-mammals_all-sites_stacked.rds")
# 
# # save it
# saveRDS(stck, file = out_path)
# 
keepers <- c(
  "RMNP", "YELL", "ABBY", "TEAK", "ONAQ", "NIWO", "WREF", "STER",
  "SJER", "MOAB"
)

# read the mammal data
stck <- readRDS(file.path(data_dir, "mammal-trap-data.rds"))$mam_pertrapnight

# remove non-captures
caps <- stck |> 
  filter(
    grepl("5|4", trapStatus),
    taxonID %in% c("PELE", "PEMA", "PELEPEMA"),
    siteID %in% keepers
  ) |> 
  mutate(
    year = year(collectDate), month = month(collectDate), 
    date = date(collectDate)
  )
  
# plot recap proportion by year by site
caps |> 
  group_by(siteID, year) |> 
  mutate(total_n = n()) |>
  group_by(tagID, .add = TRUE) |> 
  mutate(recap_n = n()) |> 
  filter(recap_n > 1, taxonID %in% c("PEMA")) |> 
  select(siteID, year, total_n, recap_n) |> 
  distinct() |> 
  group_by(siteID, year) |> 
  summarize(recap_prop = length(unique(tagID)) / unique(total_n)) |> 
  ggplot(aes(x = year, y = recap_prop, col = siteID)) +
  geom_point() + geom_line()

recaps <- caps |> 
  group_by(siteID, tagID) |> 
  mutate(captures = n()) |> 
  filter(captures > 1)

# estimate distances
if (FALSE) {
  trap_locations <- caps |> 
    select(siteID, plotID, namedLocation, trapCoordinate) |> 
    distinct() |> 
    geoNEON::getLocTOS(dataProd = "mam_pertrapnight", token = token) |> 
    distinct()
  saveRDS(trap_locations, file.path(data_dir, "trap-locations.rds"))
} else {
  trap_locations <- readRDS(file.path(data_dir, "trap-locations.rds")) |> 
    distinct()
}

recaps <- left_join(recaps, trap_locations)

library(geosphere)

recaps <- recaps |> 
  group_by(tagID) |> 
  arrange(tagID, date) |> 
  mutate(
    lastCap = lag(date, n = 1),
    lastCap_days = time_length(date - lastCap, unit = "days"),
    lastLon = lag(adjDecimalLongitude, n = 1),
    lastLat = lag(adjDecimalLatitude, n = 1),
    lastPlot = lag(plotID, n = 1),
    diff_plot = lastPlot == plotID,
    # lastCap_dist =
  ) 

# calculate distance from last capture
distances = NULL
for(i in seq_len(nrow(recaps))) {
  coords.A = cbind(recaps$adjDecimalLongitude[i], recaps$adjDecimalLatitude[i])
  coords.B = cbind(recaps$lastLon[i], recaps$lastLat[i])
  d_i = distm(x = coords.A, y = coords.B) |> as.vector()
  d_i
  distances = c(distances, d_i)
}

recaps$lastCap_dist <- distances


recaps.summry <- recaps |> 
  group_by(year, siteID, tagID) |> 
  summarize(mean_dist = mean(lastCap_dist, na.rm = TRUE),
            sd_dist = sd(lastCap_dist, na.rm = TRUE)) |> 
  arrange(desc(mean_dist)) |> 
  filter(mean_dist <= 7000) |> 
  group_by(year, siteID) |> 
  summarize(
    mean_dist = mean(mean_dist, na.rm = TRUE),
    sd_dist = sd(mean_dist, na.rm = TRUE)
  ) 

recaps.summry |> 
  ggplot(aes(x = year, y = mean_dist, col = siteID)) +
  geom_point() + 
  geom_line() +
  theme_bw() + 
  labs(y = "Mean dispersal (m, across individuals)", x = "Year", col = "Site")
ggsave("dispersal-by-site-year.jpg")
  
tmp <- read.csv(file.path(data_dir, "PlotDatePEMAdata.csv")) |> select(-X)

tmp <- tmp |> 
  mutate(year = year(first.date)) |> 
  group_by(year, siteID) |> 
  summarize(mean_density = mean(N, na.rm = TRUE))
  

recaps.summry |> 
  left_join(tmp) |> 
  ggplot(aes(y = mean_dist, x = mean_density)) + 
  geom_point(aes(col = siteID)) + 
  geom_smooth(method = "lm", aes(group = 1), fill = "grey80") + 
  theme_bw() +
  labs(x = "Mean density (across bouts within sites)", 
       y = "Mean dispersal (m, across individuals)",
       col = "Site")
ggsave("density-by-dispersal.jpg")  
  

recaps.summry |> 
  left_join(tmp) |> 
  ggplot(aes(y = mean_dist, x = mean_density, col = factor(year))) + 
  geom_point() + 
  geom_smooth(method = "lm", aes(group = year), fill = "grey80", alpha = .2) + 
  theme_bw() +
  labs(x = "Mean density (across bouts within sites)", 
       y = "Mean dispersal (m, across individuals)",
       col = "Year")
ggsave("density-dispersal-by-year.jpg")

# calculate unique plots x year combinations
caps |> 
  select(plotID, year) |> 
  distinct() |> 
  nrow()

# ----

dat_list <- load("SiteSpeciesSDdata.RData", verbose = TRUE)

all_sd <- pemasd |> 
  bind_rows(pelesd) |> 
  bind_rows(mumusd) |> 
  bind_rows(mipesd) |> 
  bind_rows(remasd) |> 
  bind_rows(sihisd) |> 
  bind_rows(tastsd)

all_sd <- left_join(all_sd, dtc)

library(tigris)
states <- tigris::states(cb = TRUE) |> 
  filter(!NAME %in% c("Guam", "American Samoa", "Puerto Rico",
                      "United States Virgin Islands", 
                      "Commonwealth of the Northern Mariana Islands",
                      "Alaska", "Hawaii"))

all_sd |> 
  # filter(Latitude > -90) |>
  ggplot(aes(x = Longitude, y = Latitude, fill = sd_N / mean_N)) +
  geom_sf(data = states, inherit.aes = FALSE, fill = "grey85", alpha = 0.5) +
  geom_point(size = 3, shape = 21, col = "black") +
  facet_wrap(~species) + 
  theme_bw() +
  # theme(legend.position = "inside", legend.position.inside = c(1, 0), 
  #       legend.justification = c(1, 0),
  #       plot.margin = margin(l = 0, r = 0)
  #       ) +
  scale_fill_gradient2(low = "blue", midpoint = 1,  high = "red") +
  lims(x = c(-126, NA), y = c(22, 50)) + 
  labs(fill = "CV (pop. size)")
ggsave("pop-CV-map.jpg")

species = unique(all_sd$species)

stck <- stck |> 
  filter(scientificName %in% species) |> 
  mutate(
    year = year(collectDate),
    date = date(collectDate)
  ) 

dirty_cnts <- stck |> 
  group_by(siteID, year, scientificName) |> 
  summarize(n = n()) |> 
  group_by(siteID, scientificName) |> 
  mutate(n = scale(n))
  

dirty_cnts |> 
  filter(
    !siteID %in% c("DEJU", "BONA", "LAJA", "TOOL",
                   "ABBY", "DSNY", "GUAN", "HEAL", "DELA", "LENO", "NIWO",
                   "OSBS", "RMNP", "TEAK", "SRER", "WREF", "HARV")
  ) |> 
  ggplot(aes(x = year, y = n, col = scientificName, shape = scientificName)) +
  # geom_point() + 
  geom_line(aes(group = scientificName), size = 1) +
  facet_wrap(~siteID, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        strip.background = element_blank()) +
  scale_x_continuous(breaks = seq(2012, 2025, by = 2)) +
  scale_color_manual(
    values = c(
      "#332288", "#117733", "#44AA99", "#88CCEE", "#CC6677", "#AA4499", 
      "#882255")
    ) +
  labs()
ggsave("community-cycles.jpg", width = 10, height = 0.5*8)

# syched vs asynched
dirty_cnts |> 
  filter(siteID %in% c("SCBI", "UNDE", "UKFS", "KONZ")) |> 
  ggplot(aes(x = year, y = n, col = scientificName, shape = scientificName)) +
  facet_wrap(~ siteID, scales = "free_x") +
  geom_line(aes(group = scientificName), size = 1) +
  labs(col = "Species", y = "Population size (z-score)", x = "Year") +
  theme_bw() +
  scale_x_continuous(breaks = seq(2010, 2025, by = 3))
ggsave("sync-vs-async_sites.jpg", width = 7, height = 0.8*7)
  

dirty_cnts |> 
  filter(siteID %in% c("DCFS", "GRSM"), grepl("Peromyscus", scientificName)) |> 
  ggplot(aes(x = year, y = n, col = scientificName, shape = scientificName)) +
  facet_wrap(~ siteID, scales = "free_x") +
  geom_line(aes(group = scientificName), size = 1) +
  labs(col = "Species", y = "Population size (z-score)", x = "Year") +
  theme_bw() +
  scale_x_continuous(breaks = seq(2010, 2025, by = 3))
ggsave("Peromyscus-sync-vs-async.jpg", width = 7, height = 0.4*7)


# Map of correlation coefficients (map)
load("SpeciesSiteWeightMods.RData", verbose = TRUE)

tmp <- lapply(names(species.weight.mods), function(n){
  df = data.frame(species.weight.mods[[n]])
  df$species <-  n
  df
}) |> bind_rows()

tmp |> left_join(dtc) |> group_by(species) |> 
  mutate(sig = ifelse(is.na(p_val) | p_val >= 0.05, FALSE, TRUE),
         coef = scale(estimate)) |> 
  ggplot(aes(x = Longitude, y = Latitude, fill = coef, shape = sig)) +
  facet_wrap(~species) +
  geom_sf(data = states, inherit.aes = FALSE, fill = "grey85", alpha = 0.5) +
  geom_point(size = 3, col = "black") +
  scale_fill_gradient2(low = "blue", midpoint = 0, high = "red") +
  lims(x = c(-126, NA), y = c(22, 50)) + 
  scale_shape_manual(values = c(21 ,24)) +
  labs(fill = "relative slope\n(weight ~ pop size)", shape = "significant?") +
  theme_bw()

# ---- mark recapture using marked (EXAMPLE) ----
# library(marked)
# 
# data(dipper)
# dipper.proc=process.data(dipper,model="cjs",begin.time=1)
# dipper.ddl=make.design.data(dipper.proc)
# # fixed p
# mod.Phisex.pdot=crm(
#   dipper.proc,dipper.ddl,
#   model.parameters=list(Phi=list(formula=~sex+time),p=list(formula=~1)),
#   hessian=TRUE
# )
# xx=compute_real(mod.Phisex.pdot,"Phi",unique=TRUE,vcv=TRUE)
# ncaps = nrow(dipper)
# npop = ncaps / c(xx$p$lcl, xx$p$estimate, xx$p$ucl)
# names(npop) = c("UCL", "mean", "LCL")
# npop # population size estimates
# 
# # variable p
# mod.p=crm(
#   dipper.proc,dipper.ddl,
#   model.parameters=list(Phi=list(formula=~1),p=list(formula=~sex+time)),
#   hessian=TRUE
# )
# xx=compute_real(mod.p,"p",unique=TRUE,vcv=TRUE)
# 
# size_estimates <- dipper.ddl$p |> 
#   group_by(sex, time) |> tally() |> 
#   left_join(xx$p) |> 
#   mutate(N = across(c(ucl, estimate, lcl), ~n/.x, .unpack = TRUE))
# 
# size_estimates |> 
#   ggplot(aes(x = time, y = N$estimate, col = sex)) + 
#   geom_point() +
#   # geom_pointrange(aes(ymin = N$ucl, ymax = N$lcl)) +
#   geom_line(aes(group = sex))


# ---- species by plot ----

tmp <- stck |> 
  # mutate(
  #   year = year(collectDate), month = month(collectDate), 
  #   date = date(collectDate)
  # ) |> 
  filter(!is.na(scientificName)) |> 
  group_by(siteID, scientificName) |> 
  tally() |> ungroup() |> 
  complete(siteID, scientificName) |> 
  mutate(n = replace_na(n, 0)) |>
  group_by(scientificName) |> 
  mutate(sites_with = sum(n > 1)) |> 
  arrange(desc(sites_with)) |> 
  ungroup() |> 
  mutate(scientificName = factor(scientificName, levels = unique(scientificName)))
  
tmp |> 
  select(scientificName, sites_with) |> distinct() |> 
  filter(sites_with >= 10) |> 
  ggplot(aes(y = scientificName, x = sites_with)) + 
  geom_col() +
  labs(x = "Number of sites", y = "Species")

ggsave("common_species.jpg")


# relative abundances (kind of)
tmp |> 
  group_by(scientificName) |> 
  summarize(mean_n = mean(n, na.rm = TRUE)) |> 
  arrange(desc(mean_n))

tmp |> 
  filter(sites_with >= 10) |> 
  mutate(present = n>0) |> 
  arrange(desc(sites_with)) |> 
  mutate(scientificName = factor(scientificName, unique(scientificName))) |> 
  select(scientificName, siteID, present) |> distinct() |> 
  ggplot(aes(y = scientificName, x = siteID, fill = present)) +
  geom_tile() +
  scale_fill_manual(values = c("white", "grey50")) +
  theme(axis.text.x = element_text(angle = 90))
  # pivot_wider(names_from = siteID, values_from = present) +
  


