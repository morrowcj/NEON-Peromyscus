library(tidyverse)
library(tigris)

## ---- Site locations ----

# Load locations and meta data of NEON sites
site_locations <- read.csv(
  "infection-modeling/data/raw-data/NEON-SiteMap-Table.csv"
) %>%
  tibble() %>% 
  rename(siteID = siteCode)

# save the location data
saveRDS(site_locations, 'infection-modeling/data/site-location-data.rds')

## ---- Trap locations ----

# Load locations and meta data of NEON small mammal traps
trap_locations <- readRDS(
  file.path(
    "neon-data/data/eight_sites/location_data/small-mammal-trap-locations.rds"
  )
) %>% tibble() %>% 
  relocate(siteID, plotID, trapCoordinate, .before = 0)

# save the location data
saveRDS(trap_locations, "infection-modeling/data/trap-location-data.rds")

# ---- Map the sites ----

US_states <- tigris::states(cb = TRUE)

# list of the non contiguous US states and territories
non_contig <- c(
  "Alaska", "Hawaii", "Guam", "American Samoa", "Puerto Rico",
  "United States Virgin Islands", 
  "Commonwealth of the Northern Mariana Islands"
)

# filter to lower 48
US_states <- US_states |> filter(!NAME %in% non_contig)


# read NEON site data
site_locations <- read.csv(
  "infection-modeling/data/raw-data/NEON-SiteMap-Table.csv"
)

# manually adjust the label offsets
## "BLAN" "HARV" "MLBS" "ORNL" "SCBI" "SERC" "STEI" "UNDE"
site_locations$label_offx <- c(0.5, 0, 0, 0, -0.5, 1.2, -1.1, 1.3)
site_locations$label_offy <- c(.55, 0.57, -.56, -.56, -0.53, 0, 0, 0)

# plot the states, with 8 NEON sites on top
ggplot() +
  geom_sf(data = US_states, fill = "grey95") +
  geom_point(
    data = site_locations, size = 2, show.legend = FALSE,
    aes(x = longitude, y = latitude, col = siteCode),
  ) + 
  geom_label(
    data = site_locations, size = 3.5, fontface = "bold", show.legend = FALSE,
    aes(x = longitude + label_offx, y = latitude + label_offy, label = siteCode,
        col = siteCode)
  ) + 
  scale_x_continuous(limits = c(-92, -70)) + 
  scale_y_continuous(limits = c(35, 47)) + 
  scale_color_manual(values = site_locations$color_hex) + 
  scale_fill_manual(values = site_locations$color_hex) + 
  labs(x = NULL, y = NULL) + 
  theme_bw() + theme(panel.grid = element_blank())
  
# save the map
ggsave(
  file.path("infection-modeling/graphics", "eight_NEON_sites_map.jpg"), dpi = 300,
  width = 6.5, height = 0.7*6.5
)
