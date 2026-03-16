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
  file.path("infection-modeling/graphics", "eight_NEON_sites_map.jpg"), 
  dpi = 300,
  width = 6.5, height = 0.7*6.5
)


## Add Peromyscus shapefiles (15 Mar 2026)
library(sf)
# white-footed mouse home range
leuco <- read_sf(
  file.path(
    "infection-modeling/data/raw-data/shapefiles",
    "white-footed-mouse-data/mWFDEx_CONUS_Range_2001v1.shp"
  )
) %>% 
  st_transform(crs = st_crs(US_states))
# deer mouse home range
manic <- read_sf(
  file.path(
    "infection-modeling/data/raw-data/shapefiles/",
    "north-american-deermouse-data/mNADEx_CONUS_Range_2001v1.shp"
  )
) %>% 
  st_transform(crs = st_crs(US_states))



US_border <- tigris::nation(resolution = "5m") %>%
  st_transform(crs = st_crs(US_states))

US_leuco <- st_intersection(US_states$geometry, leuco$geometry)
US_manic <- st_intersection(US_states$geometry, manic$geometry)

overlap = st_intersection(US_leuco, US_manic)


(pero_map <- (
  ggplot() + 
  geom_sf(data = US_border, fill = "grey85", color = "white") +
  geom_sf(data = US_leuco, aes(fill = "PELE"), color = NA) +
  geom_sf(data = US_manic, aes(fill = "PEMA"), color = NA) +
  geom_sf(data = overlap, aes(fill = "both"), color = NA) +
  geom_sf(data = US_states, fill = NA, color = "white") + 
  geom_point(
    data = site_locations, size = 1, show.legend = FALSE,
    aes(
      x = longitude, y = latitude, 
      # col = siteCode
    ),
    col = "black"
  ) + 
  geom_label(
    data = site_locations, size = 2.5, fontface = "bold", show.legend = FALSE,
    aes(
      x = longitude + (label_offx), y = latitude + label_offy, label = siteCode,
      # col = siteCode
    ), 
    col = "black", fill = "white",
    label.r = unit(0, "lines")
  ) + 
  scale_x_continuous(limits = c(-96.5, -68.2)) +
  scale_y_continuous(limits = c(25.5, 48.5)) +
  scale_fill_manual(
    breaks = c("PELE", "PEMA", "both"),
    # values = colorspace::lighten(c("#7a6452", "#f8ec32", "#535947"), 0.25)
    values = colorspace::lighten(c("#3c354a", "#7a6452", "#535947"), 0.25)
  ) + 
  labs(x = NULL, y = NULL, fill = NULL) + 
  # theme_void(paper = "white") + 
  theme_bw() + 
  theme(
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.1),
    legend.justification = c(1, 0),
    legend.text = element_text(size = 12),
    plot.margin = margin(l = 0, t = 0, b = 0, r = 0),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank()
  ) 
))

# save the map
ggsave(
  file.path("infection-modeling/graphics", "Peromyscus-ranges.jpg"), dpi = 300,
  width = 4.43, height = 4.5
) 

saveRDS(pero_map, "infection-modeling/data/peromyscus-range-neon.rds")
