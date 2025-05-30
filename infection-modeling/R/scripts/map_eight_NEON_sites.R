# Map the sites 

library(tidyverse)
library(tigris)

US_states <- states(cb = TRUE)

# list of the non contiguous US states and territories
non_contig <- c(
  "Alaska", "Hawaii", "Guam", "American Samoa", "Puerto Rico",
  "United States Virgin Islands", 
  "Commonwealth of the Northern Mariana Islands"
)

# filter to lower 48
US_states <- US_states |> filter(!NAME %in% non_contig)


# read NEON site data
eight_sites <- read.csv("infection-modeling/data/raw-data/NEON-SiteMap-Table.csv")

# manually adjust the label offsets
## "BLAN" "HARV" "MLBS" "ORNL" "SCBI" "SERC" "STEI" "UNDE"
eight_sites$label_offx <- c(0.5, 0, 0, 0, -0.5, 1.2, -1.1, 1.3)
eight_sites$label_offy <- c(.55, 0.57, -.56, -.56, -0.53, 0, 0, 0)

# plot the states, with 8 NEON sites on top
ggplot() +
  geom_sf(data = US_states, fill = "grey95") +
  geom_point(
    data = eight_sites, size = 2, show.legend = FALSE,
    aes(x = longitude, y = latitude, col = siteCode),
  ) + 
  geom_label(
    data = eight_sites, size = 3.5, fontface = "bold", show.legend = FALSE,
    aes(x = longitude + label_offx, y = latitude + label_offy, label = siteCode,
        col = siteCode)
  ) + 
  scale_x_continuous(limits = c(-92, -70)) + 
  scale_y_continuous(limits = c(35, 47)) + 
  scale_color_manual(values = eight_sites$color_hex) + 
  scale_fill_manual(values = eight_sites$color_hex) + 
  labs(x = NULL, y = NULL) + 
  theme_bw() + theme(panel.grid = element_blank())
  
# save the map
ggsave(
  file.path("infection-modeling/graphics", "eight_NEON_sites_map.jpg"), dpi = 300,
  width = 6.5, height = 0.7*6.5
)
