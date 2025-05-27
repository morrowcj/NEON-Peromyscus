# Community analyses NEON

# ---- packages ----
pkgs <- c("tidyr", "dplyr", "lubridate", "vegan", "ggplot2")
pacman::p_load(pkgs, character.only = TRUE)

# ---- Load Data ----

## Mammals
# small mammal trapping data
small_mammal_prod <- readRDS("data/neon-data/mammal-trap-data.rds")
# beetle pitfall trap collection data
beetle_prod <- readRDS("data/neon-data/beetle-pitfall-data.rds")
# plant presence and cover 
plant_prod <- readRDS("data/neon-data/plant-presence-and-cover.rds")

eight_sites <- c("HARV", "ORNL", "SCBI", "BLAN", "SERC", "STEI", "UNDE", "MLBS")

# ---- Build community matrices

## Mammals - abundance
# mammal communities
small_mammals <- small_mammal_prod$mam_pertrapnight |> 
  mutate(
    trapStatNum = as.numeric(gsub("^(\\d{1}).*", "\\1", trapStatus)),
    year = year(collectDate), 
    month = month(collectDate)
  ) |> 
  filter(
    trapStatNum >= 4,
    year >= 2021, # TODO for now, only look at the last 5 years
    siteID %in% eight_sites # TODO for now, only the 8 sites used
  ) 

# list of all species codes and scientific names
mam_species <- small_mammals |> 
  group_by(taxonID, scientificName) |> tally() |> 
  arrange(desc(n))

# factorize taxon IDs, ordered by abundance
mam_species$taxonID <- with(
  mam_species, 
  factor(taxonID, levels = taxonID[order(n, decreasing = T)])
)
small_mammals$taxonID <- factor(
  small_mammals$taxonID, levels = levels(mam_species$taxonID)
)
spec_cols <- levels(mam_species$taxonID)

# counts of each species by month
monthly_mammals <- small_mammals |> 
  group_by(year, month, taxonID, siteID, plotID) |> 
  summarize(count = n(), .groups = "drop") |> 
  arrange(taxonID)

# build community dataframe
mamcom <- monthly_mammals |> 
  pivot_wider(names_from = taxonID, values_from = count, values_fill = 0)
# add in total captures
mamcom$total_abundance = rowSums(mamcom[, spec_cols])

# document very rare species
rare_cutoff <- 0.01
species_counts <- colSums(mamcom[, spec_cols])
rare = (species_counts / nrow(mamcom)) <= rare_cutoff
rare_species = spec_cols[rare]

# update, excluding rows without any captures
mamcom <- mamcom |> filter(total_abundance > 0)

# get only the species count cols
mamcommat <- mamcom[, spec_cols]

## Plant communities ----
plants <- plant_prod$div_10m2Data100m2Data |> 
  mutate(month = month(endDate),
         year = year(endDate)) |> 
  filter(
    year >= 2021, # TODO for now, only look at the last 5 years
    siteID %in% eight_sites # TODO for now, only the 8 sites used)
  )

# list of all species codes and scientific names
plant_species <- plants |> 
  group_by(taxonID, scientificName) |> tally() |> 
  arrange(desc(n))

# factorize taxon IDs, ordered by abundance
plant_species$taxonID <- with(
  plant_species, 
  factor(taxonID, levels = taxonID[order(n, decreasing = T)])
)
plants$taxonID <- factor(
  plants$taxonID, levels = levels(plant_species$taxonID)
)
plant_cols <- levels(plant_species$taxonID)

# counts of each species by month
monthly_plants <- plants |> 
  group_by(year, month, taxonID, siteID, plotID) |> 
  summarize(count = n(), .groups = "drop") |> 
  arrange(taxonID)

# build community dataframe
plantcom <- monthly_plants |> 
  pivot_wider(names_from = taxonID, values_from = count, values_fill = 0)
# add in total captures
plantcom$total_abundance = rowSums(plantcom[, plant_cols])

# document very rare species
plant_counts <- colSums(plantcom[, plant_cols])
prare = (plant_counts / nrow(plantcom)) <= rare_cutoff
rare_plants = plant_cols[prare]

# update, excluding rows without any captures
plantcom <- plantcom |> filter(total_abundance > 0)

# get only the species count cols
plantcommat <- plantcom[, plant_cols]

## Beetle communities ----
beetles <- beetle_prod$bet_expertTaxonomistIDProcessed |> 
  mutate(
    year = year(collectDate), 
    month = month(collectDate)
  ) |> 
  filter(
    year >= 2021, # TODO for now, only look at the last 5 years
    siteID %in% eight_sites # TODO for now, only the 8 sites used
  ) 

# list of all species codes and scientific names
beetle_species <- beetles |> 
  group_by(taxonID, scientificName) |> tally() |> 
  arrange(desc(n))

# factorize taxon IDs, ordered by abundance
beetle_species$taxonID <- with(
  beetle_species, 
  factor(taxonID, levels = taxonID[order(n, decreasing = T)])
)
beetles$taxonID <- factor(
  beetles$taxonID, levels = levels(beetle_species$taxonID)
)
beetle_cols <- levels(beetle_species$taxonID)

# counts of each species by month
monthly_beetles <- beetles |> 
  group_by(year, month, taxonID, siteID, plotID) |> 
  summarize(count = n(), .groups = "drop") |> 
  arrange(taxonID)

# build community dataframe
beetlecom <- monthly_beetles |> 
  pivot_wider(names_from = taxonID, values_from = count, values_fill = 0)
# add in total captures
beetlecom$total_abundance = rowSums(beetlecom[, beetle_cols])

# document very rare species
beetle_counts <- colSums(beetlecom[, beetle_cols])
brare = (beetle_counts / nrow(beetlecom)) <= rare_cutoff
rare_beetles = spec_cols[rare]

# update, excluding rows without any captures
beetlecom <- beetlecom |> filter(total_abundance > 0)

# get only the species count cols
beetlecommat <- beetlecom[, beetle_cols]

# ---- Ordination ----
# Mammal community PCA
mamRDA <- rda(mamcommat |> select(-PELEPEMA))
mam_scores <- scores(mamRDA)
mamcom[, c("PC1", "PC2")] <- mam_scores$sites
mam_spec_scores <- mam_scores$species |> 
  as.data.frame() |> 
  tibble::rownames_to_column(var = "taxonID") |> 
  mutate(show_label = abs(PC1) >= 0.5 | abs(PC2) >= 0.5)

# mamDist <- vegdist(mamcommat |> select(-PELEPEMA) |> distinct())
# mamMDS <- metaMDS(mamDist)
# mamMDS_scores <- scores(mamRDA)
# mamcom[, c("MDS1", "MDS2")] <- mamMDS_scores$sites
# mam_spec_scores <- mam_scores$species |> 
#   as.data.frame() |> 
#   tibble::rownames_to_column(var = "taxonID") |> 
#   mutate(show_label = abs(PC1) >= 0.5 | abs(PC2) >= 0.5)

# Plant community PCA
plantRDA <- rda(plantcommat)
plant_scores <- scores(plantRDA)
plantcom[, c("PC1", "PC2")] <- plant_scores$sites
plant_spec_scores <- plant_scores$species |> 
  as.data.frame() |> 
  tibble::rownames_to_column(var = "taxonID") |> 
  mutate(show_label = abs(PC1) >= 0.5 | abs(PC2) >= 0.5)

# beetle community PCA
beetleRDA <- rda(beetlecommat)
beetle_scores <- scores(beetleRDA)
beetlecom[, c("PC1", "PC2")] <- beetle_scores$sites
beetle_spec_scores <- beetle_scores$species |> 
  as.data.frame() |> 
  tibble::rownames_to_column(var = "taxonID") |> 
  mutate(show_label = abs(PC1) >= 0.5 | abs(PC2) >= 0.5)

# mammal ~ plant RDA
## summarize mammals to site-month
mamcom_grp <- mamcom |> select(-PC1, -PC2) |> 
  group_by(year, month, siteID) |> 
  summarize(across(all_of(spec_cols), ~mean(.x, na.rm = TRUE)), n = n(),
            .groups = "drop")

## summarize plants to site-month
plantcom_grp <- plantcom |> select(-PC1, -PC2) |> 
  group_by(year, month, siteID) |> 
  summarize(across(all_of(plant_cols), ~mean(.x, na.rm = TRUE)), n = n(),
            .groups = "drop")

## summarize beetles to site-month
beetlecom_grp <- beetlecom |> select(-PC1, -PC2) |> 
  group_by(year, month, siteID) |> 
  summarize(across(all_of(beetle_cols), ~mean(.x, na.rm = TRUE)), n = n(),
            .group = "drop")

## combine mammals and plants into a single table
plantmamcom_grp <- full_join(
  plantcom_grp, mamcom_grp, by = c("year", "month", "siteID")
) %>%
  select(year, month, siteID, all_of(spec_cols), all_of(plant_cols)) %>%
  filter(complete.cases(.))

## combine mammals and beetles
beetlemamcom_grp <- full_join(
  mamcom_grp, beetlecom_grp, by = c("year", "month", "siteID")
) |> 
  select(year, month, siteID, all_of(spec_cols), all_of(beetle_cols)) %>%
  filter(complete.cases(.))

## combine plants and beetles
plantbeetlecom_grp <- full_join(
  plantcom_grp, beetlecom_grp, by = c("year", "month", "siteID")
) |> 
  select(year, month, siteID, all_of(plant_cols), all_of(beetle_cols)) %>%
  filter(complete.cases(.))

## run plant-mammal RDA
plantmamRDA <- rda(plantmamcom_grp[, spec_cols[!rare]] |> select(-PELEPEMA),
                   plantmamcom_grp[, plant_cols[!prare]],
                   scale = TRUE)
plantmam_scores <- list(
  sites = plantmamRDA$CCA$u |> data.frame() |> select(RDA1, RDA2), 
  species = plantmamRDA$CCA$v |> data.frame() |> select(RDA1, RDA2),
  biplot = plantmamRDA$CCA$biplot
)
plantmamcom_grp[, c("RDA1", "RDA2")] <- plantmam_scores$sites
plantmamcom_biplot <- plantmam_scores$biplot |> as.data.frame() |>
  tibble::rownames_to_column(var = "taxonID") |>
  mutate(
    show_label = TRUE,
    show_label = abs(RDA1) >=0.45 | abs(RDA2) >= 0.45,
    nudge_x = 0, nudge_y = 0
  )
plantmam_spec_scores <- plantmam_scores$species |>
  as.data.frame() |>
  tibble::rownames_to_column(var = "taxonID") |>
  mutate(
    show_label = TRUE,
    show_label = abs(RDA1) >= 0.25 | abs(RDA2) >= 0.25,
    nudge_x = 0, nudge_y = 0
  )

# run mammal-beetle RDA
beetlemamRDA <- rda(beetlemamcom_grp[, spec_cols[!rare]] |> select(-PELEPEMA), 
                    beetlemamcom_grp[, beetle_cols[!brare]],
                   scale = TRUE)
# beetlemam_scores <- scores(beetlemamRDA)
beetlemam_scores <- list(
  sites = beetlemamRDA$CCA$u |> data.frame() |> select(RDA1, RDA2), 
  species = beetlemamRDA$CCA$v |> data.frame() |> select(RDA1, RDA2),
  biplot = beetlemamRDA$CCA$biplot
)
beetlemamcom_grp[, c("RDA1", "RDA2")] <- beetlemam_scores$sites
beetlemamcom_biplot <- beetlemam_scores$biplot |> as.data.frame() |> 
  tibble::rownames_to_column(var = "taxonID") |> 
  mutate(
    show_label = TRUE,
    show_label = abs(RDA1) >=0.45 | abs(RDA2) >= 0.45,
    nudge_x = 0, nudge_y = 0
  ) 
beetlemam_spec_scores <- beetlemam_scores$species |> 
  as.data.frame() |> 
  tibble::rownames_to_column(var = "taxonID") |> 
  mutate(
    show_label = TRUE,
    show_label = abs(RDA1) >= 0.25 | abs(RDA2) >= 0.25,
    nudge_x = 0, nudge_y = 0
  )

# run mammal-beetle RDA
beetleplantRDA <- rda(plantbeetlecom_grp[, beetle_cols[!brare]], 
                      plantbeetlecom_grp[, plant_cols[!prare]],
                    scale = TRUE)
# beetlemam_scores <- scores(beetlemamRDA)
beetleplant_scores <- list(
  sites = beetleplantRDA$CCA$u |> data.frame() |> select(RDA1, RDA2), 
  species = beetleplantRDA$CCA$v |> data.frame() |> select(RDA1, RDA2),
  biplot = beetleplantRDA$CCA$biplot |> data.frame() |> select(RDA1, RDA2)
)
plantbeetlecom_grp[, c("RDA1", "RDA2")] <- beetleplant_scores$sites
plantbeetlecom_biplot <- beetleplant_scores$biplot |> as.data.frame() |> 
  tibble::rownames_to_column(var = "taxonID") |> 
  mutate(
    show_label = TRUE,
    show_label = abs(RDA1) >=0.5 | abs(RDA2) >= 0.5,
    nudge_x = 0, nudge_y = 0
  ) 
beetleplant_spec_scores <- beetleplant_scores$species |> 
  as.data.frame() |> 
  tibble::rownames_to_column(var = "taxonID") |> 
  mutate(
    show_label = TRUE,
    show_label = abs(RDA1) >= 0.5 | abs(RDA2) >= 0.5,
    nudge_x = 0, nudge_y = 0
  )

# plot the beetle-plant RDA
ggplot(plantbeetlecom_grp, aes(x = RDA1, y = RDA2)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_point(col = "grey50") +
  ## mammal scores
  geom_segment(
    data = beetleplant_spec_scores, 
    aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
    col = "orange"
  ) +
  ## beetle constraints
  geom_segment(
    data = plantbeetlecom_biplot, #|> filter(show_label, !is.na(RDA1)),
    inherit.aes = FALSE, col = "forestgreen",
    aes(x = 0, y = 0, xend = RDA1, yend = RDA2)
  ) + 
  ## beetle labels
  geom_text(data = plantbeetlecom_biplot |> filter(show_label, !is.na(RDA1)),
            inherit.aes = FALSE, col = "forestgreen", size = 3, 
            fontface = "bold",
            aes(x = RDA1, y = RDA2, label = taxonID)) + 
  ## animal labels
  geom_text(data = plantbeetle_spec_scores |> filter(show_label, !is.na(RDA1)),
            col = "orange",
            size = 3, fontface = "bold",
            aes(x = RDA1, y = RDA2, label = taxonID)) +
  theme_classic() + 
  theme(legend.position = "inside", legend.position.inside = c(0.01, 0.01),
        legend.justification = c(0, 0))

ggsave("graphics/neon-communities/beetle-plant-rda.jpg", 
       height = 7.5, width = 1.1*7.5, dpi = 300)

# plot the beetle-mammal RDA
ggplot(beetlemamcom_grp, aes(x = RDA1, y = RDA2)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_point(col = "grey50") +
  ## mammal scores
  geom_segment(
    data = beetlemam_spec_scores, 
    aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
    col = "cornflowerblue"
  ) +
  ## beetle constraints
  geom_segment(
    data = beetlemamcom_biplot, #|> filter(show_label, !is.na(RDA1)),
    inherit.aes = FALSE, col = "orange",
    aes(x = 0, y = 0, xend = RDA1, yend = RDA2)
  ) + 
  ## beetle labels
  geom_text(data = beetlemamcom_biplot |> filter(show_label, !is.na(RDA1)),
            inherit.aes = FALSE, col = "orange", size = 3, 
            fontface = "bold",
            aes(x = RDA1, y = RDA2, label = taxonID)) + 
  ## animal labels
  geom_text(data = beetlemam_spec_scores,
            col = "cornflowerblue",
            size = 3, fontface = "bold",
            aes(x = RDA1, y = RDA2, label = taxonID)) +
  theme_classic() + 
  theme(legend.position = "inside", legend.position.inside = c(0.01, 0.01),
        legend.justification = c(0, 0))

ggsave("graphics/neon-communities/beetle-mammal-rda.jpg", 
       height = 7.5, width = 1.1*7.5, dpi = 300)


## Plot the plant-mammal RDA
ggplot(plantmamcom_grp, aes(x = RDA1, y = RDA2)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_point(col = "grey50") +
  ## mammal scores
  geom_segment(
    data = plantmam_spec_scores, 
    aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
    col = "cornflowerblue"
  ) +
  ## plant constraints
  geom_segment(
    data = plantmamcom_biplot, #|> filter(show_label, !is.na(RDA1)),
    inherit.aes = FALSE, col = "forestgreen",
    aes(x = 0, y = 0, xend = RDA1, yend = RDA2)
  ) + 
  ## plant labels
  geom_text(data = plantmamcom_biplot,
            inherit.aes = FALSE, col = "forestgreen", size = 3, 
            fontface = "bold",
            aes(x = RDA1, y = RDA2, label = taxonID)) + 
  ## animal labels
  geom_text(data = plantmam_spec_scores,
             col = "cornflowerblue",
             size = 3, fontface = "bold",
             aes(x = RDA1, y = RDA2, label = taxonID)) +
  theme_classic() + 
  theme(legend.position = "inside", legend.position.inside = c(0.01, 0.01),
        legend.justification = c(0, 0))

ggsave("graphics/neon-communities/plant-mammal-rda.jpg", 
       height = 7.5, width = 1.1*7.5, dpi = 300)

## Plot the mammal PCA
ggplot(mamcom, aes(x = PC1, y = PC2, col = siteID)) + 
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_point() +
  geom_segment(
    data = mam_spec_scores |> filter(show_label), inherit.aes = FALSE,
    aes(xend = PC1*.5, yend = PC2*.5, x = 0, y = 0), col = "cornflowerblue"
  ) + 
  geom_text(data = mam_spec_scores |> filter(show_label), inherit.aes = FALSE,
            aes(x = PC1*.5, y = PC2*.5, label = taxonID), col = "cornflowerblue",
            size = 3, fontface = "bold") +
  theme_classic() + 
  labs(col = NULL) + 
  theme(legend.position = "inside", legend.position.inside = c(0.99, 0.001),
        legend.justification = c(1, 0))

ggsave("graphics/neon-communities/mammal-pca.jpg", 
       height = 7.5, width = 1.1*7.5, dpi = 300)

ggplot(plantcom, aes(x = PC1, y = PC2, col = siteID)) + 
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_point() + 
  geom_segment(
    data = plant_spec_scores |> filter(show_label), inherit.aes = FALSE,
    aes(xend = PC1, yend = PC2, x = 0, y = 0), col = "forestgreen", alpha = 0.2
  ) + 
  geom_text(data = plant_spec_scores |> filter(show_label), inherit.aes = FALSE,
            aes(x = PC1, y = PC2, label = taxonID), col = "forestgreen",
            size = 2, fontface = "plain", alpha = 1) +
  theme_classic() + 
  labs(col = NULL) + 
  theme(legend.position = "inside", legend.position.inside = c(0.99, 0.001),
        legend.justification = c(1, 0),
        legend.background = element_blank())

ggsave("graphics/neon-communities/plant-pca.jpg", 
       height = 7.5, width = 1.1*7.5, dpi = 300)

# plot the beetle PCA
ggplot(beetlecom, aes(x = PC1, y = PC2, col = siteID)) + 
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_point() +
  geom_segment(
    data = beetle_spec_scores |> filter(show_label), inherit.aes = FALSE,
    aes(xend = PC1*.5, yend = PC2*.5, x = 0, y = 0), col = "cornflowerblue"
  ) + 
  geom_text(data = beetle_spec_scores |> filter(show_label), inherit.aes = FALSE,
            aes(x = PC1*.5, y = PC2*.5, label = taxonID), col = "cornflowerblue",
            size = 3, fontface = "bold") +
  theme_classic() + 
  labs(col = NULL) + 
  theme(legend.position = "inside", legend.position.inside = c(0.001, 0.99),
        legend.justification = c(0, 1))

ggsave("graphics/neon-communities/beetle-pca.jpg", 
       height = 7.5, width = 1.1*7.5, dpi = 300)

# plot the plant PCA
ggplot(plantcom, aes(x = PC1, y = PC2, col = siteID)) + 
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_point() + 
  geom_segment(
    data = plant_spec_scores |> filter(show_label), inherit.aes = FALSE,
    aes(xend = PC1, yend = PC2, x = 0, y = 0), col = "forestgreen"
  ) + 
  geom_text(data = plant_spec_scores |> filter(show_label), inherit.aes = FALSE,
            aes(x = PC1, y = PC2, label = taxonID), col = "forestgreen",
            size = 2, fontface = "bold") +
  theme_classic() + 
  labs(col = NULL) + 
  theme(legend.position = "inside", legend.position.inside = c(0.99, 0.001),
        legend.justification = c(1, 0),
        legend.background = element_blank())

ggsave("graphics/neon-communities/plant-pca.jpg", 
       height = 7.5, width = 1.1*7.5, dpi = 300)
