library(tidyverse)

## ---- Load raw data ----

# # mammal data
# mammals <- readRDS("infection-modeling/data/mammal-data.rds")

# immunity expression data
immunity <- readRDS("infection-modeling/data/immune-data.rds")

# immunity PCA data
ipca_list <- readRDS("infection-modeling/data/immune-PCA.rds")

# climate PCA data
cpca_list <- readRDS("infection-modeling/data/climate-PCA.rds")

# weather PCA data
wpca_list <- readRDS("infection-modeling/data/weather-PCA.rds")

# capture times data
cap_times <- readRDS("infection-modeling/data/capture-times.rds")

# other behavior data
behavior <- readRDS("infection-modeling/data/individual-behavior-data.rds")

## ---- Prepare Peromyscus data for models ----

# Load peromyscus dataset
Peros <- readRDS("infection-modeling/data/peromyscus-data.rds")

# get table rows, to check for proper merging.
n_peros <- nrow(Peros)

# add in immune data
Peros <- left_join(Peros, immunity %>% select(-year), by = "uid")

# check that merges worked
stopifnot(nrow(Peros) == n_peros)

# add in immune PCA data
Peros <- left_join(Peros, ipca_list$site_scores, by = "uid")

# check that merges worked
stopifnot(nrow(Peros) == n_peros)

# add in climate data (used to build the PCA)
Peros <- left_join(Peros, cpca_list$data, by = "siteID")

# check that merges worked
stopifnot(nrow(Peros) == n_peros)

# add in climate PCA data
Peros <- left_join(Peros, cpca_list$site_scores, by = "siteID")

# check that merges worked
stopifnot(nrow(Peros) == n_peros)

# add in weather data (used to build PCAs)
Peros <- left_join(Peros, wpca_list$data, by = c("siteID", "date"))

# check that merges worked
stopifnot(nrow(Peros) == n_peros)

# add in weather PCA data
Peros <- left_join(Peros, wpca_list$site_scores, by = c("siteID", "date"))

# check that merges worked
stopifnot(nrow(Peros) == n_peros)

# add in capture timing
Peros <- left_join(Peros, cap_times, by = "uid")

## TODO: add to prepare-peromyscus-data.rds
# Add in a variable about capture time shift
Peros <- Peros %>%
  group_by(iid) %>%
  arrange(iid, cap_num) %>%
  mutate(capprop_shift = cap_prop_night - first(cap_prop_night)) %>%
  ungroup()

# check that merges worked
stopifnot(nrow(Peros) == n_peros)

# add in other behavior (and iid)
Peros <- left_join(Peros, behavior, by = "iid")

# check that merges worked
stopifnot(nrow(Peros) == n_peros)

# reorganize the data a bit:
# Peros <- Peros %>%
#   relocate(iid, .after = tagID)

## ---- Convert behaviors to PCA ----

trans_fun <- function(x){
  log(x + min(x) + 1)
}

bpca_dat <- Peros %>%
  select(
    iid, cap_prop_night, avg_move_dist,
    # trapability, unique_traps
    weighted_trapability, weighted_trap_diversity
  ) %>%
  group_by(iid) %>%
  mutate(avg_capprop_night = mean(cap_prop_night, na.rm = TRUE)) %>%
  select(-cap_prop_night) %>% ungroup() %>% distinct() %>%
  filter(complete.cases(.)) %>%
  tibble() %>%
  # mutate(across(-iid, ~as.vector(scale(.x)), .names = "scl_{.col}")) %>%
  mutate(across(-iid, ~as.vector(trans_fun(.x)), .names = "logtrans_{.col}")) %>%
  select(iid, starts_with("logtrans"))

bpca <- bpca_dat %>%
  select(-iid) %>%
  vegan::pca(scale = TRUE)

bpca_scores <- vegan::scores(bpca)

bpca_sp <- bpca_scores$species %>% data.frame() %>%
  rownames_to_column("scaled_behavior")

bpca_st <- bind_cols(
  bpca_dat,
  bpca_scores$sites %>% data.frame()
)

bpca_list <- list(
  site_scores = bpca_st %>%
    select(iid, behave_PC1 = PC1, behave_PC2 = PC2),
  species_scores = bpca_sp %>%
    rename(behave_PC1 = PC1, behave_PC2 = PC2),
  pca_obj = bpca,
  data = bpca_dat
)

saveRDS(bpca_list, "infection-modeling/data/peromyscus-behavior-pca.rds")

Peros <- left_join(
  Peros,
  bpca_st %>% select(iid, behave_PC1 = PC1, behave_PC2 = PC2),
  by = "iid"
)

# ordiplot(behave_pca)
# ordiplot(behave_pca, display = "species", type = "text", arrows = TRUE)
# scores(behave_pca)$species

## ---- Discriminant analysis (species reclassification) ----
## Added Oct 27, 2025

# update record so that all observations of an individual have the same genetic ID
Peros <- Peros %>%
  group_by(iid) %>%
  arrange(iid, is.na(Gel_Taxon)) %>%
  mutate(
    Gel_Taxon = first(Gel_Taxon), # genetically-derived species
  ) %>%
  ungroup()

# Check the accuracy of the field estimates against genetic estimates:
Peros %>%
  filter(!is.na(Gel_Taxon)) %>%
  mutate(agrees = Gel_Taxon == new_taxonID) %>%
  group_by(siteID) %>%
  mutate(n = sum(!is.na(agrees))) %>%
  group_by(Gel_Taxon, siteID, n) %>%
  summarize(
    accuracy = sum(agrees, na.rm = TRUE) / length(!is.na(agrees)),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = Gel_Taxon, values_from = accuracy)

# Fit some linear discriminant analyses (LDA)

## using ear and foot length
DFAfit_earFoot <- MASS::lda(
  Gel_Taxon ~ weight*earLength*hindfootLength*sex_male*sex_mature,
  data = Peros
)

## Predict to the full data set
earFoot_pred <- predict(DFAfit_earFoot, newdata = Peros)$posterior[, "PELE"]

## using ear length (no foot length)
DFAfit_ear <- MASS::lda(
  Gel_Taxon ~ weight*earLength*sex_male*sex_mature,
  data = Peros
)
ear_pred <- predict(DFAfit_ear, newdata = Peros)$posterior[, "PELE"]

## using just weight and sight probabilities
DFAfit_mass <- MASS::lda(
  Gel_Taxon ~ weight*sex_male*sex_mature*siteID,
  data = Peros
)
mass_pred <- predict(DFAfit_mass, newdata = Peros)$posterior[, "PELE"]

## Add them into Peros
Peros <- Peros %>%
  mutate(
    PELE_pred_earfoot = .env$earFoot_pred,
    PELE_pred_ear = .env$ear_pred,
    PELE_pred_weightsite = .env$mass_pred
  ) %>%
  group_by(iid) %>%
  mutate(
    avg_earfoot_PELEpred = mean(PELE_pred_earfoot, na.rm = TRUE),
    avg_ear_PELEpred = mean(PELE_pred_ear, na.rm = TRUE),
    avg_weightsite_PELEpred = mean(PELE_pred_weightsite, na.rm = TRUE)
  ) %>% ungroup()

# function to flag accuracte/inaccurate assignments
match_fun <- function(x, pred){
  case_when(
    x == "PELE" & pred >= 0.5 ~ TRUE,
    x == "PEMA" & pred < 0.5 ~ TRUE,
    x == "PELE" & pred < 0.5 ~ FALSE,
    x == "PEMA" & pred >= 0.5 ~ FALSE,
    .default = NA
  )
}

tmp <- Peros %>%
  filter(!is.na(Gel_Taxon)) %>%
  mutate(
    earfoot_correct = match_fun(Gel_Taxon, PELE_pred_earfoot),
    ear_correct = match_fun(Gel_Taxon, PELE_pred_ear),
    mass_correct = match_fun(Gel_Taxon, PELE_pred_weightsite),
    earfoot_correct_iid = match_fun(Gel_Taxon, avg_earfoot_PELEpred),
    ear_correct_iid = match_fun(Gel_Taxon, avg_ear_PELEpred),
    mass_correct_iid = match_fun(Gel_Taxon, avg_weightsite_PELEpred)
  )

tmp %>%
  group_by(siteID, Gel_Taxon) %>%
  summarize(
    earfoot_n = sum(!is.na(earfoot_correct)),
    ear_n =  sum(!is.na(ear_correct)),
    mass_n = sum(!is.na(mass_correct)),
    earfoot_obs = sum(earfoot_correct, na.rm = TRUE) / earfoot_n,
    ear_obs = sum(ear_correct, na.rm = TRUE) / ear_n,
    mass_obs = sum(mass_correct, na.rm = TRUE) / mass_n
  )

tmp %>%
  group_by(iid) %>%
  select(
    iid, Gel_Taxon, siteID, earfoot_correct_iid, ear_correct_iid,
    mass_correct_iid
  ) %>%
  distinct() %>%
  group_by(Gel_Taxon, siteID) %>%
  summarize(
    earfoot_n = sum(!is.na(earfoot_correct_iid)),
    ear_n =  sum(!is.na(ear_correct_iid)),
    mass_n = sum(!is.na(mass_correct_iid)),
    earfoot_obs = sum(earfoot_correct_iid, na.rm = TRUE) / earfoot_n,
    ear_obs = sum(ear_correct_iid, na.rm = TRUE) / ear_n,
    mass_obs = sum(mass_correct_iid, na.rm = TRUE) / mass_n
  )

# Perform the reassignments in a variable called "updated_taxa"
Peros <- Peros %>%
  mutate(
    updated_taxa = case_when(
      # 1st, defer to molecular genotype
      !is.na(Gel_Taxon) ~ Gel_Taxon,
      # 2nd, use ear and hindfood length (with weight, sex, maturity)
      !is.na(avg_earfoot_PELEpred) & avg_earfoot_PELEpred >= 0.5 ~ "PELE",
      !is.na(avg_earfoot_PELEpred) & avg_earfoot_PELEpred < 0.5 ~ "PEMA",
      # 3rd, use ear length (with weight, sex, maturity)
      !is.na(avg_ear_PELEpred) & avg_ear_PELEpred >= 0.5 ~ "PELE",
      !is.na(avg_ear_PELEpred) & avg_ear_PELEpred < 0.5 ~ "PEMA",
      # 4th, assign all individuals at at key sites as "PELE"
      siteID %in% c("BLAN", "HARV", "SERC", "ORNL") ~ "PELE",
      # remaining PELEPEMA/PESP should be considered unknown
      new_taxonID %in% c("PELEPEMA", "PESP") ~ NA,
      # 5th, use the (corrected) field-assigned individual values
      !is.na(new_taxonID) ~ new_taxonID,
      # 6th, assign remaining individuals as unknown/NA (n = 52)
      .default = NA
    )
  )

# proportion of samples that have been reclassified: 19%
Peros %>%
  group_by(iid) %>%
  summarise(
    agrees = first(new_taxonID) == first(updated_taxa), .groups = "drop"
  ) %>%
  ungroup() %>%
  summarize(
    reclass_prop = 1 - (sum(agrees, na.rm = TRUE) / sum(!is.na(agrees)))
  ) %>%
  unlist() %>% round(2)

## ---- Save the Model data ----

saveRDS(Peros, "infection-modeling/data/peromyscus-model-data.rds")

## ---- Create model directory ----

mod_output_dir <- file.path(
  "infection-modeling/data",
  "model-objects"
)

if (!dir.exists(mod_output_dir)) {
  dir.create(mod_output_dir, recursive = TRUE)
}
