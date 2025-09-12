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

bpca_scores <- scores(bpca)

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
