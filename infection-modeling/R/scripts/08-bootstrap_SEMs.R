## ---- Parameters ----
force_run = FALSE

## ---- Load Packages ----
library(tidyverse)
library(lme4)
library(lmerTest)
library(piecewiseSEM)
library(semEff)

## ---- Load objects ----

# Load in the model data
Peros <- readRDS("infection-modeling/data/peromyscus-model-data.rds") %>%
  mutate(
    species = updated_taxa
  ) %>% filter(!is.na(species))

# Load in cleaned model data
model_data_list <- readRDS(
  file = "infection-modeling/data/model-objects/peromyscus-model-dataframes.rds"
)

# Load in SEM objects
simple_model_objects <- readRDS(
  "infection-modeling/data/model-objects/simplified-SEM-objects.rds"
)

## ---- Bootstraps ----

modlist <- simple_model_objects$component_mods

# refit the model to the completely unscaled data
if (force_run || !exists("unscaled_modlist") | !exists("unscaled_psem.fit")) {
  unscaled_modlist <- lapply(
    modlist, function(x) update(x, data = model_data_list$mod_data)
  )

  unscaled_psem.spec <- as.psem(unscaled_modlist)
  unscaled_psem.fit <- summary(unscaled_psem.spec)
}



full_boot_file <- "infection-modeling/data/model-objects/bootSEM_full.rds"

# Bootstrap the model
if (force_run || !file.exists(full_boot_file)) {
  boot_time <- system.time(
    full_boot <- bootEff(
      unscaled_modlist, R = 1000,
      # ran.eff = "siteID",
      type = "parametric",
      std.x = FALSE, std.y = FALSE,
      parallel = "snow", ncpus = 12,
      #bM.arg = list(".progress" = "txt", "PBargs" = list(style=3))
    )
  )

  cat(boot_time["elapsed"]/60, "minutes") 
  # cat(boot_time*100/60/60, "estimated hours") # estimated hours to run...
  ## nonparametric = 164.19 seconds (R=10, ncpus=12)
  ## parametric =  292.24 seconds (R=10, ncpus=12)

  saveRDS(full_boot, file = full_boot_file)
} else {
  full_boot <- readRDS(file = full_boot_file)
}

## Standardize the effects
# TODO

## Summarize the effects
full_effs <- semEff(full_boot, ci.conf = 0.9)
full_smry <- summary(full_effs)
