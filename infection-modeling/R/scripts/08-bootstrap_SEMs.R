## ---- Load Packages ----
library(optparse)
library(tidyverse)
library(lme4)
library(lmerTest)
library(piecewiseSEM)
library(semEff)

## ---- Parse arguments ----
parser <- OptionParser()
parser <- add_option(
  parser, c("-f", "--force"),
  action = "store_true",
  default = FALSE,
  dest = "force",
  help = "Force long-running code to execute [FALSE]"
)
opt <- parse_args(parser)

## ---- Parameters ----
force_run = opt$force

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

# path to save the unscaled model fit
unscaled_psem_file <- file.path(
  "infection-modeling/data/model-objects",
  "unscaled-peros_SEM-objects.rds"
)

# refit the model to the completely unscaled data
if (force_run || !file.exists(unscaled_psem_file)) {

  # refit with unscaled data
  unscaled_modlist <- lapply(
    modlist, function(x) update(x, data = model_data_list$mod_data)
  )

  # fit the psem
  unscaled_psem.spec <- as.psem(unscaled_modlist)
  unscaled_psem.fit <- summary(unscaled_psem.spec)

  # collect output
  out_list <- list(
    component_mods = unscaled_modlist,
    sem_mod = list(spec = unscaled_psem.spec, fit = unscaled_psem.fit)
  )

  # save it
  saveRDS(out_list, unscaled_psem_file)

  # remove the list copy
  rm(out_list)
} else {

  # load the data list
  out_list <- readRDS(unscaled_psem_file)

  # unpack the list into individual objects
  unscaled_modlist <- out_list$component_mods
  unscaled_psem.spec <- out_list$sem_mod$spec
  unscaled_psem.fit <- out_list$sem_mod$fit

  # remove the list copy
  rm(out_list)
}

# path to save the SEM boot object
full_boot_file <- "infection-modeling/data/model-objects/bootSEM_full.rds"

# Bootstrap the model, *without* automatic standardization (std.x, std.y)
if (force_run || !file.exists(full_boot_file)) {

  # save (and time) the bootstrap
  boot_time <- system.time(
    full_boot <- bootEff(
      unname(unscaled_modlist), R = 2000, type = "parametric",
      std.x = FALSE, std.y = FALSE, parallel = "snow", ncpus = 12
    )
  )

  # print a message about how long it took
  cat(boot_time["elapsed"]/60, "minutes") # ~1 hour for R=1000, ncpus=12

  # save it
  saveRDS(full_boot, file = full_boot_file)
} else {

  # load it
  full_boot <- readRDS(file = full_boot_file)
    ## NOTE: warnings about convergence only apply to the bootstrapped
    ## models and aren't worrisome.
}

# copy the boot object as a backup (we'll be changing the original)
full_boot_copy <- full_boot

## ---- Standardize the effects ----

# Get response standardizations
sdResp_list <- lapply(
  unscaled_modlist, function(fm){
    piecewiseSEM:::GetSDy(model = fm, data = model_data_list$mod_data)
  }
)

# Get predictor standardizations
sdPred_list <- lapply(
  unscaled_modlist, function(fm){
    piecewiseSEM:::GetSDx(model = fm, data = model_data_list$mod_data)
  }
)

# Manually standardize the bootstrap results (in place)
for (resp.no in seq_len(length(unscaled_modlist))) { # response numbers

  # sd of this response
  sdResp = sdResp_list[[resp.no]]

  for (pred in names(sdPred_list[[resp.no]])) { # predictor names

    # sd of this predictor
    sdPred = sdPred_list[[resp.no]][pred]

    # scale the bootstrapped coefficients by tese values
    full_boot[[resp.no]]$t[, pred] <-
      full_boot[[resp.no]]$t[, pred] * (sdPred / sdResp)

    # rescale the mean boostrapped coefficient
    full_boot[[resp.no]]$t0[pred] <-
      full_boot[[resp.no]]$t0[pred] * (sdPred / sdResp)
  }
}

# calculate the effect coefficients, with 90% conf. interval
full_effs <- semEff(full_boot, ci.conf = 0.9)

# get a table of effects, clean up names, and rearrange
full_effs_tab <- getEffTable(full_effs) %>% tibble() %>%
  rename(
    Response = response, Predictor = predictor, boot.eff = effect,
    boot.bias = bias, boot.SE = std_err, boot.ci.low = lower_ci,
    boot.ci.up = upper_ci
  ) %>%
  relocate(Predictor, .after = Response) %>%
  mutate(
    Response = gsub("\\.", "_", Response),
    Predictor = gsub("\\.", "_", Predictor)
  )

# Calculate empirical P-value
empirical_pvals <- lapply(
  full_boot, function(x) {
    pos_dist <- apply(x$t, 2, function(y) y >= 0)
    pos_prop <- apply(pos_dist, 2, function(y) mean(y, na.rm = TRUE))
    one_sided <- if_else(pos_prop >= 0.5, (1 - pos_prop), pos_prop)
    two_sided <- one_sided * 2
    two_sided
  }
)
## TODO: I don't know how to calculate empirical Pvals for indirect/total effs.

# Extract join the bootstrapped results with the psem coefficient table
full_stats <- unscaled_psem.fit$coefficients %>%
  data.frame() %>% rename(sigstar = Var.9) %>% tibble() %>%
  mutate(effect_type = "direct") %>%
  relocate(effect_type, .after = "Predictor") %>%
  full_join(full_effs_tab) %>%
  # group_by(Response, Predictor, effect_type) %>%
  rowwise() %>%
  mutate(
    boot.z = boot.eff / boot.SE,
    boot.p = min(pnorm(boot.z), pnorm(boot.z, lower.tail = FALSE)) * 2
  ) %>%
  ungroup()

# Plot the discrepancies between psem P-values and confidence intervals
full_stats %>% filter(effect_type == "direct") %>%
  ggplot(
    aes(
      y = Predictor, x = boot.eff, xmin = boot.ci.low, xmax = boot.ci.up,
      shape = P.Value <= 0.1, color = boot.ci.low<0&boot.ci.up>0
    )
  ) + facet_wrap(~Response, scales = "free_x") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_pointrange(fill = "white") +
  scale_shape_manual(values = c(21, 16)) +
  scale_color_manual(values = c("black", "grey50")) +
  labs(color = "CI overlaps 0", shape = "P < 0.1", x = "Std. Effect") +
  theme_bw() +
  theme(strip.background = element_blank())
  ## Note that the differences are likely to come from two sources:
    ## 1. The P-values come from non-standardized effects (test != 0)
    ## while CIs come from standardized effects (test != avg).
    ## 2. semEff calculates *unique* effects that account for colinearity.

## ---- Compare and visualize effects ----

# Plot the biases
full_stats %>% filter(effect_type == "direct") %>%
  ggplot(aes(y = Predictor, x = boot.bias)) +
  facet_wrap(~Response, scales = "free_x") +
  geom_col(fill = "grey80", color = "black") +
  geom_vline(xintercept = 0) +
  labs(x = "Bias") +
  theme_bw() +
  theme(strip.background = element_blank())

# Compare psem effects with "unique" semEff effects
full_stats %>% filter(effect_type == "direct") %>%
  ggplot(aes(y = Predictor)) +
  facet_wrap(~Response, scales = "free_x") +
  geom_col(
    aes(x = Std.Estimate, fill = "psem"), col = "black",
    position = position_nudge(y = 0.1), width = 0.5
  ) +
  geom_col(
    aes(x = boot.eff, fill = "semEff"), col = "black",
    position = position_nudge(y = -0.1), width = 0.5
  )

# save the stats
saveRDS(
  full_stats,
  "infection-modeling/data/model-objects/SEM-statistics-all.rds"
)

# # Compare scaled vs unscaled effects
#
# def <- stdEff(unscaled_modlist)
# uns <- stdEff(unscaled_modlist, std.x = FALSE, std.y = FALSE)
# nonu <- stdEff(unscaled_modlist, std.x = FALSE, std.y = FALSE,
#                unique.eff = FALSE) # this is different.

## ---- Species bootstraps ----

# Load original models
pele_mod_obs <- readRDS(
  "infection-modeling/data/model-objects/PELE-SEM-objects.rds"
)
pema_mod_obs <- readRDS(
  "infection-modeling/data/model-objects/PEMA-SEM-objects.rds"
)

# filter the unscaled data
pele_data <- model_data_list$mod_data %>% filter(species == "PELE")
pema_data <- model_data_list$mod_data %>% filter(species == "PEMA")

# extract the model lists
pele_modlist <- pele_mod_obs$component_mods
pema_modlist <- pema_mod_obs$component_mods

# path to save the unscaled model fit
unscaled_species_file <- file.path(
  "infection-modeling/data/model-objects",
  "unscaled-species_SEM-objects.rds"
)

# refit the species models to the completely unscaled data
if (force_run || !file.exists(unscaled_species_file)) {

  # refit with unscaled data
  pele_modlist <- lapply(
    pele_modlist, function(x) update(x, data = pele_data)
  )
  pema_modlist <- lapply(
    pema_modlist, function(x) update(x, data = pema_data)
  )

  # psem specifications
  pele_spec <- as.psem(pele_modlist)
  pema_spec <- as.psem(pema_modlist)

  # psem fits
  pele_psem <- summary(pele_spec)
  pema_psem <- summary(pema_spec)

  # collect output
  out_list <- list(
    pele = list(
      component_mods = pele_modlist,
      sem_mod = list(spec = pele_spec, fit = pele_psem)
    ),
    pema = list(
      component_mods = pema_modlist,
      sem_mod = list(spec = pema_spec, fit = pema_psem)
    )
  )

  # save it
  saveRDS(out_list, unscaled_species_file)

  # remove the list copy
  rm(out_list)
} else {

  # load the data list
  out_list <- readRDS(unscaled_species_file)

  # unpack the list into individual objects
  pele_modlist <- out_list$pele$component_mods
  pema_modlist <- out_list$pema$component_mods
  pele_spec <- out_list$pele$sem_mod$spec
  pema_spec <- out_list$pema$sem_mod$spec
  pele_psem <- out_list$pele$sem_mod$fit
  pema_psem <- out_list$pema$sem_mod$fit

  # remove the list copy
  rm(out_list)
}

# paths to save the boot objects
pele_boot_file <- file.path(
  "infection-modeling/data/model-objects",
  "bootSEM_PELE.rds"
)
pema_boot_file <- file.path(
  "infection-modeling/data/model-objects",
  "bootSEM_PEMA.rds"
)

# Bootstrap the pele model
if (force_run || !file.exists(pele_boot_file)) {
  boot_time <- system.time(
    pele_boot <- bootEff(
      unname(pele_modlist), R = 2000, type = "parametric",
      std.x = FALSE, std.y = FALSE, parallel = "snow", ncpus = 12
    )
  )
  cat(boot_time["elapsed"]/60, "minutes") # ~1 hour for R=1000, ncpus=12
  saveRDS(pele_boot, file = pele_boot_file)
} else {
  pele_boot <- readRDS(file = pele_boot_file)
}

# Boostrap the pema model
if (force_run || !file.exists(pema_boot_file)) {
  boot_time <- system.time(
    pema_boot <- bootEff(
      unname(pema_modlist), R = 2000, type = "parametric",
      std.x = FALSE, std.y = FALSE, parallel = "snow", ncpus = 12
    )
  )
  cat(boot_time["elapsed"]/60, "minutes") # ~1 hour for R=1000, ncpus=12
  saveRDS(pema_boot, file = pema_boot_file)
} else {
  pema_boot <- readRDS(file = pema_boot_file)
}
## ---- Species standardization ----

# Get response standardizations
pele_sdResp <- lapply(
  pele_modlist, function(fm){
    piecewiseSEM:::GetSDy(model = fm, data = pele_data)
  }
)
pema_sdResp <- lapply(
  pema_modlist, function(fm){
    piecewiseSEM:::GetSDy(model = fm, data = pema_data)
  }
)

# Get predictor standardizations
pele_sdPred <- lapply(
  pele_modlist, function(fm){
    piecewiseSEM:::GetSDx(model = fm, data = pele_data)
  }
)
pema_sdPred <- lapply(
  pema_modlist, function(fm){
    piecewiseSEM:::GetSDx(model = fm, data = pema_data)
  }
)

# standardize the PELE bootstrap
for (resp.no in seq_len(length(pele_modlist))) {
  sdResp = pele_sdResp[[resp.no]]
  for (pred in names(pele_sdPred[[resp.no]])) {
    sdPred = pele_sdPred[[resp.no]][pred]
    pele_boot[[resp.no]]$t[, pred] <-
      pele_boot[[resp.no]]$t[, pred] * (sdPred / sdResp)
    pele_boot[[resp.no]]$t0[pred] <-
      pele_boot[[resp.no]]$t0[pred] * (sdPred / sdResp)
  }
}

# standardize the PEMA bootstrap
for (resp.no in seq_len(length(pema_modlist))) {
  sdResp = pema_sdResp[[resp.no]]
  for (pred in names(pema_sdPred[[resp.no]])) {
    sdPred = pema_sdPred[[resp.no]][pred]
    pema_boot[[resp.no]]$t[, pred] <-
      pema_boot[[resp.no]]$t[, pred] * (sdPred / sdResp)
    pema_boot[[resp.no]]$t0[pred] <-
      pema_boot[[resp.no]]$t0[pred] * (sdPred / sdResp)
  }
}

# calculate the effect coefficients, with 90% conf. interval
pele_effs <- semEff(pele_boot, ci.conf = 0.9)
pema_effs <- semEff(pema_boot, ci.conf = 0.9)

# PELE effects table
pele_effs_tab <- getEffTable(pele_effs) %>% tibble() %>%
  rename(
    Response = response, Predictor = predictor, boot.eff = effect,
    boot.bias = bias, boot.SE = std_err, boot.ci.low = lower_ci,
    boot.ci.up = upper_ci
  ) %>%
  mutate(
    Response = gsub("\\.", "_", Response),
    Predictor = gsub("\\.", "_", Predictor),
    Species = "PELE"
  ) %>%
  relocate(Predictor, .after = Response) %>%
  relocate(Species, .before = 0) %>%
  full_join(
    pele_psem$coefficients %>% data.frame() %>% rename(sigstar = Var.9) %>%
      tibble() %>% mutate(effect_type = "direct", Species = "PELE") %>%
      relocate(effect_type, .after = "Predictor") %>%
      relocate(Species, .before = 0),
    by = c("Species", "Response", "Predictor", "effect_type")
  )

# PEMA effects table
pema_effs_tab <- getEffTable(pema_effs) %>% tibble() %>%
  rename(
    Response = response, Predictor = predictor, boot.eff = effect,
    boot.bias = bias, boot.SE = std_err, boot.ci.low = lower_ci,
    boot.ci.up = upper_ci
  ) %>%
  mutate(
    Response = gsub("\\.", "_", Response),
    Predictor = gsub("\\.", "_", Predictor),
    Species = "PEMA"
  ) %>%
  relocate(Predictor, .after = Response) %>%
  relocate(Species, .before = 0) %>%
  full_join(
    pema_psem$coefficients %>% data.frame() %>% rename(sigstar = Var.9) %>%
      tibble() %>% mutate(effect_type = "direct", Species = "PEMA") %>%
      relocate(effect_type, .after = "Predictor") %>%
      relocate(Species, .before = 0),
    by = c("Species", "Response", "Predictor", "effect_type")
  )

# combine them into one table
species_effs_tab <- full_join(pele_effs_tab, pema_effs_tab)

# save the stats
saveRDS(
  species_effs_tab,
  "infection-modeling/data/model-objects/SEM-statistics-species.rds"
)
