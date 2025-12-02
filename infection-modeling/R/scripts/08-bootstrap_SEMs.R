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
unscaled_psem_file <- file.path(
  "infection-modeling/data/model-objects",
  "unscaled-peros_SEM-objects.rds"
)

if (force_run || !file.exists(unscaled_psem_file)) {
  unscaled_modlist <- lapply(
    modlist, function(x) update(x, data = model_data_list$mod_data)
  )

  unscaled_psem.spec <- as.psem(unscaled_modlist)
  unscaled_psem.fit <- summary(unscaled_psem.spec)

  out_list <- list(
    component_mods = unscaled_modlist,
    sem_mod = list(spec = unscaled_psem.spec, fit = unscaled_psem.fit)
  )
  saveRDS(out_list, unscaled_psem_file)
  rm(out_list)
} else {
  out_list <- readRDS(unscaled_psem_file)
  unscaled_modlist <- out_list$component_mods
  unscaled_psem.spec <- out_list$sem_mod$spec
  unscaled_psem.fit <- out_list$sem_mod$fit
  rm(out_list)
}



full_boot_file <- "infection-modeling/data/model-objects/bootSEM_full.rds"

# Bootstrap the model
if (force_run || !file.exists(full_boot_file)) {
  boot_time <- system.time(
    full_boot <- bootEff(
      unname(unscaled_modlist), R = 1000,
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
  ## NOTE: warnings about convergence only apply to the boostrapped models
  ## and aren't worrisome.
}

# copy the boot object
full_boot_copy <- full_boot

## Standardize the effects
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
for (resp.no in seq_len(length(unscaled_modlist))) { # loop through responses
  resp.name = names(unscaled_modlist)[resp.no]
  # sd of this response
  sdResp = sdResp_list[[resp.no]]

  for (pred in names(sdPred_list[[resp.no]])) { # loop through predictors
    # sd of this predictor
    sdPred = sdPred_list[[resp.no]][pred]
    # rescale the bootstrapped coefficients
    full_boot[[resp.no]]$t[, pred] <- full_boot[[resp.no]]$t[, pred] * (sdPred / sdResp)
    # rescale the mean boostrapped coefficient
    full_boot[[resp.no]]$t0[pred] <- full_boot[[resp.no]]$t0[pred] * (sdPred / sdResp)
  }
}

## Summarize the effects
full_effs <- semEff(full_boot, ci.conf = 0.9)
## get a table of effects and clean up names
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

## Calculate empirical P-value
# TODO
# ... This should be the proportion of samples less than 0 (for positive effects) times 2

full_stats <- unscaled_psem.fit$coefficients %>%
  data.frame() %>% rename(sigstar = Var.9) %>% tibble() %>%
  mutate(effect_type = "direct") %>%
  relocate(effect_type, .after = "Predictor") %>%
  full_join(full_effs_tab)

# Plot the descrepancies between the two methods
## filled grey points are those with significant P-values but insignificant boostrapped CIs
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
  theme(
    strip.background = element_blank(),
    # legend.position = "inside", legend.position.inside = c(1, 0),
    # legend.justification = c(1, 0)
  )

# Look at the bias
full_stats %>% filter(effect_type == "direct") %>%
  ggplot(
    aes(
      y = Predictor, x = boot.bias,
    )
  ) + facet_wrap(~Response, scales = "free_x") +
  geom_col(fill = "grey80", color = "black") +
  geom_vline(xintercept = 0) +
  labs(x = "Bias") +
  theme_bw() +
  theme(
    strip.background = element_blank(),
  )

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

### Compare scaled vs unscaled effects

def <- stdEff(unscaled_modlist)
uns <- stdEff(unscaled_modlist, std.x = FALSE, std.y = FALSE)
nonu <- stdEff(unscaled_modlist, std.x = FALSE, std.y = FALSE,
               unique.eff = FALSE) # this is different.


## ---- Species bootstraps ----
