# This script closely follows that of "08-bootstrap_SEMs.R" but for the 
## 2026 re-analyses that include only PELE.

## ---- Pacakges ----
library(tidyverse)
library(optparse)
library(semEff)
library(lme4)
library(lmerTest)

## ---- Parser ----

# Construct the argument parser, with one optional flag
parser <- OptionParser()
parser <- add_option(
  parser, c("-f", "--force"),
  action = "store_true",
  default = FALSE,
  dest = "force",
  help = "Force long-running code to execute [FALSE]"
)
parser <- add_option(
  parser, c("-n", "--ncores"),
  default = 3,
  dest = "ncores",
  help = "Number of (logical) cores to execute on"
)
opt <- parse_args(parser)

## ---- Parameters ----
# pass the value of the force argument to the variable
force_run = opt$force
ncores = opt$ncores

message(paste('ncores =', ncores))

## ---- Setup ----

# Load PELE data
Peros <- readRDS("infection-modeling/data/peromyscus-model-data.rds") %>%
  mutate(species = updated_taxa) %>%
  filter(!is.na(species)) %>%
  mutate(
    across(c(siteID, plotID, year, iid, species), ~as.factor(.x)) # factor vars
  ) %>% 
  filter(species == "PELE")

sem_objects <- readRDS(
  "infection-modeling/data/model-objects/pele_2026_full-sem-objects.rds"
)

modlist = sem_objects$component_mods
modlist = lapply(modlist, function(fm){update(fm, data = Peros)})
modlist[["captime"]] = modlist[["captime"]] %>% update(. ~ . + (1|year))
semfit = sem_objects$sem_mod$fit

## ---- Bootstrap ----

pele_boot_path = file.path(
  "infection-modeling/data/model-objects",
  "pele-sem_2026_bootstrap.rds"
)

if (force_run || !file.exists(pele_boot_path)) {
  boot_time <- system.time(
    full_boot <- bootEff(
      unname(modlist), R = 2000, type = "parametric", 
      std.x = FALSE, std.y = FALSE, parallel = "snow", ncpus = ncores
    )
  )
  mins = try(boot_time["elapsed"]/60)
  message(paste(mins, "minutes elapsed"))
  saveRDS(full_boot, file = pele_boot_path)
} else {
  full_boot <- readRDS(pele_boot_path)
}

## ---- Standardize the effects ----

# responses
sdResp_list <- lapply(
  modlist, function(fm){
    piecewiseSEM:::GetSDy(model = fm, data = model.frame(fm))
  }
)

# predictors
sdPred_list <- lapply(
  modlist, function(fm){
    piecewiseSEM:::GetSDx(model = fm, data = model.frame(fm))
  }
)

# look at the response sds
sdResp_list %>% bind_rows()
# look at the predictor sd - slightly different across mods due to sample size
sdPred_list %>% bind_rows() %>% 
  mutate(response = names(sdPred_list)) %>% 
  relocate(response, .before = 0)

# manually standardize the bootstrap results:
for (resp.no in seq_len(length(modlist))) {
  # sd of this response
  sdResp = sdResp_list[[resp.no]]
  
  for (pred in names(sdPred_list[[resp.no]])) { # predictor names
    
    # sd of this predictor
    sdPred = sdPred_list[[resp.no]][pred]
    
    # scale the bootstrapped coefficients by these values
    full_boot[[resp.no]]$t[, pred] <-
      full_boot[[resp.no]]$t[, pred] * (sdPred / sdResp)
    
    # rescale the mean boostrapped coefficient
    full_boot[[resp.no]]$t0[pred] <-
      full_boot[[resp.no]]$t0[pred] * (sdPred / sdResp)
  }
}

# save the standardized version of the bootstrap
saveRDS(
  full_boot,
  file.path(
    "infection-modeling/data/model-objects"
    ,paste0("rescaled_", basename(pele_boot_path))
  )
)

# calculate the effect coefficients, with 90% conf. interval
full_effs <- semEff(full_boot, ci.conf = 0.9)

# write effects table (including indirect effect breakdown) for each mod

sink("infection-modeling/data/raw-PELE-effects_decomp.txt")
for (i in seq_len(length(full_effs$Summary))) {
  if (i == 1) {
    cat("Variables table:\n")
  } else {
    cat("Response:", names(full_effs$Summary)[i], "\n")
  }
  print(full_effs$Summary[[i]])
  cat("\n\n") # new line
}
sink()

warning("NEED TO CHECK THE CONF INTS")

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
full_stats <- semfit$coefficients %>%
  data.frame() %>% rename(sigstar = Var.9) %>% tibble() %>%
  mutate(effect_type = "direct") %>%
  relocate(effect_type, .after = "Predictor") %>%
  full_join(full_effs_tab) %>%
  # group_by(Response, Predictor, effect_type) %>%
  rowwise() %>%
  mutate(
    boot.z = boot.eff / boot.SE,
    boot.p = min(pnorm(boot.z), pnorm(boot.z, lower.tail = FALSE)) * 2,
    boot.eff.mean = (boot.ci.up + boot.ci.low) / 2 ,
    boot.p.mean = boot.eff.mean / boot.SE,
    boot.p.mean = min(
      pnorm(boot.p.mean), pnorm(boot.p.mean, lower.tail = FALSE)
    ) * 2,
  ) %>%
  ungroup()

# save the stats
saveRDS(
  full_stats,
  "infection-modeling/data/model-objects/pele-SEM-2026_boot-statistics-all.rds"
)
