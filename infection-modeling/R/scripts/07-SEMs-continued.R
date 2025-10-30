## ---- Load packages ----

library(tidyverse)
library(vegan)
library(lme4)
library(lmerTest)
library(piecewiseSEM)

## ---- Load and clean data ----

# get the model output directory, for saving files
mod_output_dir <- file.path(
  "infection-modeling/data",
  "model-objects"
)

# Load in the model data
Peros <- readRDS("infection-modeling/data/peromyscus-model-data.rds") %>%
  mutate(
    species = updated_taxa
  ) %>% filter(!is.na(species))


# get only the complete cases for variables to use
mod_dat <- Peros %>%
  mutate(
    across(c(siteID, plotID, year, iid, species), ~as.factor(.x)) # factor vars
  )

scaled_dat <- mod_dat %>%
  mutate(
    across(
      c(
        weight, cap_prop_night, weighted_trapability, weighted_trap_diversity,
        avg_move_dist
        ),
      ~as.vector(scale(.x))
    )
  )

# ---- Simplified SEM (no interactions) ----

## Weight
simple_weight <- lmer(
  weight ~ sex_male + sex_mature +
    wthr_PC1 +
    (1|year) + (1|plotID) + (1|species),
  data = scaled_dat
)

## Maturity
simple_mature <- glmer(
  sex_mature ~ sex_male +
    wthr_PC1 + clim_PC1 +
    (1|year) + (1|plotID) + (1|species),
  data = scaled_dat, family = "binomial"
)

## Capture time
simple_captime <- lmer(
  cap_prop_night ~ sex_male + sex_mature + weight +
    wthr_PC1 +
    (1|siteID) + (1|species),
  data = scaled_dat
)

## Capture time shift
simple_shift <- lmer(
  capprop_shift ~ sex_male + sex_mature + weight + cap_prop_night +
    (1|siteID) + (1|year) + (1|species),
  data = scaled_dat
)

## Tolerance
simple_tol <- lmer(
  expr_PC2 ~ sex_mature + Bb_infected + ticks_attached +
    (1|siteID) + (1|year) + (1|species),
  data = scaled_dat
)

## Burden (not used)
simple_burden <- lmer(
  log_burden ~ sex_male + sex_mature + weight +
    ticks_attached +
    capprop_shift +
    expr_PC1 +
    wthr_PC1 +
    (1|siteID) + (1|year) + (1|species),
  data = scaled_dat
)

## Infection
simple_inf <- glmer(
  Bb_infected ~ sex_male + sex_mature + weight + ticks_attached +
    capprop_shift +
    expr_PC1 +
    wthr_PC1 +
    (1|species) + (1|siteID) + (1|year) ,
  data = scaled_dat, family = "binomial"
)

## Resistance
simple_res <- lmer(
  expr_PC1 ~ sex_male + sex_mature + weight + ticks_attached +
    wthr_PC1 +
    (1|year) + (1|siteID) + (1|species),
  data = scaled_dat
)

## Ticks
simple_tick <- glmer(
  ticks_attached ~ sex_male + sex_mature + weight +
    capprop_shift +
    wthr_PC1 + clim_PC1 +
    (1|siteID) + (1|year) + (1|species),
  data = scaled_dat, family = "binomial"
)

# ---- Full simple SEM ----

## Full model
simple_sem <- psem(
  simple_tol,
  simple_inf,
  simple_res,
  simple_tick,
  simple_shift,
  simple_captime,
  simple_weight,
  simple_mature,
  data = scaled_dat
)
simple_out <- summary(simple_sem)

## save the results to a file
simple_model_objects <- list(
  component_mods = list(
    tolerance = simple_tol, infection = simple_inf, resistance = simple_res,
    ticks = simple_tick, captime_shift = simple_shift, captime = simple_captime,
    weight = simple_weight, maturity = simple_mature
  ),
  sem_mod = list(spec = simple_sem, fit = simple_out)
)
saveRDS(
  simple_model_objects,
  "infection-modeling/data/model-objects/simplified-SEM-objects.rds"
)

## reformat the tables
simple_out$dTable <- simple_out$dTable %>%
  data.frame() %>% rename(sig = "Var.6") %>% tibble()
simple_out$coefficients <- simple_out$coefficients %>%
  data.frame() %>% rename(sig = "Var.9") %>% tibble()

## Add terms from dsep tests
simple_out$dTable %>% filter(P.Value <= 0.5)

## ---- Full cumulative effects ----

## Calculate cumulative effects

source("infection-modeling/R/functions/diagram_psem.R")

frame_list <- frameplot_psem(simple_out$coefficients)
base_plot <- build_psem_plot(frame_list)

# build the table to fill
cumulative_tab <- expand_grid(
  Response = unique(frame_list$edges$Response),
  Predictor = unique(frame_list$edges$Predictor)
) %>%
  filter(Response != Predictor) %>%
  mutate(
    respID = frame_list$nodes$id[match(Response, frame_list$nodes$name)],
    predID = frame_list$nodes$id[match(Predictor, frame_list$nodes$name)]
  ) %>%
  left_join(
    select(frame_list$edges, Response, Predictor, Std.Estimate, P.Value),
    by = c("Response", "Predictor")
  ) %>%
  rename(direct = Std.Estimate)

# add a list of all the paths from the responses to the predictors
cumulative_tab <- cumulative_tab %>%
  rowwise() %>% # setup to loop through each row
  mutate(
    all_paths = list(unique(get_paths(base_plot, from = predID, to = respID))),
  )

# calculate direct and indirect effects
cumulative_tab <- cumulative_tab %>%
  mutate(
    all_coefs = list(lapply(
      all_paths,
      function(x){
        get_coefs_along_path(
          p = x, edges = frame_list$edges, nodes = frame_list$nodes
        )
      }
    )),
    coef_prods = list(lapply(all_coefs, prod)),
    is_direct_all = list(lapply(all_coefs, length) == 1),
    total = sum(unlist(coef_prods), na.rm = TRUE),
    direct_ind = list(lapply(is_direct_all, which) %>% unlist()),
    direct_ind = if_else(length(direct_ind[1]) == 0, NA, direct_ind[1]),
    new_direct = if_else(is.na(direct_ind), 0, all_coefs[[1]][direct_ind])
  )

cum_eff_tab <- cumulative_tab %>%
  unnest(new_direct) %>%
  ungroup() %>%
  mutate(
    new_direct = replace_na(new_direct, 0),
    indirect = total - new_direct
  ) %>%
  select(
    Response, Predictor, P.Value,  direct, new_direct, indirect, total
  )

saveRDS(
  cum_eff_tab,
  file = file.path(
    "infection-modeling/data/model-objects",
    "cumulative-effects-table_simplified-SEM.rds"
    )
)

## ---- Collect random effects ----

warning("Random effects collection has not yet been implemented.")

## ---- Species-specific models ----

## How many PEMA can we use?
Peros %>%
  filter(species == "PEMA") %>%
  select(
    siteID, year,
    Bb_infected, sex_male, sex_mature, weight, ticks_attached,
    wthr_PC1, clim_PC1
  ) %>%
  filter(complete.cases(.)) %>%
  nrow()

### Because there were very few PEMA observed at our 8 sites from 2020-2024 for
### which all variables were measured, we further simplified species-specific
### models to only include infection status, ticks, weight, sex, reproductive
### status, weather, and climate. This left us with only 52 PEMA observations.
### We fit the most complex model that we could for PEMA and then parameterized
### that same model for the PELE data for direct comparison.

## Infection
pema_simple_inf <- simple_inf %>%
  update(
    . ~ sex_male + sex_mature + weight + ticks_attached +
      wthr_PC1 + #clim_PC1 + climate significant in neither
      (1|siteID) + (1|year),
    data = scaled_dat %>% filter(species == "PEMA")
  )
pele_simple_inf <- simple_inf %>%
  update(
    . ~ sex_male + sex_mature + weight + ticks_attached +
      wthr_PC1 + #clim_PC1 +
      (1|siteID) + (1|year),
    data = scaled_dat %>% filter(species == "PELE")
  )

## Ticks
pema_simple_tick <- simple_tick %>%
  update(
    . ~ sex_male + sex_mature + weight +
      wthr_PC1 + clim_PC1 +
      (1|siteID) + (1|year),
    data = scaled_dat %>% filter(species == "PEMA")
  )
pele_simple_tick <- simple_tick %>%
  update(
    . ~ sex_male + sex_mature + weight +
      wthr_PC1 + clim_PC1 +
      (1|siteID) + (1|year),
    data = scaled_dat %>% filter(species == "PELE")
  )

## Weight
pema_simple_weight <- simple_weight %>%
  update(
    . ~ sex_male + sex_mature +
      wthr_PC1 + #clim_PC1 +
      (1|siteID) + (1|year),
    data = scaled_dat %>% filter(species == "PEMA")
  )
pele_simple_weight <- simple_weight %>%
  update(
    . ~ sex_male + sex_mature +
      wthr_PC1 + #clim_PC1 +
      (1|siteID) + (1|year),
    data = scaled_dat %>% filter(species == "PELE")
  )

## Reproductive status
pema_simple_mature <- simple_mature %>%
  update(
    . ~ sex_male +
      wthr_PC1 + #clim_PC1 +
      (1|siteID) + (1|year),
    data = scaled_dat %>% filter(species == "PEMA")
  )
pele_simple_mature <- simple_mature %>%
  update(
    . ~ sex_male +
      wthr_PC1 + #clim_PC1 +
      (1|siteID) + (1|year),
    data = scaled_dat %>% filter(species == "PELE")
  )

## ---- Species SEM ----

## PEMA
pema_sem <- psem(
  pema_simple_inf,
  pema_simple_tick,
  pema_simple_weight,
  pema_simple_mature,
  data = scaled_dat %>% filter(species == "PEMA")
)
pema_out <- summary(pema_sem)

## PELE
pele_sem <- psem(
  pele_simple_inf,
  pele_simple_tick,
  pele_simple_weight,
  pele_simple_mature,
  data = scaled_dat %>% filter(species == "PELE")
)
pele_out <- summary(pele_sem)

## ---- Sp. model comparison ----

## likelihood ratio test (flawed due to convergence)
sp_sem_compare <- anova(pele_sem, pema_sem)

## join the results
spec_coefs <- bind_rows(
  pema_out$coefficients %>% data.frame() %>% mutate(species = "PEMA"),
  pele_out$coefficients %>% data.frame() %>% mutate(species = "PELE"),
)

## get critical value for z-test
alpha = 0.1
crit_prob <- 1 - (alpha/2)
z_crit <- qnorm(crit_prob)

## Determine whether 90% confidence intervals (unstandardized) overlap by
### species for each coefficient...
spec_coefs <- spec_coefs %>% arrange(Response, Predictor, species) %>%
  group_by(Response, Predictor) %>%
  mutate(
    larger = which.max(Estimate),
    smaller = which.min(Estimate),
    upper_low = Estimate[unique(larger)] - (z_crit*Std.Error[unique(larger)]),
    lower_high = Estimate[unique(smaller)] + (z_crit*Std.Error[unique(smaller)]),
    CI_overlap = upper_low < lower_high,
  ) %>%
  select(
    -larger, -smaller
  )

## Count the coefficients that do and do not differ
spec_coefs %>% group_by(CI_overlap) %>% tally() %>% mutate(n = n/2)

## ---- Species cumulative effects ----
