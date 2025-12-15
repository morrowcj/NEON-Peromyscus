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

saveRDS(
  list(mod_data = mod_dat, scaled_dat = scaled_dat),
  file = "infection-modeling/data/model-objects/peromyscus-model-dataframes.rds"
)

# ---- Simplified SEM (no interactions) ----

## Weight
simple_weight <- lmer(
  weight ~ sex_male + sex_mature +
    wthr_PC1 +
    # (1|year) + (1|plotID) + (1|species),
    (1|year) + (1|siteID) + (1|species),
  data = scaled_dat
)

## Maturity
simple_mature <- glmer(
  sex_mature ~ sex_male +
    wthr_PC1 + clim_PC1 +
    # (1|year) + (1|plotID) + (1|species),
    (1|year) + (1|siteID) + (1|species),
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

simple_modlist = list(
  tolerance = simple_tol, infection = simple_inf, resistance = simple_res,
  ticks = simple_tick, captime_shift = simple_shift, captime = simple_captime,
  weight = simple_weight, maturity = simple_mature
)

## Full model
simple_sem <- as.psem(
  simple_modlist
)
simple_out <- summary(simple_sem)

## save the results to a file
simple_model_objects <- list(
  component_mods = simple_modlist,
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

frame_list <- build_nodes_edges(simple_out$coefficients)
base_plot <- build_psem_plot(frame_list)

# build the table to fill
cumulative_tab <- expand_grid(
  To = unique(frame_list$edges$To),
  From = unique(frame_list$edges$From)
) %>%
  filter(To != From) %>%
  mutate(
    respID = frame_list$nodes$id[match(To, frame_list$nodes$name)],
    predID = frame_list$nodes$id[match(From, frame_list$nodes$name)]
  ) %>%
  left_join(
    select(frame_list$edges, To, From, Val, P.Value),
    by = c("To", "From")
  ) %>%
  rename(direct = Val)

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

# clean up
cum_eff_tab <- cumulative_tab %>%
  unnest(new_direct) %>%
  ungroup() %>%
  mutate(
    new_direct = replace_na(new_direct, 0),
    indirect = total - new_direct
  ) %>%
  select(
    To, From, P.Value,  direct, new_direct, indirect, total
  )

saveRDS(
  cum_eff_tab,
  file = file.path(
    "infection-modeling/data/model-objects",
    "cumulative-effects-table_simplified-SEM.rds"
    )
)

## ---- Use the semEff package to bootstrap ----
# library(semEff)
# ## TODO
# ## This takes a long time to run
# run_bootstrap = TRUE
# if (run_bootstrap){
#   simple_boot <- bootEff(
#     simple_modlist, R = 10, type = 'parametric',
#     parallel = "snow", ncpus = parallel::detectCores() - 4
#     # .progress = "txt"
#   )
#
#   simple_effs <- semEff(simple_boot)
# }
#
# dir_effs <- semEff::getDirEff(simple_effs)
#
# tmp_boot = bootMer(
#   simple_modlist$weight,
#   FUN = function(x){
#     fixef(x)
#   }
# )
# summary(tmp_boot)
#
# tmp_effs <- stdEff(
#   simple_modlist, unique.eff = TRUE,
#   incl.raw = FALSE, refit.x = TRUE,
#   cen.x = TRUE, std.x = TRUE,
#   cen.y = TRUE, std.y = TRUE
# )
# tmp_effs$weight
#
# # Not the same...
# simple_model_objects$sem_mod$fit$coefficients %>%
#   data.frame() %>% filter(Response == "weight")
#
# fixef(simple_modlist$infection)
#
# tmp_effs

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

## Save the output
pele_model_objects <- list(
  component_mods = list(
    infection = pele_simple_inf,
    ticks = pele_simple_tick,
    weight = pele_simple_weight,
    maturity = pele_simple_mature
  ),
  sem_mod = list(
    spec = pele_sem,
    fit = pele_out
  )
)

saveRDS(
  pele_model_objects,
  "infection-modeling/data/model-objects/PELE-SEM-objects.rds"
)

pema_model_objects <- list(
  component_mods = list(
    infection = pema_simple_inf,
    ticks = pema_simple_tick,
    weight = pema_simple_weight,
    maturity = pema_simple_mature
  ),
  sem_mod = list(
    spec = pema_sem,
    fit = pema_out
  )
)

saveRDS(
  pema_model_objects,
  "infection-modeling/data/model-objects/PEMA-SEM-objects.rds"
)
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
spec_coefs <- spec_coefs %>%
  rename(Species = species) %>%
  arrange(Response, Predictor, Species) %>%
  relocate(Species, .after = Predictor) %>%
  mutate(z_crit = z_crit) %>%
  rowwise() %>%
  mutate(
    t_crit = qt(crit_prob, DF),
    lower_CL = Estimate - (t_crit * Std.Error),
    upper_CL = Estimate + (t_crit * Std.Error),
    lower_zCL = Estimate - (z_crit * Std.Error),
    upper_zCL = Estimate + (z_crit * Std.Error)
  ) %>%
  group_by(Response, Predictor) %>%
  mutate(
    CI_overlap = first(lower_CL) <= last(upper_CL) &
      last(lower_CL) <= first(upper_CL)
  ) # %>%
  # ggplot(aes(x = Predictor, y = Estimate, col = species)) +
  # facet_wrap(~Response, scales = "free_y") +
  # geom_hline(yintercept = 0, col = "black", linetype = "dotted") +
  # geom_errorbar(
  #   aes(ymin = lower_CL, ymax = upper_CL, linetype = CI_overlap),
  #   width = 0.2, linewidth = 1,
  #   position = position_dodge(.2)
  # ) +
  # geom_point(
  #   size = 4, aes(shape = P.Value > 0.1), stroke = 2, fill = "grey",
  #   position = position_dodge(.2)
  # ) +
  # theme_bw() +
  # scale_linetype_manual(values = c("solid", "dashed")) +
  # scale_shape_manual(values = c(16, 21))

# spec_coefs <- spec_coefs %>% arrange(Response, Predictor, species) %>%
#   group_by(Response, Predictor) %>%
#   mutate(
#     larger = which.max(Estimate),
#     smaller = which.min(Estimate),
#     upper_low = Estimate[unique(larger)] - (z_crit*Std.Error[unique(larger)]),
#     lower_high = Estimate[unique(smaller)] + (z_crit*Std.Error[unique(smaller)]),
#     CI_overlap = upper_low < lower_high,
#   ) %>%
#   select(
#     -larger, -smaller
#   )

saveRDS(
  spec_coefs,
  "infection-modeling/data/model-objects/joint-species-coefficients_SEM.rds"
)

## Count the coefficients that do and do not differ
spec_coefs %>% group_by(CI_overlap) %>% tally() %>% mutate(n = n/2)


## ---- Species cumulative effects ----

## PELE Graph
pele_fp <- build_nodes_edges(pele_out$coefficients)
pele_baseplot <- build_psem_plot(pele_fp)
# render_graph(pele_baseplot)

## PEMA graph
pema_fp <- build_nodes_edges(pema_out$coefficients)
pema_baseplot <- build_psem_plot(pema_fp)
# render_graph(pema_baseplot)

## PELE
# build the table to fill
pele_cumtab <- expand_grid(
  To = unique(pele_fp$edges$To),
  From = unique(pele_fp$edges$From)
) %>%
  filter(To != From) %>%
  mutate(
    respID = pele_fp$nodes$id[match(To, pele_fp$nodes$name)],
    predID = pele_fp$nodes$id[match(From, pele_fp$nodes$name)]
  ) %>%
  left_join(
    select(pele_fp$edges, To, From, Val, P.Value),
    by = c("To", "From")
  ) %>%
  rename(direct = Val)

# add a list of all the paths from the responses to the predictors
pele_cumtab <- pele_cumtab %>%
  rowwise() %>% # setup to loop through each row
  mutate(
    all_paths = list(unique(get_paths(pele_baseplot, from = predID, to = respID))),
  )

# calculate direct and indirect effects
pele_cumtab <- pele_cumtab %>%
  mutate(
    all_coefs = list(lapply(
      all_paths,
      function(x){
        get_coefs_along_path(
          p = x, edges = pele_fp$edges, nodes = pele_fp$nodes
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

# clean up
pele_cumeffs <- pele_cumtab %>%
  unnest(new_direct) %>%
  ungroup() %>%
  mutate(
    new_direct = replace_na(new_direct, 0),
    indirect = total - new_direct
  ) %>%
  select(
    To, From, P.Value,  direct, new_direct, indirect, total
  ) %>%
  mutate(Species = "PELE")

saveRDS(
  pele_cumeffs,
  file = file.path(
    "infection-modeling/data/model-objects",
    "PELE_cumulative-effects-table_SEM.rds"
  )
)

## PEMA
# build the table to fill
pema_cumtab <- expand_grid(
  To = unique(pema_fp$edges$To),
  From = unique(pema_fp$edges$From)
) %>%
  filter(To != From) %>%
  mutate(
    respID = pema_fp$nodes$id[match(To, pema_fp$nodes$name)],
    predID = pema_fp$nodes$id[match(From, pema_fp$nodes$name)]
  ) %>%
  left_join(
    select(pema_fp$edges, To, From, Val, P.Value),
    by = c("To", "From")
  ) %>%
  rename(direct = Val)

# add a list of all the paths from the responses to the predictors
pema_cumtab <- pema_cumtab %>%
  rowwise() %>% # setup to loop through each row
  mutate(
    all_paths = list(unique(get_paths(pema_baseplot, from = predID, to = respID))),
  )

# calculate direct and indirect effects
pema_cumtab <- pema_cumtab %>%
  mutate(
    all_coefs = list(lapply(
      all_paths,
      function(x){
        get_coefs_along_path(
          p = x, edges = pema_fp$edges, nodes = pema_fp$nodes
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

# clean up
pema_cumeffs <- pema_cumtab %>%
  unnest(new_direct) %>%
  ungroup() %>%
  mutate(
    new_direct = replace_na(new_direct, 0),
    indirect = total - new_direct
  ) %>%
  select(
    To, From, P.Value,  direct, new_direct, indirect, total
  ) %>%
  mutate(Species = "PEMA")

saveRDS(
  pema_cumeffs,
  file = file.path(
    "infection-modeling/data/model-objects",
    "PEMA_cumulative-effects-table_SEM.rds"
  )
)

## Combine them
sp_cumeffs <- bind_rows(pele_cumeffs, pema_cumeffs)

# save it
saveRDS(
  sp_cumeffs,
  file = file.path(
    "infection-modeling/data/model-objects",
    "species_cumulative-effects-table_simplified-SEM.rds"
  )
)
