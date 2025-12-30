## ---- Packages ----

library(tidyverse)
library(lme4)
library(semEff)

## ---- Data setup ----

# Read and clean the data
Peros <- readRDS("infection-modeling/data/peromyscus-model-data.rds") %>%
  mutate(
    species = updated_taxa
  ) %>% filter(!is.na(species)) %>%
  mutate(
    across(c(siteID, plotID, year, iid, species), ~as.factor(.x)) # factor vars
  )

# load full model statistics (semEff)
full_stats <- readRDS(
  "infection-modeling/data/model-objects/SEM-statistics-all.rds"
)

# load SEM objects
fullSEM_model_objects <- readRDS(
  file.path("infection-modeling/data/model-objects",
            "unscaled-peros_SEM-objects.rds")
)

# extract the model list
modlist <- fullSEM_model_objects$component_mods

# Load the bootstrapped effects
mod_boot <- readRDS(
  "infection-modeling/data/model-objects/rescaled_bootSEM_full.rds"
)

# Create a semEff object
mod_effs <- suppressMessages(semEff(mod_boot, ci.conf = 0.9))

# Extract direct, total, and indirect effects
dir_effs <- getDirEff(mod_effs)
tot_effs <- getTotEff(mod_effs)
ind_effs <- getIndEff(mod_effs)

# Function to replace . with _ in variable names
fix_names <- function(L){
  names(L) <- gsub("\\.", "_", names(L))
  lapply(L, function(x){
    names(x) <- gsub("\\.", "_", names(x))
    x
  })
}

# List of effects to iterate through (repaired names)
eff_list <- list(
  dir = fix_names(dir_effs),
  ind = fix_names(ind_effs),
  tot = fix_names(tot_effs)
)


unscaled_boot <- readRDS("infection-modeling/data/model-objects/bootSEM_full.rds")
unscaled_effs <- suppressMessages(semEff(unscaled_boot, ci.conf = 0.9))

unscaled_eff_list <- list(
  dir = fix_names(getDirEff(unscaled_effs)),
  ind = fix_names(getIndEff(unscaled_effs)),
  tot = fix_names(getTotEff(unscaled_effs))
)


## ---- Prediction scenarios ----

# Create a table of factors that will remain constant for predicitons
constants <- tibble(
  # Individual
  species = "PELE", # P. leucopus
  sex_mature = 1, # mature
  capprop_shift = quantile(Peros$capprop_shift, 0.5, na.rm = TRUE), # avg climate
  # Environment
  year = 2023, # year with most data
  siteID = "STEI", # Steigerwaldt-Chequamegon
  wthr_PC1 = quantile(Peros$wthr_PC1, 0.5, na.rm = TRUE), # avg weather
  clim_PC1 = quantile(Peros$clim_PC1, 0.5, na.rm = TRUE), # avg climate
)

# Create a table of scenarios that match Fig. 1
extreme_scenario <- tibble(
    ID = LETTERS[1:2], # Individual ID
    sex_male = c(0, 1), # F, M
    weight = quantile(Peros$weight, c(0.1, 0.9), na.rm = TRUE), # small, large
    cap_prop_night = quantile(Peros$cap_prop_night, c(0.9, 0.1), na.rm = TRUE), # early, late
    expr_PC1 = quantile(Peros$expr_PC1, c(0.9, 0.1), na.rm = TRUE), # strong, weak
  ) %>%
  bind_cols(constants)

## TODO: am I doing capture time wrong?

# Get a list of quantiles to use as sample points
quants = c(0.1, 0.3, 0.5, 0.6, 0.9)

# Create a table of scenarios from the full population range
complete_scenarios <- expand_grid(
  sex_male = c(0, 1),
  siteID = unique(Peros$siteID),
  weight.quant = quants,
  capprop.quant = quants,
  # capshift.quant = quants,
  res.quant = quants
) %>%
  mutate(
    weight = quantile(Peros$weight, weight.quant, na.rm = TRUE),
    cap_prop_night = quantile(Peros$cap_prop_night, capprop.quant, na.rm = TRUE),
    # capprop_shift = quantile(Peros$capprop_shift, capshift.quant, na.rm = TRUE),
    expr_PC1 = quantile(Peros$expr_PC1, res.quant, na.rm = TRUE)
  ) %>%
  bind_cols(constants %>% select(-siteID))

# Get rows from complete matching extremes (but at all sites)
complete_scenarios %>%
  filter(
    sex_male == 0 & weight.quant == 0.1 &
      capprop.quant == 0.1 & res.quant == 0.9 |
    sex_male == 1 & weight.quant == 0.9 &
      capprop.quant == 0.9 & res.quant == 0.1
  ) %>%
  mutate(scenarioID = LETTERS[sex_male + 1]) %>%
  relocate(scenarioID, .before = 0)

## ---- Naive prediction ----

Peros %>%
  filter(species == "PELE", sex_mature == 1, year == 2023) %>%
  group_by(siteID, sex_male) %>%
  summarize(
    n = n(),
    tick_prop = mean(ticks_attached, na.rm = TRUE),
    infect_prop = mean(Bb_infected, na.rm = TRUE)
  )

# tick predictions for extreme scanrio
extreme_scenario$naive_tick_pred <- predict(
  modlist$ticks, newdata = extreme_scenario, type = "response",
  # re.form = NA # ignore random effects
) # 0.298, 0.385

# tick predictions for complete scenarios
complete_scenarios$naive_tick_pred <- predict(
  modlist$ticks, newdata = complete_scenarios, type = "response"
)

# infection for extreme
extreme_scenario$naive_inf_pred <- predict(
  modlist$infection, type = "response", re.form = NA,
  newdata = extreme_scenario %>% mutate(ticks_attached = naive_tick_pred)
) # 0.260, 0.557

# infection for complete
complete_scenarios$naive_inf_pred <- predict(
  modlist$infection, type = "response",
  newdata = complete_scenarios %>% mutate(ticks_attached = naive_tick_pred)
)

complete_scenarios %>%
  filter(
    sex_male == 0 & weight.quant == 0.1 &
      capprop.quant == 0.1 & res.quant == 0.9 |
      sex_male == 1 & weight.quant == 0.9 &
      capprop.quant == 0.9 & res.quant == 0.1
  ) %>%
  mutate(scenarioID = LETTERS[sex_male + 1]) %>%
  relocate(scenarioID, .before = 0) %>%
  ggplot() +
  geom_col(
    aes(y = naive_tick_pred, x = siteID, fill = scenarioID), position = "dodge"
  )

complete_scenarios %>%
  ggplot(aes(x = weight, y = naive_tick_pred)) +
  geom_point() +
  geom_smooth(method = "lm")

complete_scenarios %>%
  ggplot(aes(x = factor(sex_male), y = naive_tick_pred)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = 1))

complete_scenarios %>%
  ggplot(
    aes(x = cap_prop_night, y = naive_tick_pred)
  ) +
  geom_point() +
  geom_smooth(method = "lm")

complete_scenarios %>%
  ggplot(aes(x = factor(siteID), y = naive_tick_pred)) +
  geom_point(aes(col = factor(sex_male)))


## ---- Informed predictions ----

## TODO - This does not seem to be working...

# Make predictions using effects from semEff
for (eff_type in names(eff_list)) {
  tick_name = paste("tick_pred", eff_type, sep = ".")
  inf_name = paste("inf_pred", eff_type, sep = ".")

  ## ticks for extreme
  extreme_scenario[[tick_name]] <- predEff(
    modlist$ticks, type = "response",
    newdata = extreme_scenario, data = Peros,
    # effects = unscaled_eff_list[[eff_type]]$ticks_attached[-1],
    effects = eff_list[[eff_type]]$ticks_attached[-1]
    # re.form = NA
  )

  ## infection for extreme
  extreme_scenario[[inf_name]] <- predEff(
    modlist$infection, type = "response",
    newdata = extreme_scenario %>% rename(ticks_attached = tick_name),
    data = Peros,
    # effects = unscaled_eff_list[[eff_type]]$Bb_infected[-1],
    effects = eff_list[[eff_type]]$Bb_infected[-1]
    # re.form = NA
  )

  ## ticks for complete
  complete_scenarios[[tick_name]] <- predEff(
    modlist$ticks, type = "response",
    newdata = complete_scenarios, data = Peros,
    # effects = unscaled_eff_list[[eff_type]]$ticks_attached[-1],
    effects = eff_list[[eff_type]]$ticks_attached[-1]
  )

  ## infection for complete
  complete_scenarios[[inf_name]] <- predEff(
    modlist$infection, type = "response",
    newdata = complete_scenarios %>% rename(ticks_attached = tick_name),
    data = Peros,
    effects = eff_list[[eff_type]]$Bb_infected[-1]
    # effects = unscaled_eff_list[[eff_type]]$Bb_infected[-1]
  )
}

(tmp <- extreme_scenario %>%
  select(ID, starts_with("tick_pred"), starts_with("inf_pred"))
  )

## ---- Manually ----

## ---- OLDER ----
#
# # default prediction of ticks from predEff
# tick_preds_noboot <- predEff(
#   modlist$ticks, newdata = pred_dat,
#   type = "response",
#   data = Peros,
# )
# # 0.296, 0.383
#
# # add back into data
# pred_dat$ticks_attached = tick_preds_noboot
#
# # default prediction of infection from predEff
# infect_preds_noboot <- predEff(
#   modlist$infection,
#   newdata = pred_dat,
#   type = "response",
#   data = Peros
# )
# # 0.588, 0.819
#
# pred_dat$Bb_infected = infect_preds_noboot
#
# # ## On average, a large male PELE with high resistance who comes out early has
# # ## a 23% increased probability of infection and 9% increased probability of
# # ## being parasitized by ticks
# # ## compared to a small female PELE with low resistance who comes out late
# # ## at the same site, on the same day.
# # diff(tick_preds_noboot)
# # diff(infect_preds_noboot)
#
# tick_effects <- full_stats %>%
#   filter(effect_type == "total", Response == "ticks_attached") %>%
#   select(Predictor, boot.eff)
#
# tick_effs <- tick_effects$boot.eff
# names(tick_effs) <- tick_effects$Predictor
#
# inf_tick_preds <- predEff(
#   modlist$ticks, newdata = pred_dat, type = "response", data = Peros,
#   effects = tick_effs
# )
#
# predEff(
#   modlist$ticks, newdata = pred_dat, type = "response", data = Peros,
#   effects = fix_names(dir_effs)$ticks.attached
# )
#
# infect_effects <- full_stats %>%
#   filter(effect_type == "total", Response == "Bb_infected") %>%
#   select(Predictor, boot.eff)
#
# inf_effs <- infect_effects$boot.eff
# names(inf_effs) <- infect_effects$Predictor
#
# predEff(
#   modlist$infection,
#   newdata = pred_dat, #%>% mutate(ticks_attached = inf_tick_preds),
#   type = "response", data = Peros, effects = inf_effs
# )
# # 0.832, 0.100 # STEI
#
# stop("Predictions with the bootstraps are not ready - need fixing.")
#
# # tick SEM predictions
# if (FALSE) {
#   tick_preds <- predEff(
#     modlist$ticks, newdata = pred_dat,
#     effects = mod_boot$ticks_attached,
#     type = "response",
#     data = Peros,
#     parallel = "snow", ncpus = 12, ci.conf = 0.9
#   )
#   saveRDS(
#     tick_preds,
#     file = "infection-modeling/data/model-objects/scenario_tick_preds.rds"
#   )
# } else {
#   tick_preds <- readRDS(
#     "infection-modeling/data/model-objects/scenario_tick_preds.rds"
#   )
# }
#
# # tick_preds %>%
# #   bind_cols() %>%
# #   mutate(x = factor(LETTERS[row_number()])) %>%
# #   rowwise() %>%
# #   mutate(
# #     SE_lwr = max(fit - (2*se.fit), 0),
# #     SE_upr = min(fit + (2*se.fit), 1)
# #   ) %>% ungroup() %>%
# #   ggplot(aes(x = x)) +
# #   geom_pointrange(
# #     aes(
# #       y = fit, ymin = SE_lwr, ymax = SE_upr,
# #       col = "± 2SE"
# #     )
# #   ) +
# #   geom_pointrange(
# #     aes(
# #       y = fit, ymin = ci.lower, ymax = ci.upper,
# #       col = "± 90% CI"
# #     ),
# #     position = position_nudge(x = 0.1)
# #   ) +
# #   geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
# #   theme_bw() +
# #   theme() +
# #   labs(x = "Scenario", y = "P(Parasitism)", col = "Estimate")
#
# # add to predicted ticks to data
#
#
# # infection SEM predictions
# if (FALSE) {
#   infect_preds <- predEff(
#     modlist$infection,
#     newdata = pred_dat,
#     effects = mod_boot$Bb_infected,
#     type = "response",
#     data = Peros,
#     parallel = "snow", ncpus = 12
#   )
#
#   saveRDS(
#     infect_preds,
#     file = "infection-modeling/data/model-objects/scenario_infect_preds.rds"
#   )
# } else {
#   infect_preds <- readRDS(
#     "infection-modeling/data/model-objects/scenario_infect_preds.rds"
#   )
# }
#
# # infect_preds %>% bind_cols()
#
# # # example with shipley data
# # ship_preds <- predEff(mod = shipley.sem, newdata = shipley,
# #                       type = "response", parallel = "snow", ncpus = 12)
