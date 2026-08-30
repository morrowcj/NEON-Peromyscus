# Script to impute Peromyscus data from the PSEM model

## ---- Setup ----

library(tidyverse)
library(lme4)
library(lmerTest)

## ---- Load and setup data ----

# Load raw data
PELE_data <- readRDS("infection-modeling/data/peromyscus-model-data.rds") %>%
  mutate(species = updated_taxa) %>%
  filter(species == "PELE") %>%
  mutate(
    across(c(siteID, plotID, year, iid, species), ~as.factor(.x)), # factor vars
    clim_PC1 = -1*clim_PC1 # invert climate PC so it increases with temp, precip
  ) 

# random effects variables
ranef_vars <- c("siteID", "year")

# fixed effects variables (and tolerance)
fixef_vars <- c(
  "sex_male",
  "sex_mature", 
  "weight",
  "cap_prop_night",
  "capprop_shift",
  "ticks_attached",
  "Bb_infected",
  "expr_PC1",
  "expr_PC2",
  "wthr_PC1",
  "clim_PC1"
)

# load models
mod_obs <- readRDS(
  "infection-modeling/data/model-objects/pele_2026_full-sem-objects.rds"
)

# Refit the models, to ensure that the clim PC coefficients are correct
modlist <- lapply(
  mod_obs$component_mods, function(x) update(x, data = PELE_data)
)

# Reduce the dataset to include:
## 1) only columns for variables that are needed
## 2) only rows that contain all of the minimum data (sex, wthr, clim)
impute_data <- PELE_data %>%
  select(
    uid, # observation identifier - needed to rejoin with full data
    all_of(ranef_vars), all_of(fixef_vars)
  ) %>% 
  filter(!is.na(sex_male), !is.na(wthr_PC1), !is.na(clim_PC1))


# list the fixed effects for each model
lapply(modlist, function(x) names(fixef(x)))

## ---- Get random effects tables ----

# Site
lapply(modlist, function(x) ranef(x)$siteID)

# Year
lapply(modlist, function(x) ranef(x)$year)

## ---- Impute maturity ----

# predict from the model
pred_mature <- lme4:::predict.merMod(
  modlist$maturity, 
  newdata = impute_data, # use the updated data set
  type = "response", # return values on the scale of the response 
  allow.new.levels = TRUE, # if random effect not available for a group, use the population average (unconditional)
  se.fit = TRUE # also calculate the SE for each prediction
)

# Check that predictions are the correct dimensions
stopifnot(length(pred_mature$fit) == nrow(impute_data))

# update the data with the predictions
impute_data <- impute_data %>% 
  mutate(
    sex_mature.obs = sex_mature, # rename to indicate this is observed
    sex_mature.pred = pred_mature$fit, # add predicted values
    sex_mature.SEpred = pred_mature$se, # add SEs
    # use observed if it exists, else use prediction:
    sex_mature = if_else(is.na(sex_mature.obs), sex_mature.pred, sex_mature.obs)
  )

## ---- Impute weight ----

# predict
pred_weight <- lme4:::predict.merMod(
  modlist$weight, type = "response", newdata = impute_data,
  allow.new.levels = TRUE, se.fit = TRUE
)

# check
stopifnot(length(pred_weight$fit) == nrow(impute_data))

# update
impute_data <- impute_data %>% 
  mutate(
    weight.obs = weight, 
    weight.pred = pred_weight$fit, 
    weight.SEpred = pred_weight$se,
    weight = if_else(is.na(weight.obs), weight.pred, weight.obs)
  )

## ---- Impute capture time ----

# predict
pred_captime <- lme4:::predict.merMod(
  modlist$captime, type = "response", newdata = impute_data,
  allow.new.levels = TRUE, se.fit = TRUE
)

# check
stopifnot(length(pred_captime$fit) == nrow(impute_data))

# update
impute_data <- impute_data %>% 
  mutate(
    cap_prop_night.obs = cap_prop_night, 
    cap_prop_night.pred = pred_captime$fit,
    cap_prop_night.SEpred = pred_captime$se,
    cap_prop_night = if_else(
      is.na(cap_prop_night.obs), cap_prop_night.pred, cap_prop_night.obs
    )
  )

## ---- Impute shift in capture time ----

# predict
pred_capshift <- lme4:::predict.merMod(
  modlist$captime_shift, type = "response", newdata = impute_data,
  allow.new.levels = TRUE, se.fit = TRUE
)

# check
stopifnot(length(pred_captime$fit) == nrow(impute_data))

# update
impute_data <- impute_data %>% 
  mutate(
    capprop_shift.obs = capprop_shift,
    capprop_shift.pred = pred_capshift$fit,
    capprop_shift.SEpred = pred_capshift$se,
    capprop_shift = if_else(
      is.na(capprop_shift.obs), capprop_shift.pred, capprop_shift.obs
    )
  )

## ---- Impute parasitism ----

# predict
## NOTE: here, prediction is a fraction representing parasitism probability
pred_ticks <- lme4:::predict.merMod(
  modlist$ticks, type = "response", newdata = impute_data,
  allow.new.levels = TRUE, se.fit = TRUE
)

# Alternatively, we could make this binary by using a 0.5 threshold
alt_tick_preds <- if_else(pred_ticks$fit >= 0.5, 1, 0)

# check
stopifnot(length(pred_ticks$fit) == nrow(impute_data))

# update
impute_data <- impute_data %>% 
  mutate(
    ticks_attached.obs = ticks_attached,
    ticks_attached.pred = pred_ticks$fit,
    ticks_attached.SEpred = pred_ticks$se,
    ticks_attached = if_else(
      is.na(ticks_attached.obs), ticks_attached.pred, ticks_attached.obs
    )
  )

## ---- Impute Resistance ----

# predict
## NOTE: recall that this is a prediction of the PC axis, not resistance itself
pred_resistance <- lme4:::predict.merMod(
  modlist$resistance, type = "response", newdata = impute_data,
  allow.new.levels = TRUE, se.fit = TRUE
)

# check
stopifnot(length(pred_resistance$fit) == nrow(impute_data))

# update
impute_data <- impute_data %>% 
  mutate(
    expr_PC1.obs = expr_PC1,
    expr_PC1.pred = pred_resistance$fit,
    expr_PC1.SEpred = pred_resistance$se,
    expr_PC1 = if_else(
      is.na(expr_PC1.obs), expr_PC1.pred, expr_PC1.obs
    )
  )

## ---- Impute Infection ----

# predict
## NOTE: Like parasitism, this is a predicted probability of infection
pred_infection <- lme4:::predict.merMod(
  modlist$infection, type = "response", newdata = impute_data,
  allow.new.levels = TRUE, se.fit = TRUE
)

# check
stopifnot(length(pred_infection$fit) == nrow(impute_data))

# update
impute_data <- impute_data %>% 
  mutate(
    Bb_infected.obs = Bb_infected,
    Bb_infected.pred = pred_infection$fit,
    Bb_infected.SEpred = pred_infection$se,
    Bb_infected = if_else(
      is.na(Bb_infected.obs), Bb_infected.pred, Bb_infected.obs
    )
  )

## ---- Impute tolerance ----

# predict
## NOTE: like resistance, this is a prediction of the tolerance PC axis
pred_tolerance <- lme4:::predict.merMod(
  modlist$tolerance, type = "response", newdata = impute_data,
  allow.new.levels = TRUE, se.fit = TRUE
)

# check
stopifnot(length(pred_tolerance$fit) == nrow(impute_data))

# update
impute_data <- impute_data %>% 
  mutate(
    expr_PC2.obs = expr_PC2,
    expr_PC2.pred = pred_tolerance$fit,
    expr_PC2.SEpred = pred_tolerance$se,
    expr_PC2 = if_else(
      is.na(expr_PC2.obs), expr_PC2.pred, expr_PC2.obs
    )
  )

## ---- Save the imputed data ----

# rearrange the columns
impute_data <- impute_data %>% 
  relocate(sex_male, .after = year) %>% 
  relocate(ends_with(".obs"), .after = sex_male) %>% 
  relocate(ends_with(".pred"), .before = sex_mature) %>% 
  relocate(ends_with(".SEpred"), .after = -1)
  
# save an R object
saveRDS(impute_data, "infection-modeling/data/model-objects/imputed-PELE-data.rds")

# write a csv file
# write.csv(impute_data, "infection-modeling/data/model-objects/imputed-PELE-data.csv")

## ---- Visualizations ----

theme_set(theme_bw())

# relationship between predicted and observed infections
impute_data %>% 
  ggplot(aes(y = Bb_infected.obs, x = Bb_infected.pred)) + 
  geom_point(alpha = 0.2) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") + #1:1 line
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

# relationship between predicted and observed parasitism
impute_data %>% 
  ggplot(aes(y = ticks_attached.obs, x = ticks_attached.pred)) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") + #1:1 line
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

# relationship between predicted and observed resistance
impute_data %>% 
  ggplot(aes(y = expr_PC1.obs, x = expr_PC1.pred)) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") + #1:1 line
  geom_point(size = 3) + 
  geom_smooth(method = "lm")

# relationship between predicted and observed tolerance
impute_data %>% 
  ggplot(aes(y = expr_PC2.obs, x = expr_PC2.pred)) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") + #1:1 line
  geom_point(size = 3) + 
  geom_smooth(method = "lm")


# -- Relationships with infection --

# predicted relationship between weight and infection after imputation
impute_data %>% 
  filter(weight <= 50) %>%  # remove outliers
  ggplot(aes(x = weight, y = Bb_infected)) + 
  geom_point() + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

# predicted relationship between resistance and infection after imputation
impute_data %>% 
  ggplot(aes(x = expr_PC1, y = Bb_infected)) + 
  geom_point() + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

# predicted relationship between tolerance and infection after imputation
impute_data %>% 
  ggplot(aes(x = expr_PC2, y = Bb_infected)) + 
  geom_point() + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

# predicted relationship between weather (PC) and infection after imputation
impute_data %>% 
  ggplot(aes(x = wthr_PC1, y = Bb_infected)) + 
  geom_point() + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

# predicted relationship between climate (PC) and infection after imputation
impute_data %>% 
  ggplot(aes(x = clim_PC1, y = Bb_infected)) + 
  geom_point() + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

## ---- Sample from imputation ----

# -- Weight? --

# get upper and lower quartiles of observed weights:
wght_q <- quantile(impute_data$weight.obs, na.rm = TRUE, probs = c(0.25, 0.75))

# filter the data by these quartiles
small_mice <- impute_data %>% 
  filter(weight <= wght_q[1], year %in% c(2022, 2023, 2024)) %>% 
  mutate(size = "small")
large_mice <- impute_data %>% filter(weight >= wght_q[2]) %>% mutate(size = "large")

# sample from these subsets
small_sample <- small_mice[sample(nrow(small_mice), size = 20), ]
large_sample <- large_mice[sample(nrow(large_mice), size = 20), ]

# Look at the relationship with infection
bind_rows(small_sample, large_sample) %>% 
  ggplot(aes(x = size, y = Bb_infected)) + 
  stat_summary(fun.data = mean_se) + 
  geom_smooth(method = "lm", aes(group = 1))

# look at relationship with parasitism
bind_rows(small_sample, large_sample) %>% 
  ggplot(aes(x = size, y = ticks_attached)) + 
  stat_summary(fun.data = mean_se) + 
  geom_smooth(method = "lm", aes(group = 1))



