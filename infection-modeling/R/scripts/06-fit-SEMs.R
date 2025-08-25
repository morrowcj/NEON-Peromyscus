library(tidyverse)
library(vegan)
library(lme4)
library(lmerTest)
library(piecewiseSEM)

## TODO: utilize new_taxonID and iid

## EDITED: Aug 20 13:27

## ---- Load Data ----

# get the model output directory, for saving files
mod_output_dir <- file.path(
  "infection-modeling/data",
  "model-objects"
)

# Load in the mode ldata
Peros <- readRDS("infection-modeling/data/peromyscus-model-data.rds")

# get only the complete cases for variables to use
mod_dat <- Peros %>% 
  filter(new_taxonID != "PESP") %>% # exclude uncertain species
  select(
    uid,
    siteID, plotID, year, iid, # ID cols
    Bb_infected, Bb_burden, # Borrelia
    sex_male, sex_mature, weight, # mouse traits
    ticks_attached, nymphalTicksAttached, larvalTicksAttached, # ticks
    cap_prop_night,
    weighted_trapability, weighted_trap_diversity, avg_move_dist, # behavior
    expr_PC1, expr_PC2, # gene expression
    wthr_PC1, wthr_PC2, # weather
    clim_PC1, clim_PC2, # climate
  )

complete_dat <- mod_dat %>% 
  filter(
    year %in% c(2020:2024),
    complete.cases(.)
  ) %>% 
  mutate(
    across(
      c(weight, cap_prop_night:clim_PC2),
      ~as.vector(scale(.x))
    ),
    across(c(siteID, plotID, year, iid), ~as.factor(.x)),
    Bb_burden = as.vector(scale(log(Bb_burden + 1)))
  )

full_dat <- mod_dat %>% 
  mutate(
    across(
      c(weight, cap_prop_night:clim_PC2),
      ~as.vector(scale(.x))
    ),
    across(c(siteID, plotID, year, iid), ~as.factor(.x)),
    Bb_burden = as.vector(scale(log(Bb_burden + 1)))
  )
  

## ---- Correct *correlated* PC axes... ----

# Test correlation of expression PC on this dataset
cor_test <- cor.test(complete_dat$expr_PC1, complete_dat$expr_PC2)
c(cor_test$statistic, P = cor_test$p.value, conf_int = cor_test$conf.int)

## This is no longer a problem...

# # read a gene lookup table
# gene_lookup <- readRDS("infection-modeling/data/expression-lookup.rds")
# 
# # get expression data for this observation set
# new_PCdat <- Peros %>% 
#   filter(uid %in% complete_dat$uid) %>%
#   select(uid, gene_lookup$gene, Bb_burden, Bb_infected)
# # fit a new PCA
# new_PCA <- new_PCdat %>% 
#   select(-c(uid, Bb_burden, Bb_infected)) %>% 
#   pca(scale = TRUE)
# 
# # fit a new envfit object
# new_envfit <- envfit(new_PCA, select(new_PCdat, c(Bb_burden, Bb_infected)))
# 
# # display it - note the axes are reversed
# ordiplot(new_PCA) %>% 
#   orditorp(display = "species"); plot(new_envfit, add = TRUE)
# 
# # extract the scores
# new_scores <- scores(new_PCA)
# 
# # get the variables scores
# new_sp <- new_scores$species %>% data.frame() %>% 
#   rownames_to_column("gene") %>% 
#   full_join(gene_lookup) %>% 
#   arrange(group, PC1, PC2) %>% 
#   tibble() %>% 
#   mutate(expr_PC1 = -1*PC1, expr_PC2 = -1*PC2)
# 
# # and the observation scores
# new_st <- bind_cols(
#   new_PCdat,
#   new_scores$sites %>% data.frame()
# ) %>% 
#   tibble() %>% 
#   mutate(expr_PC1 = -1*PC1, expr_PC2 = -1*PC2)
# 
# # view resistance
# new_st %>% 
#   ggplot(aes(x = expr_PC1)) + 
#   geom_hline(yintercept = 0, col = "black", linetype = "dashed") + 
#   geom_vline(xintercept = 0, col = "black", linetype = "dashed") + 
#   geom_smooth(aes(y = `IL-10`, col = "IL-10"), method = "lm") +
#   geom_smooth(aes(y = `IFN-y`, col = "IFN-y"), method = "lm") + 
#   geom_smooth(aes(y = `IL-6`, col = "IL-6"), method = "lm") + 
#   geom_smooth(aes(y = `TLR-2`, col = "TLR-2"), method = "lm") + 
#   geom_point(aes(y = `IL-10`, col = "IL-10"), alpha = 0.5, size = 3) + 
#   geom_point(aes(y = `IFN-y`, col = "IFN-y"), alpha = 0.5, size = 3) + 
#   geom_point(aes(y = `IL-6`, col = "IL-6"), alpha = 0.5, size = 3) + 
#   geom_point(aes(y = `TLR-2`, col = "TLR-2"), alpha = 0.5, size = 3) + 
#   labs(y = "Resistance expression", x = "Expression PC1", color = "Gene") +
#   theme(
#     legend.position = "inside", legend.position.inside = c(0.7, 1),
#     legend.justification = c(1, 1), legend.background = element_blank()
#   )
# 
# # view tolerance
# new_st %>% 
#   ggplot(aes(x = expr_PC2)) + 
#   geom_smooth(aes(y = `GATA-3`, col = "GATA3"), method = "lm") +
#   geom_smooth(aes(y = `TGF-B`, col = "TGFB"), method = "lm") + 
#   geom_point(aes(y = `GATA-3`, col = "GATA3"), alpha = 0.5, size = 3) + 
#   geom_point(aes(y = `TGF-B`, col = "TGFB"), alpha = 0.5, size = 3) + 
#   labs(y = "Tolerance expression", x = "Expression PC2", color = "Gene") +
#   theme(
#     legend.position = "inside", legend.position.inside = c(0.7, 1),
#     legend.justification = c(1, 1), legend.background = element_blank()
#   )
# 
# # add new PCs into dataset
# complete_dat <- left_join(
#   complete_dat %>% select(-c(expr_PC1, expr_PC2)),
#   new_st %>% select(uid, expr_PC1, expr_PC2),
#   by = "uid"
# )

# # Older: from the clean-immune-data.R file
# ipca2324_list <- readRDS(
#   "infection-modeling/data/immune-PCA_no2022.rds"
# )
# 
# new_exprPCs <- semi_join(ipca2324_list$site_scores, complete_dat, by = "uid")
# 
# # now they are no longer correlated...
# new_exprPCs %>%  
#   select(-uid) %>% 
#   with(cor.test(x = expr_PC1, y = expr_PC2))
# 
# complete_dat <- full_join(
#   complete_dat %>% select(-c(expr_PC1, expr_PC2)),
#   new_exprPCs,
#   by = "uid"
# )

## ---- Look at the model data ----

# Are there duplicate individuals?
complete_dat %>% 
  group_by(iid) %>% 
  tally() %>% filter(n > 1) %>% 
  nrow() %>% all.equal(0) %>% isTRUE()

## ---- Tolerance ----

# fit full model
tol_full <- lmer(
  expr_PC2 ~ 
    Bb_burden + 
    sex_male*sex_mature*weight + cap_prop_night + 
    weighted_trap_diversity + weighted_trapability + avg_move_dist + 
    # nymphalTicksAttached + larvalTicksAttached +
    ticks_attached + 
    wthr_PC1 + wthr_PC2 +
    clim_PC1 + clim_PC2 + 
    (1|year) + (1|siteID),
  data = complete_dat
)
summary(tol_full)

# remove singular variables
VarCorr(tol_full)
tol_nosing <- update(tol_full, . ~ . - (1|siteID))

# stepwise model reduction
tol_red <- tol_full %>% 
  step() %>% 
  get_model()

# minimum viable model
tol_alt <- tol_red %>% update(. ~ . + Bb_burden)
summary(tol_alt)

## ---- Burden ----

# fit omnibus burden model
burden_full <- lmer(
  Bb_burden ~ 
    # Bb_infected + 
    sex_male*sex_mature*weight + cap_prop_night +
    weighted_trap_diversity + weighted_trapability + avg_move_dist +
    # nymphalTicksAttached + larvalTicksAttached + 
    ticks_attached + 
    expr_PC1 + # expr_PC2 + 
    wthr_PC1 + wthr_PC2 +
    clim_PC1 + clim_PC2 + 
    (1|year) + (1|siteID),
  data = complete_dat,
)
summary(burden_full)

# non singular model (copy)
burden_nosing <- burden_full

# reduce the burden model
burden_red <- update(burden_full) %>% 
  step() %>% 
  get_model()

# add in critical variables 
burden_alt <- burden_red %>% 
  update(
    . ~ . 
    + ticks_attached + expr_PC1,
  )

# compare to other models - not sig different
anova(burden_full, burden_red, burden_alt)

## ---- Infection ----
# 
# infect_full <- glmer(
#   Bb_infected ~ 
#     sex_male*sex_mature*weight + cap_prop_night +
#     weighted_trap_diversity + weighted_trapability + avg_move_dist +
#     # nymphalTicksAttached + larvalTicksAttached + 
#     ticks_attached + 
#     expr_PC1 + # expr_PC2 + 
#     wthr_PC1 + wthr_PC2 +
#     clim_PC1 + clim_PC2 + 
#     (1|year) + (1|siteID) + (1|tagID),
#   data = complete_dat,
#   family = "binomial"
# )
# summary(infect_full)
# 
# # reduce the model
# infect_red <- infect_full %>% 
#   update(
#     . ~ . 
#     - clim_PC1 - clim_PC2 - wthr_PC2
#     - cap_prop_night - weighted_trapability - weighted_trap_diversity
#     - avg_move_dist
#   ) %>% summary()
# 
# infect_alt <- glmer(
#   Bb_infected ~ 
#     expr_PC1 + 
#     ticks_attached + 
#     sex_male * sex_mature * weight +
#     (1|year) + (1|siteID) + (1|tagID),
#   data = complete_dat,
#   family = "binomial"
# )
# summary(infect_alt)


## ---- Resistance ----

# fit omnibus resistance model
res_full <- lmer(
  expr_PC1 ~ 
    # Bb_infected + Bb_burden + 
    sex_male * sex_mature * weight + cap_prop_night +
    weighted_trap_diversity + weighted_trapability + avg_move_dist +
    # nymphalTicksAttached + larvalTicksAttached + 
    ticks_attached + 
    wthr_PC1 + wthr_PC2 +
    clim_PC1 + clim_PC2 + 
    (1|year) + (1|siteID),
  data = complete_dat
)

# remove singular variable(s)
VarCorr(res_full)
res_nosing <- lm(
  formula = update(formula(res_full), . ~ . - (1|siteID) - (1|year)), 
  data = complete_dat
)

# reduced
res_red <- update(res_full) %>% 
  step() %>% 
  get_model()

# update(
#   res_full, 
#   . ~ . 
#   - sex_male:sex_mature:weight - sex_male:sex_mature - sex_mature:weight 
#   - sex_male:weight
#   - clim_PC2 - clim_PC1
#   - weighted_trapability - avg_move_dist
#   - ticks_attached
# ) %>% summary()

# minimum
res_alt <- update(
  res_red,
  . ~ . + ticks_attached + sex_male 
)

## ---- Tick attachment ----

# fit omnibus tick model
tick_full <- glmer(
  ticks_attached ~ 
    # Bb_infected + Bb_burden + 
    sex_male * sex_mature * weight + cap_prop_night +
    weighted_trap_diversity + weighted_trapability + avg_move_dist +
    wthr_PC1 + wthr_PC2 +
    clim_PC1 + clim_PC2 + 
    (1|year) + (1|siteID),
  data = complete_dat,
  family = "binomial"
)
summary(tick_full)

# no singular
tick_nosing <- tick_full

# reduced model
tick_red <- tick_full %>% 
  update(
    . ~ . 
    # - sex_male*sex_mature*weight + sex_male + sex_mature + weight
    - clim_PC2 - wthr_PC2
    - sex_male:sex_mature:weight - sex_male:sex_mature 
    - sex_mature:weight - sex_male:weight 
    - sex_mature - sex_male
    - weight
    - weighted_trapability - cap_prop_night - avg_move_dist
    - weighted_trap_diversity
  ) 
summary(tick_red)

# minimum viable model
tick_alt <- tick_red %>% update(. ~ . + sex_male - (1|year) + weighted_trap_diversity)

## ---- Capture time ----

# full model
captime_full <- lmer(
  cap_prop_night ~
    sex_male * sex_mature * weight + 
    weighted_trap_diversity + weighted_trapability + avg_move_dist +
    wthr_PC1 + wthr_PC2 +
    clim_PC1 + clim_PC2 + 
    (1|year) + (1|siteID),
  data = complete_dat
)

# remove singular variables
VarCorr(captime_full)
captime_nosing <- captime_full %>% update(. ~ . - (1|siteID))
summary(captime_nosing)

# reduced
captime_red <- captime_full %>% 
  step() %>% 
  get_model()

# add in some important variables
captime_alt <- captime_red %>% 
  update(. ~ . + sex_male*sex_mature)

## ---- Trap diversity ----

# full
trapdiv_full <- lmer(
  weighted_trap_diversity ~ 
    sex_male*sex_mature*weight + 
    weighted_trapability + avg_move_dist + 
    wthr_PC1 + wthr_PC2 +
    clim_PC1 + clim_PC2 + 
    (1|year) + (1|siteID),
  data = complete_dat
)

# remove singular
VarCorr(trapdiv_full)
trapdiv_nosing <- update(trapdiv_full, . ~ . - (1|year))
summary(trapdiv_nosing)

# reduced
trapdiv_red <- trapdiv_full %>% 
  step() %>% 
  get_model()
summary(trapdiv_red)

# add in some interesting variables
trapdiv_alt <- trapdiv_red %>% update(. ~ . + sex_male*weight)
summary(trapdiv_alt)

## ---- Trapability ----

# full
trapable_full <- lmer(
  weighted_trapability ~ 
    sex_male*sex_mature*weight + 
    avg_move_dist + 
    wthr_PC1 + wthr_PC2 +
    clim_PC1 + clim_PC2 + 
    (1|year) + (1|siteID),
  data = complete_dat
)

# remove singular
VarCorr(trapable_full)
trapable_nosing <- update(trapable_full, . ~ . - (1|year))
summary(trapable_nosing)

# reduced
trapable_red <- trapable_full %>% 
  step() %>% 
  get_model()
summary(trapable_red)

# add in first climate axis
trapable_alt <- trapable_red %>% update(. ~ . + clim_PC1)
summary(trapable_alt)

## ---- Move distance ----

# full
movedist_full <- lmer(
  avg_move_dist ~ 
    sex_male*sex_mature*weight + 
    wthr_PC1 + wthr_PC2 +
    clim_PC1 + clim_PC2 + 
    (1|year) + (1|siteID),
  data = complete_dat
)

# remove singular
VarCorr(movedist_full)
movedist_nosing <- update(movedist_full, . ~ . - (siteID))
summary(movedist_nosing)

# reduced
movedist_red <- movedist_full %>% 
  step() %>% 
  get_model()
summary(movedist_red)

# copy
movedist_alt <- movedist_red

## ---- Full SEM (complete) ----

# Full model
full_burd_sem_comp <- psem(
  tol_nosing, 
  burden_nosing,
  res_nosing,
  tick_nosing,
  captime_nosing,
  trapdiv_nosing,
  trapable_nosing,
  movedist_nosing,
  # expr_PC1 %~~% expr_PC2,
  data = complete_dat
)
full_burd_sem_comp_smry <- summary(full_burd_sem_comp)

# missing paths? - None
full_burd_sem_comp_smry$dTable %>% data.frame() %>% filter(P.Value <= 0.1) %>% 
  select(-c(Test.Type, Crit.Value, DF))

# all paths
full_burd_sem_comp_smry$coefficients %>% 
  data.frame() %>% 
  select(Response, Predictor, P.Value, Var.9)

# most important paths
full_burd_sem_comp_smry$coefficients %>% 
  data.frame() %>% 
  select(Response, Predictor, P.Value, Var.9) %>% 
  filter(P.Value <= 0.1)

# save it
saveRDS(
  full_burd_sem_comp_smry, 
  file.path(mod_output_dir, "full-burden-SEM_complete-cases.rds")
)

## ---- Minimal SEM (complete) ----

# model built with selected components
alt_burd_sem_comp <- psem(
  tol_alt, 
  burden_alt,
  res_alt,
  tick_alt,
  captime_alt,
  trapdiv_alt,
  trapable_alt,
  movedist_alt,
  # expr_PC1 %~~% expr_PC2,
  data = complete_dat
)
alt_burd_sem_comp_smry <- summary(alt_burd_sem_comp)

# missing paths
alt_burd_sem_comp_smry$dTable %>% data.frame() %>% filter(P.Value <= 0.1) %>% 
  select(-c(Test.Type, Crit.Value, DF))

# coefficient table
alt_burd_sem_comp_smry$coefficients %>% 
  data.frame() %>% 
  select(Response, Predictor, P.Value, Var.9)

# Update with missing paths
red_burd_sem_comp <- psem(
  tol_alt,
  # tol_alt %>% update(. ~ . + avg_move_dist),
  burden_alt %>% update(. ~ . + wthr_PC1),
  res_alt,
  tick_alt,
  captime_alt,
  trapdiv_alt,
  trapable_alt %>% update(. ~ . + sex_male),
  movedist_alt,
  # expr_PC1 %~~% expr_PC2,
  data = complete_dat
)
red_burd_sem_comp_smry <- summary(red_burd_sem_comp)

# missing paths? - none
red_burd_sem_comp_smry$dTable %>% data.frame() %>% filter(P.Value <= 0.1)

# coefficient table
red_burd_sem_comp_smry$coefficients %>%
  data.frame() %>%
  select(Response, Predictor, Std.Estimate, P.Value, Var.9) 

# save it 
saveRDS(
  red_burd_sem_comp_smry, 
  file.path(mod_output_dir, "reduced-burden-SEM_complete-cases.rds")
)

# Update with missing paths that seem important
sel_burd_sem_comp <- psem(
  tol_alt,
  # tol_alt %>% update(. ~ . + avg_move_dist),
  burden_alt,
  res_alt,
  tick_alt %>% update(. ~ . + cap_prop_night + avg_move_dist),
  captime_alt,
  trapdiv_alt,
  trapable_alt,
  movedist_alt,
  # expr_PC1 %~~% expr_PC2,
  data = complete_dat
)
sel_sem_complete_smry <- summary(sel_burd_sem_comp)

sel_sem_complete_smry$coefficients %>% 
  data.frame() %>% 
  select(Response, Predictor, Std.Estimate, P.Value, Var.9)

sel_sem_complete_smry$R2

# save it 
saveRDS(
  sel_sem_complete_smry, 
  file.path(mod_output_dir, "selected-burden-SEM_complete-cases.rds")
)


## ---- Full SEM (maximal) ----

# Full model
full_burd_sem_max <- psem(
  tol_full %>% update(data = full_dat), 
  burden_full %>% update(data = full_dat),
  res_full %>% update(data = full_dat),
  tick_full %>% update(data = full_dat),
  captime_full %>% update(data = full_dat),
  trapdiv_full %>% update(data = full_dat),
  trapable_full %>% update(data = full_dat),
  movedist_full %>% update(data = full_dat),
  # expr_PC1 %~~% expr_PC2,
  data = full_dat
)
full_burd_sem_max_smry <- summary(full_burd_sem_max)

saveRDS(
  full_burd_sem_max_smry, 
  file.path(mod_output_dir, "full-burden-SEM_maximal-cases.rds")
)

## ---- Alt model (maximal) ----

# Alt model
alt_burd_sem_max <- psem(
  tol_alt %>% update(data = full_dat), 
  burden_alt %>% update(data = full_dat),
  res_alt %>% update(data = full_dat),
  tick_alt %>% update(data = full_dat),
  captime_alt %>% update(data = full_dat),
  trapdiv_alt %>% update(data = full_dat),
  trapable_alt %>% update(data = full_dat),
  movedist_alt %>% update(data = full_dat),
  # expr_PC1 %~~% expr_PC2,
  data = full_dat
)
alt_burd_sem_max_smry <- summary(alt_burd_sem_max)

alt_burd_sem_max_smry$dTable %>% data.frame() %>% 
  select(Independ.Claim, P.Value, Var.6) %>% tibble() %>% 
  filter(P.Value <= 0.1)

alt_burd_sem_max_smry$coefficients %>% 
  data.frame() %>% 
  select(Response, Predictor, P.Value, Var.9) %>% tibble() %>% print(n = 100)

saveRDS(
  alt_burd_sem_max_smry, 
  file.path(mod_output_dir, "reduced-burden-SEM_maximal-cases.rds")
)

# Updated Alt model
sel_burd_sem_max <- psem(
  tol_alt %>% update(
    . ~ . , data = full_dat
  ), 
  burden_alt %>% update(. ~ . + wthr_PC1, data = full_dat),
  res_alt %>% update(data = full_dat),
  tick_alt %>% update(
    . ~ . + sex_mature + weight + cap_prop_night + avg_move_dist + 
      weighted_trapability, 
    data = full_dat
  ),
  captime_alt %>% update(
    . ~ . + weight + weighted_trapability + clim_PC1, 
    data = full_dat
  ),
  trapdiv_alt %>% update(data = full_dat),
  trapable_alt %>% update(
    . ~ . + sex_male + sex_mature + weight + wthr_PC1, 
    data = full_dat
  ),
  movedist_alt %>% update(. ~ . + sex_mature, data = full_dat),
  # expr_PC1 %~~% expr_PC2,
  data = full_dat
)
sel_burd_sem_max_smry <- summary(sel_burd_sem_max)

sel_burd_sem_max_smry$Cstat

sel_burd_sem_max_smry$dTable %>% data.frame() %>% filter(P.Value <= 0.1)


sel_burd_sem_max_smry$coefficients %>% 
  data.frame() %>% 
  select(Response, Predictor, P.Value, Var.9) %>% tibble() %>% print(n = 100)

saveRDS(
  sel_burd_sem_max_smry, 
  file.path(mod_output_dir, "selected-burden-SEM_maximal-cases.rds")
)