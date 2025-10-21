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

# Load in the model data
Peros <- readRDS("infection-modeling/data/peromyscus-model-data.rds")



# get only the complete cases for variables to use
mod_dat <- Peros %>%
  # filter(new_taxonID != "PESP") %>%  # exclude uncertain species
  mutate(
    across(c(siteID, plotID, year, iid, new_taxonID), ~as.factor(.x)) # factor vars
  )
  # select(
  #   uid,
  #   siteID, plotID, year, iid, new_taxonID,# ID cols
  #   Bb_infected, log_burden, log_burden, # Borrelia
  #   sex_male, sex_mature, weight, # mouse traits
  #   ticks_attached, nymphalTicksAttached, larvalTicksAttached, # ticks
  #   cap_prop_night,
  #   weighted_trapability, weighted_trap_diversity, avg_move_dist, # behavior
  #   expr_PC1, expr_PC2, # gene expression
  #   wthr_PC1, wthr_PC2, # weather
  #   clim_PC1, clim_PC2, # climate
  #   behave_PC1, behave_PC2
  # )

scaled_dat <- mod_dat %>% 
  mutate(
    across(
      c(
        weight, cap_prop_night, weighted_trapability, weighted_trap_diversity, 
        avg_move_dist
        # expr_PC1, expr_PC2, wthr_PC1, wthr_PC2, clim_PC1, clim_PC2, 
        # behave_PC1, behave_PC2
        ),
      ~as.vector(scale(.x))
    )
  )




# most_common <- function(x){
#   names(which.max(table(x)))
# }
# 
# iid_dat <- mod_dat %>% group_by(iid) %>% 
#   summarize(
#     plotID = most_common(plotID),
#     plotLat = as.numeric(most_common(plotLat)),
#     plotLon = as.numeric(most_common(plotLon)),
#     sex_male = as.numeric(most_common(sex_male)),
#     mature_prop = mean(sex_mature, na.rm = TRUE),
#     avg_wt = mean(weight, na.rm = TRUE)
#   )

# ---- simpler mods (no interactions) ----
## Weight
simple_weight <- lmer(
  weight ~ sex_male + sex_mature + (1|year) + (1|plotID),
  data = scaled_dat
)
pele_simple_wt <- update(
  simple_weight, data = scaled_dat %>% filter(new_taxonID == "PELE")
)
pema_simple_wt <- update(
  simple_weight, data = scaled_dat %>% filter(new_taxonID == "PEMA")
)

## Maturity
simple_mature <- glmer(
  sex_mature ~ sex_male + (1|year) + (1|plotID), 
  data = scaled_dat, family = "binomial"
)
pele_simple_mature <- update(
  simple_mature,  data = scaled_dat %>% filter(new_taxonID == "PELE")
)
pema_simple_mature <- update(
  simple_mature,  data = scaled_dat %>% filter(new_taxonID == "PEMA")
)
  
## Tolerance 
simple_tol <- lmer(
  expr_PC2 ~ sex_mature + Bb_infected + ticks_attached + 
    # capprop_shift + ## not significant
    (1|siteID) + (1|year),
  # expr_PC2 ~ sex_male*sex_mature*weight + Bb_infected + (1 | siteID),
  data = scaled_dat
)
pele_simple_tol <- update(
  simple_tol, data = scaled_dat %>% filter(new_taxonID == "PELE")
)
pema_simple_tol <- update(
  simple_tol, data = scaled_dat %>% filter(new_taxonID == "PEMA")
)

## Burden
simple_burden <- lmer(
  log_burden ~ sex_male + weight + expr_PC1 + Bb_infected + wthr_PC1 +
    (1|siteID) + (1|year),
  data = scaled_dat
)
# burden_mod <- lmer(
#   log_burden ~ sex_male*weight + expr_PC1 + Bb_infected + wthr_PC1 + 
#     # capprop_shift + ## not significant
#     (1|siteID) + (1|year),
#   data = scaled_dat
# )

## Infection
simple_inf <- glmer(
  Bb_infected ~ sex_male + sex_mature + weight + ticks_attached + 
    capprop_shift + 
    wthr_PC1 +
    (1|siteID) + (1|year),
  data = scaled_dat, family = "binomial"
)
pema_simple_inf <- update(
  simple_inf, . ~ . - capprop_shift,
  data = scaled_dat %>% filter(new_taxonID == "PEMA")
) ## RANK DEFICIENT
pele_simple_inf <- update(
  simple_inf, . ~ . - capprop_shift,
  data = scaled_dat %>% filter(new_taxonID == "PELE")
)

## Resistance
simple_res <- lmer(
  expr_PC1 ~ sex_male + sex_mature + weight + ticks_attached + wthr_PC1 +
    # capprop_shift + 
    (1|year) + (1|siteID),
  data = scaled_dat
)
pema_simple_res <- update(
  simple_res, 
  data = scaled_dat %>% filter(new_taxonID == "PEMA")
)
pele_simple_res <- update(
  simple_res, 
  data = scaled_dat %>% filter(new_taxonID == "PELE")
)

# ---- Combine into SEMs ----

## Full model
simple_sem <- psem(
  simple_tol,
  # simple_burden, # doesn't add anything
  simple_inf,
  simple_res,
  simple_weight,
  simple_mature,
  data = scaled_dat
)
simple_out <- summary(simple_sem)

## leucopus model
pele_simple_sem <- psem(
  pele_simple_tol,
  pele_simple_inf,
  pele_simple_res,
  pele_simple_wt,
  pele_simple_mature
)
# pele_simple_out <- summary(pele_simple_sem)

## maniculatus model
pema_simple_sem <- psem(
  pema_simple_tol,
  pema_simple_inf,
  pema_simple_res,
  pema_simple_wt,
  pema_simple_mature
)

## Compare PELE vs PEMA
anova(pele_simple_sem, pema_simple_sem)
## NOTE:
## Though only a small number of PEMA were captured with sufficient data 
## (N = 23 individuals), and capprop_shift had to be dropped due to 
## rank-defficiency, the models differ for those individuals...
## PEMA individuals


# full mods
# tol_mod <- lmer(
#   expr_PC2 ~ sex_mature + Bb_infected + ticks_attached + 
#     # capprop_shift + ## not significant
#     (1|siteID) + (1|year),
#   # expr_PC2 ~ sex_male*sex_mature*weight + Bb_infected + (1 | siteID),
#   data = scaled_dat
# )
# burden_mod <- lmer(
#   log_burden ~ sex_male*weight + expr_PC1 + Bb_infected + wthr_PC1 + 
#     # capprop_shift + ## not significant
#     (1|siteID) + (1|year),
#   data = scaled_dat
# )
# res_mod <- lmer(
#   expr_PC1 ~ sex_male + sex_mature + weight + ticks_attached + wthr_PC1 + 
#     # capprop_shift + ## not significant
#     (1|siteID) + (1|year),
#   data = scaled_dat
# )
# infect_mod <- glmer(
#   Bb_infected ~ sex_male*sex_mature*weight + ticks_attached + wthr_PC1 +
#   # Bb_infected ~ sex_mature+weight + ticks_attached + wthr_PC1 + 
#     capprop_shift + #cap_prop_night + 
#     (1|siteID) + (1|year) + (1|iid),
#   data = scaled_dat, family = "binomial"
# )
# tick_mod <- glmer(
#   ticks_attached ~ capprop_shift + cap_prop_night + 
#     sex_male + sex_mature + weight + 
#     wthr_PC1 + clim_PC1 + 
#     (1|siteID) + (1|year) + (1|iid),
#   data = scaled_dat, family = "binomial"
# )
# # summary(tick_mod)
# 
# sem_mod <- psem(
#   tol_mod, 
#   burden_mod, 
#   infect_mod, 
#   tick_mod, 
#   data = mod_dat
# )
# 
# (out <- summary(sem_mod))
# 
# ## separate models per species
# 
# # # leucopus
# # pele_mod <- psem(
# #   tol_mod %>% update(data = mod_dat %>% filter(new_taxonID == "PELE")), 
# #   burden_mod %>% update(data = mod_dat %>% filter(new_taxonID == "PELE")), 
# #   infect_mod %>% update(data = mod_dat %>% filter(new_taxonID == "PELE")), 
# #   tick_mod %>% update(data = mod_dat %>% filter(new_taxonID == "PELE")), 
# #   data = mod_dat %>% filter(new_taxonID == "PELE")
# # )
# # pele_out <- summary(pele_mod)
# # 
# # # maniculatus
# # pema_mod <- psem(
# #   tol_mod %>% update(data = mod_dat %>% filter(new_taxonID == "PEMA")), 
# #   burden_mod %>% update(data = mod_dat %>% filter(new_taxonID == "PEMA")), 
# #   infect_mod %>% update(data = mod_dat %>% filter(new_taxonID == "PEMA")), 
# #   tick_mod %>% update(data = mod_dat %>% filter(new_taxonID == "PEMA")), 
# #   data = mod_dat %>% filter(new_taxonID == "PEMA")
# # )
# # pema_out <- summary(pema_mod)
# # 
# # anova(pele_mod, pema_mod)
# 
# # # doesn't work
# # group_out <- multigroup(sem_mod, group = "new_taxonID")
# 
# # sel_both_sem_max_smry <- readRDS(
# #   file.path(mod_output_dir, "selected-BurdenInfection-SEM_maximal-cases.rds")
# # )
