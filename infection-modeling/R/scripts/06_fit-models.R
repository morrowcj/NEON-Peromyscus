library(tidyverse)
library(lme4)
library(lmerTest)
library(piecewiseSEM)

## ---- Load Data ----

# Load in the mode ldata
Peros <- readRDS("infection-modeling/data/peromyscus-model-data.rds")

mod_dat <- Peros %>% 
  select(
    siteID, plotID, year, tagID, iid, # ID cols
    Bb_infected, Bb_burden, # Borrelia
    sex_male, sex_mature, weight, # mouse traits
    ticks_attached, nymphalTicksAttached, larvalTicksAttached, # ticks
    cap_prop_night,
    weighted_trapability, weighted_trap_diversity, avg_move_dist, # behavior
    expr_PC1, expr_PC2, # gene expression
    wthr_PC1, wthr_PC2, # weather
    clim_PC1, clim_PC2, # climate
  ) %>% 
  mutate(
    Bb_burden = scale(log(Bb_burden + 1)),
    across(
      c(weight, cap_prop_night:clim_PC2),
      ~scale(.x)
    )
  ) %>% 
  filter(
    year %in% c(2020:2024)
  )

summary(mod_dat)

## ---- Tolerance ----

# fit omnibus tolerance model
tol_full <- lmer(
  expr_PC2 ~ 
    # Bb_infected + 
    Bb_burden +
    sex_male*sex_mature*weight + cap_prop_night + 
    weighted_trap_diversity + weighted_trapability + avg_move_dist + 
    # nymphalTicksAttached + larvalTicksAttached + 
    ticks_attached + 
    wthr_PC1 + wthr_PC2 +
    clim_PC1 + clim_PC2 + 
    (1|year) + (1|siteID) + (1|tagID),
  data = mod_dat
)
summary(tol_full)

# search for a simplified model
# update(
#   tol_full, 
#   . ~ . 
#     - weighted_trap_diversity - clim_PC2 - wthr_PC2 - weighted_trapability
#     - ticks_attached - wthr_PC1 - clim_PC1 - cap_prop_night - avg_move_dist 
# ) %>% summary()

# reduced model of tolerance
tol_red <- lmer(
  expr_PC2 ~ Bb_burden + (1|tagID),
  data = mod_dat
)
summary(tol_red)
VarCorr(tol_red)
performance::r2(tol_red)

## Because year and site were singular, and no tagIDs were duplicated,
## use a simple linear model
lm(expr_PC2 ~ Bb_burden, data = mod_dat) %>% anova()

## refit the full model
tol_full <- lm(
  expr_PC2 ~ 
    # Bb_infected + 
    Bb_burden +
    sex_male*sex_mature*weight + cap_prop_night + 
    weighted_trap_diversity + weighted_trapability + avg_move_dist + 
    # nymphalTicksAttached + larvalTicksAttached + 
    ticks_attached + 
    wthr_PC1 + wthr_PC2 +
    clim_PC1 + clim_PC2,
  data = mod_dat
)
summary(tol_full)

## reduce the model
tol_red <- update(
  tol_full, 
  data = model.frame(tol_full) %>% filter(complete.cases(.))
) %>% step()

# reduce the model again, expanding the number of samples used
tol_red <- update(
  tol_red, 
  data = mod_dat %>% 
    select(one_of(colnames(model.frame(tol_red)))) %>% 
    filter(complete.cases(.))
) %>% step()

# match previous tolerance SEM component
tol_lgcy <- update(
  tol_full, 
  . ~ Bb_infected + expr_PC1 + 
    larvalTicksAttached + nymphalTicksAttached + weight + 
    (1|year) + (1|siteID) + (1|tagID)
)
summary(tol_lgcy)


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
    (1|year) + (1|siteID) + (1|tagID),
  data = mod_dat,
)
VarCorr(burden_full)
summary(burden_full)

# reduce the burden model
burden_red <- update(
  burden_full, data = model.frame(burden_full) %>% filter(complete.cases(.))
) %>% 
  step() %>% get_model()

# reduce again, attempting to improve sample size
burden_red <- burden_red %>% 
  update(
    data = mod_dat %>% select(one_of(names(model.frame(burden_red))))
  ) %>% 
  step() %>% get_model()

summary(burden_red)
VarCorr(burden_red)
performance::r2(burden_red)

# search for reduced model
# update(
#   burden_full, 
#   . ~ . 
#     - sex_male:sex_mature:weight - sex_mature:weight - sex_male:sex_mature
#     - weighted_trap_diversity - weighted_trapability - avg_move_dist 
#     - clim_PC2 - wthr_PC2
#     - wthr_PC1 - clim_PC1
#     - ticks_attached - cap_prop_night
#     - sex_mature
#     # - expr_PC1
# ) %>% 
#   summary()

burden_red <- lmer(
  Bb_burden ~ expr_PC1 + (1|tagID),
  data = mod_dat
) 
summary(burden_red)

## ---- Infection ----

# fit omnibus infection model
infect_full <- glmer(
  Bb_infected ~ 
    sex_male*sex_mature*weight + cap_prop_night +
    weighted_trap_diversity + weighted_trapability + avg_move_dist +
    # nymphalTicksAttached + larvalTicksAttached + 
    ticks_attached + 
    expr_PC1 + # expr_PC2 + 
    wthr_PC1 + wthr_PC2 +
    clim_PC1 + clim_PC2 + 
    (1|year) + (1|siteID) + (1|tagID),
  data = mod_dat,
  family = "binomial"
)
summary(infect_full)

# search for reduced model
# update(
#   infect_full,
#   . ~ . 
#   - clim_PC2 - clim_PC1 - wthr_PC2
#   - weighted_trapability - weighted_trap_diversity
#   - avg_move_dist - cap_prop_night
#   # - expr_PC1
# ) %>% summary()

# selected infection model
infect_red <- glmer(
  Bb_infected ~ sex_mature + weight + wthr_PC1 + ticks_attached + 
    (1|year) + (1|siteID),
  data = mod_dat, family = "binomial"
)
summary(infect_red)
VarCorr(infect_red)
performance::r2(infect_red)

# match previous infection SEM component
infect_lgcy <- update(
  infect_full,
  . ~ expr_PC1 + wthr_PC1 + weight + 
    larvalTicksAttached + nymphalTicksAttached + 
    (1|year) + (1|siteID) + (1|tagID)
)
summary(infect_lgcy)
  

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
    (1|year) + (1|siteID) + (1|tagID),
  data = Peros
)
summary(res_full)

update(
  res_full, 
  . ~ . 
  - sex_male:sex_mature:weight - sex_male:sex_mature - sex_mature:weight 
  - sex_male:weight
  - clim_PC2 - clim_PC1
  - weighted_trapability - avg_move_dist
  - ticks_attached
) %>% summary()

# match previous resistance SEM component
res_lgcy <- update(
  res_full,
  . ~ weight + wthr_PC1 + sex_male +
  (1|year) + (1|siteID) + (1|tagID)
)
summary(res_lgcy)

## ---- Tick attachment ----

# fit omnibus tick model
tick_fm <- glmer(
  ticks_attached ~ 
    # Bb_infected + Bb_burden + 
    sex_male + sex_mature + weight + cap_prop_night +
    weighted_trap_diversity + weighted_trapability + avg_move_dist +
    wthr_PC1 + wthr_PC2 +
    clim_PC1 + clim_PC2 + 
    (1|year) + (1|siteID) + (1|tagID),
  data = Peros,
  family = "binomial"
)
summary(tick_fm)

# match previous larval SEM component
larv_lgcy <- update(
  tick_fm,
  larvalTicksAttached ~ clim_PC1 + wthr_PC1 + 
    sex_male + sex_mature + cap_prop_night + 
    (1|year) + (1|siteID) + (1|tagID)
)
summary(larv_lgcy)

# match previous nymph SEM component
nymph_lgcy <- update(
  tick_fm,
  nymphalTicksAttached ~ clim_PC1 + wthr_PC1 + 
    sex_male + sex_mature + cap_prop_night + weight + 
    larvalTicksAttached + 
    (1|year) + (1|siteID) + (1|tagID)
)
summary(nymph_lgcy)

## ---- Capture time ----

time_fm <- lmer(
  cap_prop_night ~
    sex_male + sex_mature + weight + 
    weighted_trap_diversity + weighted_trapability + avg_move_dist +
    wthr_PC1 + wthr_PC2 +
    clim_PC1 + clim_PC2 + 
    (1|year) + (1|siteID) + (1|tagID),
  data = Peros
)
summary(time_fm)

# match previous capture time SEM component
time_lgcy <- update(
  time_fm, 
  . ~ 
    sex_male*sex_mature + weight + wthr_PC1 + 
    (1|year) + (1|siteID) + (1|tagID)
)
summary(time_lgcy)

## ---- Move distance ----
dist_fm <- lmer(
  avg_move_dist ~ 
    sex_male + sex_mature + weight + 
    weighted_trap_diversity + weighted_trapability +
    wthr_PC1 + wthr_PC2 +
    clim_PC1 + clim_PC2 + 
    (1|year) + (1|siteID) + (1|tagID),
  data = Peros
)

summary(dist_fm)

## ---- Trap diversity

trapdiv_fm <- lmer(
  weighted_trap_diversity ~
    sex_male + sex_mature + weight + 
    weighted_trapability + 
    wthr_PC1 + wthr_PC2 +
    clim_PC1 + clim_PC2 + 
    (1|year) + (1|siteID) + (1|tagID),
  data = Peros 
)

summary(trapdiv_fm)

## ---- Trapability ----

trapable_fm <- lmer(
  weighted_trapability ~
    sex_male + sex_mature + weight + 
    wthr_PC1 + wthr_PC2 +
    clim_PC1 + clim_PC2 + 
    (1|year) + (1|siteID) + (1|tagID),
  data = Peros 
)

summary(trapable_fm)

## ---- First competence SEM ----

# refit the previous PSEM (but without the res-tol effect)
legacy_sem <- psem(
  tol_lgcy,
  infect_lgcy,
  res_lgcy,
  nymph_lgcy,
  larv_lgcy,
  time_lgcy,
  data = Peros
)
legacy_fm <- summary(legacy_sem)

legacy_fm$dTable %>% data.frame() %>% filter(P.Value <= 0.05)

legacy_fm$coefficients


# # infection competence model
# icomp_spec <- psem(
#   tol_full, 
#   infect_full,
#   res_full, 
#   tick_fm,
#   time_fm,
#   # dist_fm,
#   # trapdiv_fm,
#   # trapable_fm,
#   data = Peros
# )
# 
# icomp_sem <- summary(icomp_spec)

tmp <- psem(
  tol_full, res_full,
  data = mod_dat
)
summary(tmp)
