# Estimate the impact of trait variables on infection models.

## ---- Setup ----
library(tidyverse)
library(lme4)
library(lmerTest)

# Load raw data
PELE_data <- readRDS("infection-modeling/data/peromyscus-model-data.rds") %>%
  mutate(species = updated_taxa) %>%
  filter(species == "PELE") %>%
  mutate(
    across(c(siteID, plotID, year, iid, species), ~as.factor(.x)) # factor vars
  ) 

complete_data <- PELE_data %>% 
  select(
    siteID, year, # random effects 
    Bb_infected, # response
    ticks_attached, # parasitism
    sex_male, sex_mature, weight, cap_prop_night, capprop_shift, expr_PC1, expr_PC2, # phenotypes
    wthr_PC1, clim_PC1 # environment
  ) %>% 
  filter(complete.cases(.))

# load models
mod_obs <- readRDS("infection-modeling/data/model-objects/pele_2026_full-sem-objects.rds")

## ---- Construct models of infection ----

# Original model of infections
original_fm = mod_obs$component_mods$infection %>% update(data = complete_data)
# R2 values
original_r2 = MuMIn::r.squaredGLMM(original_fm)

# Add in variables that were removed during model selection ()
full_fm = original_fm %>% update(. ~ . +cap_prop_night +clim_PC1 )
full_r2 = MuMIn::r.squaredGLMM(full_fm)

# Parasitism and environment only
parenv_fm = full_fm %>% update(. ~ . -sex_male -sex_mature -weight -cap_prop_night -capprop_shift -expr_PC1 -expr_PC2)
parenv_r2 = MuMIn::r.squaredGLMM(parenv_fm)

# Environment only
env_fm = parenv_fm %>% update(. ~ . -ticks_attached)
env_r2 = MuMIn::r.squaredGLMM(env_fm)

# null
null_fm = env_fm %>% update(. ~ . -wthr_PC1 -clim_PC1)
null_r2 = MuMIn::r.squaredGLMM(null_fm)

## ---- Compare models ----

# Conduct a formal model comparison ANOVA.
(anov <- anova(null_fm, env_fm, parenv_fm, original_fm, full_fm))
## Conclusion: When fit to the same data set:
##    - the environment+parasitism is a significant improvement over the environment-only model (P=0.016)
##    - the original model is a further improvement over env+parasitism (P=0.002)
##    - the full model is not an improvement over the original model (P=0.366)
##    - the original model is best for prediction (lowest AIC)
##    - the full model is technically the "best" fit (highest logLik), but I'd consider this an overfit.

# convert to data frame
anov_df = data.frame(anov) %>%
  setNames(c("npar", "AIC", "BIC", "logLiK", "-2logLik", "Chisq", "DF", "P")) %>% 
  select(-c(BIC, `-2logLik`)) %>% 
  rownames_to_column(var = "model") %>% mutate(model = gsub("_fm", "", model)) %>% 
  tibble()

# collect theoretical R2 values
R2_tab <- lapply(list(null_r2, env_r2, parenv_r2, original_r2, full_r2), \(x) x[1, ]) %>% bind_rows() %>% 
  setNames(c("R2_marg", "R2_cond")) %>% 
  tibble()

# get the full comparison table
compare_tab <- anov_df %>% bind_cols(R2_tab)

# save it in a spreadsheet
write.csv(
  compare_tab, 
  "infection-modeling/manuscripts/Tables and Figures/Updated-tables_PELE-only/infect-mod-compare.csv"
)

anova(parenv_fm, full_fm)
