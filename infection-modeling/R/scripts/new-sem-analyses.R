## ---- Script options ----
library(optparse)

## terminal usage: Rscript 08-bootstrap_SEMS.R (-f|--force)

# Construct the argument parser, with one optional flag
parser <- OptionParser()
parser <- add_option(
  parser, c("-f", "--force"),
  action = "store_true",
  default = FALSE,
  dest = "force",
  help = "Force long-running code to execute [FALSE]"
)
opt <- parse_args(parser)

# pass the value of the force argument to the variable
force_run = opt$force

## ---- Setup ----

library(tidyverse)
library(lme4)
library(lmerTest)
library(piecewiseSEM)
library(semEff)

# construct path to model data ojects
model_path = file.path(
  "infection-modeling/data/model-objects/unscaled-peros_SEM-objects.rds"
)

# read the objects
fullSEM_model_objects <- readRDS(model_path)

# extract list of component modles
modlist <- fullSEM_model_objects$component_mods

# extract the fit object
full_psem <- fullSEM_model_objects$sem_mod$fit

# Load raw data
Peros <- readRDS("infection-modeling/data/peromyscus-model-data.rds") %>%
  mutate(species = updated_taxa) %>%
  filter(!is.na(species)) %>%
  mutate(
    across(c(siteID, plotID, year, iid, species), ~as.factor(.x)) # factor vars
  )

#' Helper function to convert summary lme4 coefficient tables into tibbles
#'
#' @param tab coefficient table as returned by summary(x)$coefficient
#' @param type "lmer" or "glmer"
#'
#' @returns a tibble with better names
format_coef_tab <- function(tab, type = c("lmer", "glmer")){
  if (type[1] == "lmer") {
    cont_coef_names = c("Est", "SE", "DF", "t", "P")
  } else {
    # TBD
  }

  tab %>%
    data.frame() %>%
    setNames(cont_coef_names) %>%
    rownames_to_column("Source") %>%
    mutate(sig = if_else(P <= 0.1, "*", "")) %>%
    tibble()
}

space_use_differences <- list()

## ---- PELE SEM ----

# Update each component mod by removing random species effect, only keep PELE
pele_mods <- lapply(modlist, function(x){
  update(x, . ~ . -(1|species), data = Peros %>% filter(species == "PELE"))
})

# construct and fit pSEM
PELE_semSpec <- piecewiseSEM::as.psem(pele_mods)
PELE_psem <- summary(PELE_semSpec)

## ---- Add space use ----

# to existing models
space_mods <- lapply(names(pele_mods), function(id){
 if (id %in% c("tolerance", "infection", "resistance", "ticks")) {
   pele_mods[[id]] %>%
     update(
       . ~ . + avg_move_dist + weighted_trapability + weighted_trap_diversity
     )
 } else {
   return(pele_mods[[id]])
 }
})
names(space_mods) <- names(pele_mods)

# and add new models
space_mods[["trap_diversity"]] <- lmer(
  weighted_trap_diversity ~ sex_male + sex_mature + weight +
    weighted_trapability + avg_move_dist +
    wthr_PC1 + clim_PC1 +
    (1|siteID) + (1|year), data = Peros %>% filter(species == "PELE")
)

space_mods[["trapability"]] <- lmer(
  weighted_trapability ~ sex_male + sex_mature + weight + avg_move_dist +
    wthr_PC1 + clim_PC1 +
    (1|siteID) + (1|year), data = Peros %>% filter(species == "PELE")
)

space_mods[["move_dist"]] <- lmer(
  avg_move_dist ~ sex_male + sex_mature + weight + wthr_PC1 + clim_PC1 +
    (1|siteID) + (1|year), data = Peros %>% filter(species == "PELE")
)

space_semSpec <- piecewiseSEM::as.psem(space_mods)
space_sem <- summary(space_semSpec)

## ---- Compare the models ----

comp_table <- space_sem$coefficients %>%
  select(Response, Predictor, Std.Estimate, P.Value) %>%
  mutate(mod = "with_space") %>%
  full_join(
    PELE_psem$coefficients %>%
      select(Response, Predictor, Std.Estimate, P.Value) %>%
      mutate(mod = "no_space")
  )

comp_table %>%
  complete(mod, Response, Predictor) %>%
  group_by(Response, Predictor) %>%
  mutate(
    sig = P.Value <= 0.1,
    all_na = all(is.na(P.Value)),
    changed = length(unique(sig)) != 1
  ) %>% filter(changed, !all_na) %>% ungroup() %>%
  pivot_wider(
    id_cols = c(Response, Predictor),
    names_from = mod, values_from = c(Std.Estimate, P.Value)
  ) %>%
  setNames(c(
    "Response", "Predictor", "Est. (no space)", "Est. (with space)",
    "P (no space)", "P (with space)"
  )) %>%
  write.csv("infection-modeling/data/pele-sem_space-comparison.csv")

## ---- Do the same with log-transformed variants.
logPeros <- Peros %>%
  mutate(
    across(
      c(avg_move_dist, weighted_trapability, weighted_trap_diversity),
      ~log(.x + 1)
    ),
  )

logspace_mods <- lapply(names(pele_mods), function(id){
  if (id %in% c("tolerance", "infection", "resistance", "ticks")) {
    pele_mods[[id]] %>%
      update(
        . ~ . + avg_move_dist + weighted_trapability + weighted_trap_diversity,
        data = logPeros %>% filter(species == "PELE")
      )
  } else {
    return(pele_mods[[id]])
  }
})
names(logspace_mods) <- names(pele_mods)

logspace_semSpec <- piecewiseSEM::as.psem(logspace_mods)
logspace_sem <- summary(logspace_semSpec)

space_comp_tab <- logspace_sem$coefficients %>%
  select(Response, Predictor, Std.Estimate, P.Value) %>%
  mutate(mod = "log-space") %>%
  full_join(
    space_sem$coefficients %>%
      select(Response, Predictor, Std.Estimate, P.Value) %>%
      filter(Response %in% unique(logspace_sem$coefficients$Response)) %>%
      mutate(mod = "with_space")
  )

space_comp_tab %>%
  complete(mod, Response, Predictor) %>%
  group_by(Response, Predictor) %>%
  mutate(
    sig = P.Value <= 0.1,
    all_na = all(is.na(P.Value)),
    changed = length(unique(sig)) != 1
  ) %>%
  filter(changed, !all_na) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(Response, Predictor),
    names_from = mod, values_from = c(Std.Estimate, P.Value)
  ) %>%
  setNames(c(
    "Response", "Predictor", "Est. (log space)", "Est. (with space)",
    "P (log space)", "P (with space)"
  )) # no rows

# # updatchanged# # update tolerance model with space use
# new_tol_mod <-    pele_mods[["tolerance"]] %>%
#   update(. ~ . + avg_move_dist + weighted_trapability + weighted_trap_diversity)
#
# # compare against model without space use
#
# ## tolerance
# summary(pele_mods[['tolerance']])$coefficients %>%
#   format_coef_tab() %>%
#   filter(P <= 0.1) # sex_mature, Bb_infected
# summary(new_tol_mod)$coefficients %>%
#   format_coef_tab() %>%
#   filter(P <= 0.1) # sex_mature, avg_move_dist, weighted_trap_diversity
#
# # get list of significant effects... (better applied to whole list?)
# lapply(
#   list("pele" = pele_mods[["tolerance"]], "pele_space" = new_tol_mod),
#   function(x){
#     summary(x)$coefficients %>%
#       format_coef_tab() %>%
#       filter(P<=0.1) %>%
#       pull(Source) %>% sort()
#   }
# )
#
# space_use_differences[["tolerance"]] <-  paste(
#   "The PELE tolerance model that includes space use has new significant",
#   "of avg_move_dist and weighted_trap_diversity, but loses the significant",
#   "effect of Bb_infected. sex_mature is significant in both variants."
# )
