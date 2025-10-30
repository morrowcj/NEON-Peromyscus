library(tidyverse)
library(vegan)
library(lme4)
library(lmerTest)
library(piecewiseSEM)

## ---- Load Data ----

# get the model output directory, for saving files
mod_output_dir <- file.path(
  "infection-modeling/data",
  "model-objects"
)

# Load in the model data
Peros <- readRDS("infection-modeling/data/peromyscus-model-data.rds") %>%
  mutate(
    # species = new_taxonID,
    species = updated_taxa
  ) %>% filter(!is.na(species))
  # mutate(
  #   species = if_else(species == "PESP", "PELEPEMA", species)
  # )


## ---- Discriminant ----

#
# da_mod <- lmer(
#   formula = is_PELE ~ lifeStage + weight + sex_male + sex_mature +
#     year + (1|siteID),
#   data = Peros %>% filter(species %in% c("PELE", "PEMA")) %>%
#     mutate(is_PELE = as.numeric(species == "PELE"))
# )
#
# MuMIn::r.squaredGLMM(da_mod) # bad
#
# predict(da_mod, newdata = Peros %>% filter(species %in% c("PELEPEMA")))
#
# da_lm <- glm(
#   formula = is_PELE ~ lifeStage + weight + sex_male + sex_mature + year +
#     tailLength + earLength,
#   data = Peros %>% filter(species %in% c("PELE", "PEMA")) %>%
#     mutate(is_PELE = as.numeric(species == "PELE")),
#   family = "binomial"
# )
#
# summary(da_lm) # also bad
#
# predict(
#   da_lm, newdata = Peros %>% filter(species %in% c("PELEPEMA")),
#   type = "response"
# )

## ----

# get only the complete cases for variables to use
mod_dat <- Peros %>%
  # filter(!species %in% c("PESP", "PELEPEMA")) %>%  # exclude uncertain species
  mutate(
    across(c(siteID, plotID, year, iid, species), ~as.factor(.x)) # factor vars
  )
  # select(
  #   uid,
  #   siteID, plotID, year, iid, species,# ID cols
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
  weight ~ sex_male + sex_mature +
    wthr_PC1 +
    (1|year) + (1|plotID) + (1|species),
  data = scaled_dat
)
# pele_simple_wt <- update(
#   simple_weight, . ~ . - (1|species),
#   data = scaled_dat %>% filter(species == "PELE")
# )
# pema_simple_wt <- update(
#   simple_weight,  . ~ . - (1|species),
#   data = scaled_dat %>% filter(species == "PEMA")
# )

## Maturity
simple_mature <- glmer(
  sex_mature ~ sex_male +
    wthr_PC1 + clim_PC1 +
    (1|year) + (1|plotID) + (1|species),
  data = scaled_dat, family = "binomial"
)
# pele_simple_mature <- update(
#   simple_mature, . ~ . - (1|species),
#   data = scaled_dat %>% filter(species == "PELE")
# )
# pema_simple_mature <- update(
#   simple_mature, . ~ . - (1|species),
#   data = scaled_dat %>% filter(species == "PEMA")
# )

## Capture time
simple_captime <- lmer(
  cap_prop_night ~ sex_male + sex_mature + weight +
    wthr_PC1 +
    (1|siteID) + (1|species),
  data = scaled_dat
)
# pele_simple_captime <- update(
#   simple_captime, . ~ . - (1|species),
#   data = scaled_dat %>% filter(species == "PELE")
# )
# pema_simple_captime <- update(
#   simple_captime, . ~ . - (1|species),
#   data = scaled_dat %>% filter(species == "PEMA")
# )

## Capture time shift
simple_shift <- lmer(
  capprop_shift ~ sex_male + sex_mature + weight + cap_prop_night +
    (1|siteID) + (1|year) + (1|species),
  data = scaled_dat
)
# pele_simple_shift <- update(
#   simple_shift, . ~ . - (1|species),
#   data = scaled_dat %>% filter(species == "PELE")
# )
# pema_simple_shift <- update(
#   simple_shift, . ~ . - (1|species),
#   data = scaled_dat %>% filter(species == "PEMA")
# )

## Tolerance
simple_tol <- lmer(
  expr_PC2 ~ sex_mature + Bb_infected + ticks_attached +
    # capprop_shift + ## not significant
    (1|siteID) + (1|year) + (1|species),
  # expr_PC2 ~ sex_male*sex_mature*weight + Bb_infected + (1 | siteID),
  data = scaled_dat
)
# pele_simple_tol <- update(
#   simple_tol, . ~ . - (1|species),
#   data = scaled_dat %>% filter(species == "PELE")
# )
# pema_simple_tol <- update(
#   simple_tol, . ~ . - (1|species),
#   data = scaled_dat %>% filter(species == "PEMA")
# )

## Burden
simple_burden <- lmer(
  log_burden ~ sex_male + sex_mature + weight +
    # Bb_infected +
    ticks_attached +
    capprop_shift +
    expr_PC1 +
    wthr_PC1 +
    (1|siteID) + (1|year) + (1|species),
  data = scaled_dat
)
# pele_simple_burd <- update(
#   simple_burden, . ~ . - (1|species),
#   data = scaled_dat %>% filter(species == "PELE")
# )
# pema_simple_burd <- update(
#   simple_burden, . ~ . - (1|species),
#   data = scaled_dat %>% filter(species == "PEMA")
# )

## Infection
simple_inf <- glmer(
  Bb_infected ~ sex_male + sex_mature + weight + ticks_attached +
    # weighted_trapability + weighted_trap_diversity +
    capprop_shift + # cap_prop_night +
    expr_PC1 +
    wthr_PC1 + # clim_PC1 +
    (1|species) +
    (1|siteID) + (1|year) ,
  data = scaled_dat, family = "binomial"
)
# pema_simple_inf <- update(
#   simple_inf, . ~ . - capprop_shift - (1|species),
#   data = scaled_dat %>% filter(species == "PEMA")
# ) ## RANK DEFICIENT
# pele_simple_inf <- update(
#   simple_inf, . ~ . - capprop_shift - (1|species),
#   data = scaled_dat %>% filter(species == "PELE")
# )

## Resistance
simple_res <- lmer(
  expr_PC1 ~ sex_male + sex_mature + weight + ticks_attached + wthr_PC1 +
    # capprop_shift +
    (1|year) + (1|siteID) + (1|species),
  data = scaled_dat
)
# pema_simple_res <- update(
#   simple_res, . ~ . - (1|species),
#   data = scaled_dat %>% filter(species == "PEMA")
# )
# pele_simple_res <- update(
#   simple_res, . ~ . - (1|species),
#   data = scaled_dat %>% filter(species == "PELE")
# )

## Ticks
simple_tick <- glmer(
  ticks_attached ~ sex_male + sex_mature + weight +
    capprop_shift + # cap_prop_night +
    wthr_PC1 + clim_PC1 +
    (1|siteID) + (1|year) + (1|species),
  data = scaled_dat, family = "binomial"
)
# pema_simple_tick <- update(
#   simple_tick, . ~ . - (1|species),
#   data = scaled_dat %>% filter(species == "PEMA")
# )
# pele_simple_tick <- update(
#   simple_tick, . ~ . - (1|species),
#   data = scaled_dat %>% filter(species == "PELE")
# )

# ---- Combine into SEMs ----

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

## ---- Cumulative effects ----

# get only relevant columns
df <- simple_out$coefficients %>%
  select(Response, Predictor, Std.Estimate)

# number of models each predictor is used in:
df$Predictor %>% table() %>% sort(decreasing = T)

effects_tab <- df %>%

  pivot_wider(names_from = Predictor, values_from = Std.Estimate)

for(resp in unique(df$Response)) {
  df %>% filter(Response == resp)
2}

# Table of effects of each variable on each other
matrix(
  data = NA,
  nrow = length(unique(df$Response)), ncol = length(unique(df$Predictor)),
  dimnames = list(unique(df$Response), unique(df$Predictor))
)

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
    select(frame_list$edges, Response, Predictor, Std.Estimate),
    by = c("Response", "Predictor")
  ) %>% 
  rename(direct = Std.Estimate) 

# add a list of all the paths from the responses to the predictors
cumulative_tab <- cumulative_tab %>% 
  rowwise() %>% # setup to loop through each row
  mutate(
    all_paths = list(unique(get_paths(base_plot, from = predID, to = respID))),
  )

#' Get a vector of the coefficients along a path
#'
#' @param p a vector of integers defining the path 
#' @param edges an edges table
#' @param nodes a nodes table
#'
#' @returns a vector of effects. 
get_coefs_along_path <- function(
    p = c(8, 7, 6, 5, 4, 3, 2, 1),
    edges = frame_list$edges,
    nodes = frame_list$nodes
){
  # early return for a missing path
  if (length(p) == 1 | all(is.na(p))){
    return(NA)
  }
  
  # convert the path numbers into names  
  v <- nodes$name[p]

  ## direct effect
  if (length(p) == 2) {
    # return the direct effect 
    edges %>% 
      filter(Predictor == v[1], Response == v[2]) %>% 
      pull(Std.Estimate) %>% return()
  }
  
  ## indirect effects
  
  # join into a lag table
  tab <- cbind(from = head(v, -1), to = tail(v, -1))
  
  # go through all the rows of the table and find the matching coefficient
  apply(
    tab, 1, function(x) {
      pull(filter(edges, Predictor == x[1], Response == x[2]), Std.Estimate)
    }
  ) %>% return()
}

# calculate direct and indirect effects
cumulative_tab <- cumulative_tab %>% 
  mutate(
    all_coefs = list(lapply(all_paths, get_coefs_along_path)),
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
  select(Response, Predictor, direct, new_direct, indirect, total)

library(lemon)

cum_eff_tab %>% 
  filter(Response %in% c("ticks_attached", "Bb_infected")) %>% 
  pivot_longer(cols = c(new_direct, total)) %>% 
  ggplot(aes(x = Predictor, y = value, fill = name)) + 
  facet_rep_wrap(~Response, scales = "free_y") + 
  geom_hline(yintercept = 0, col = "grey50") + 
  geom_col(width = 0.5, position = "identity") + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )

filtab <- cum_eff_tab %>% 
  filter(
    Response %in% c("ticks_attached", "Bb_infected", "expr_PC2"),
    total > 0
  ) %>% 
  mutate(
    Response = factor(
      Response, 
      levels = c("ticks_attached", "Bb_infected", "expr_PC2"),
      labels = c("Ticks", "Infection", "Tolerance")
    ),
    Predictor = factor(
      Predictor, 
      levels = c(
        "clim_PC1", "wthr_PC1", "sex_male", "sex_mature", "weight", 
        "cap_prop_night", "capprop_shift", "ticks_attached", "Bb_infected"
      ),
      labels = c(
        "climate", "weather", "sex", "maturity", "weight", 
        "captime", "capshift", "ticks", "infection"
      )
    )
  )


# table of effects for Infection, Resistance, and ticks
filtab %>% 
  ggplot(aes(y = Predictor, x = total)) + 
  facet_wrap(~Response) + 
  geom_vline(xintercept = 0, col = "grey50", linetype = "dashed") + 
  geom_col(
    aes(fill = "total"), 
    width = 0.9, linewidth = 0.5, col = "black"
  ) + 
  geom_col(
    aes(x = indirect, fill = "indirect"), 
    position = position_nudge(y = 0.15),
    width = 0.3, linewidth = 0.25, col = "black"
  ) +
  geom_col(
    aes(x = new_direct, fill = "direct"), 
    position = position_nudge(y = -0.15),
    width = 0.3, linewidth = 0.25, col = "black"
  ) +
  theme_bw() + 
  theme(
    strip.background = element_blank(),
    legend.position = "inside", legend.position.inside = c(0.2, 0.9),
    legend.justification = c(0.5, 1),
    legend.background = element_rect(color = "black", linewidth = 0.3)
  ) +
  labs(x = "Standardized effect", fill = NULL) +
  scale_fill_manual(values = c("cornflowerblue", "orange", "grey50"))

ggsave(
  "infection-modeling/graphics/cumulative-effect-fig.png", 
  width = 6, height = 5, dpi = 300
)

# ct <- cumulative_tab %>% ungroup() %>% 
#   mutate(skip = is.na(all_paths))
# 
# for (resp in unique(simple_out$coefficients$Response)) {
#   # look for effects along each path
#   
#   
#     
#   # add to the table
#   cumulative_tab <- bind_rows(resp_tab)
# }

## ---- Alternate out ----

# PELE_sem <- psem(
#   simple_tol %>% update(. ~ . - (1|species), data = scaled_dat %>% filter(species == "PELE")),
#   simple_inf %>% update(. ~ . - (1|species), data = scaled_dat %>% filter(species == "PELE")),
#   simple_res %>% update(. ~ . + clim_PC1 - (1|species), data = scaled_dat %>% filter(species == "PELE")),
#   simple_tick %>% update(. ~ . - (1|species), data = scaled_dat %>% filter(species == "PELE")),
#   simple_shift %>% update(. ~ . - (1|species), data = scaled_dat %>% filter(species == "PELE")),
#   simple_captime %>% update(. ~ . - (1|species), data = scaled_dat %>% filter(species == "PELE")),
#   simple_weight %>% update(. ~ . + clim_PC1 - (1|species), data = scaled_dat %>% filter(species == "PELE")),
#   simple_mature %>% update(. ~ . - (1|species), data = scaled_dat %>% filter(species == "PELE")),
#   data = scaled_dat %>% filter(species == "PELE")
# )
# PELE_out <- summary(PELE_sem)
#
# PEMA_sem <- psem(
#   simple_tol %>% update(. ~ . - (1|species), data = scaled_dat %>% filter(species == "PEMA")),
#   simple_inf %>% update(. ~ . - (1|species), data = scaled_dat %>% filter(species == "PEMA")),
#   simple_res %>% update(. ~ . + clim_PC1 - (1|species), data = scaled_dat %>% filter(species == "PEMA")),
#   simple_tick %>% update(. ~ . - (1|species), data = scaled_dat %>% filter(species == "PEMA")),
#   simple_shift %>% update(. ~ . - (1|species), data = scaled_dat %>% filter(species == "PEMA")),
#   simple_captime %>% update(. ~ . - (1|species), data = scaled_dat %>% filter(species == "PEMA")),
#   simple_weight %>% update(. ~ . + clim_PC1 - (1|species), data = scaled_dat %>% filter(species == "PEMA")),
#   simple_mature %>% update(. ~ . - (1|species), data = scaled_dat %>% filter(species == "PEMA")),
#   data = scaled_dat %>% filter(species == "PEMA")
# )
# PEMA_out <- summary(PEMA_sem)


# species_analysis <- multigroup(alternate_sem, "(1|species)")

# ## leucopus model
# pele_simple_sem <- psem(
#   pele_simple_tol,
#   pele_simple_inf,
#   pele_simple_res,
#   pele_simple_tick,
#   pele_simple_shift,
#   pele_simple_captime,
#   pele_simple_wt,
#   pele_simple_mature
# )
# pele_simple_out <- summary(pele_simple_sem)

# ## maniculatus model
# pema_simple_sem <- psem(
#   pema_simple_tol,
#   pema_simple_inf,
#   pema_simple_res,
#   pema_simple_tick,
#   # pema_simple_shift,
#   # pema_simple_captime,
#   pema_simple_wt,
#   pema_simple_mature
# )
# pema_simple_out <- summary(pema_simple_sem)

## Compare PELE vs PEMA
# anova(pele_simple_sem, pema_simple_sem)
## NOTE:
## Though only a small number of PEMA were captured with sufficient data
## (N = 23 individuals), and capprop_shift had to be dropped due to
## rank-defficiency, the models differ for those individuals...
## PEMA individuals

## Example RE plot ----

# mod <- simple_inf
# taxonRE <- ranef(mod)$species
# vc = data.frame(VarCorr(mod))
# sd <- vc[which(vc$grp == "species"), "sdcor"]
#
# tibble(
#   var = row.names(taxonRE),
#   int = unlist(taxonRE),
#   lower = int - (sd),
#   upper = int + (sd)
# ) %>%
#   ggplot(aes(x = var, y = int, ymax = upper, ymin = lower)) +
#   geom_pointrange() +
#   labs(
#     x = "Species classification", y = "Random intercept (\U00B1 SD)",
#     title = "Random variation in tick attachment by species"
#   )
