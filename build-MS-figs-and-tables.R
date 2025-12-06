## ---- Introduction ----

# This is a script to create the high-resolution figures for the
## manuscript.

## ---- Packages and functions ----

# Load needed functions
library(tidyverse)
library(DiagrammeR)
library(ggarrow)
source("infection-modeling/R/functions/diagram_psem.R")

## ---- Data ----

# Read and clean the data
Peros <- readRDS("infection-modeling/data/peromyscus-model-data.rds") %>%
  mutate(
    Species = updated_taxa
  ) %>% filter(!is.na(Species)) %>%
  mutate(
    across(c(siteID, plotID, year, iid, Species), ~as.factor(.x)) # factor vars
  )

### PSEM ###

# Load and clean the SEM (psem)
fullSEM_model_objects <- readRDS(
  "infection-modeling/data/model-objects/simplified-SEM-objects.rds"
)

# extract the fit object
full_psem <- fullSEM_model_objects$sem_mod$fit

# clean the d-seperation table
full_psem$dTable <- full_psem$dTable %>%
  data.frame() %>% rename(sig = "Var.6") %>% tibble()

# clean the coefficient table
full_psem$coefficients <- full_psem$coefficients %>%
  data.frame() %>% rename(sig = "Var.9") %>% tibble()

# load the cummulative effect table (psem)
full_brkdwn <- readRDS(
  file.path("infection-modeling/data/model-objects",
            "cumulative-effects-table_simplified-SEM.rds")
)

# load PELE model objects
pele_model_objects <- readRDS(
  "infection-modeling/data/model-objects/PELE-SEM-objects.rds"
)

# extract the PELE SEM
pele_psem <- pele_model_objects$sem_mod$fit

# load PEMA model objects
pema_model_objects <- readRDS(
  "infection-modeling/data/model-objects/PEMA-SEM-objects.rds"
)

# extract PEMA SEM
pele_psem <- pema_model_objects$sem_mod$fit

# load the joint species coefficients table
spec_coefs <- readRDS(
  "infection-modeling/data/model-objects/joint-species-coefficients_SEM.rds"
) %>%
  rename(Species = Species)

## semEFF ##

# load full model statistics (semEff)
full_stats <- readRDS(
  "infection-modeling/data/model-objects/SEM-statistics-all.rds"
)

# Flag significance based on various methods
full_stats <- full_stats %>%
  mutate(
    psem.sig = P.Value <= 0.1, # raw model stats
    boot.sig = boot.p <= 0.1, # pseudo-p-value, using orig. coef. and boot SE
    boot.sig.mean = boot.p.mean <= 0.1, # pseudo-P using boot coef. and SE
    ci.sig = !(boot.ci.low < 0 & boot.ci.up > 0), # based on 90% CI
    joint_sig = if_else(effect_type == "direct", psem.sig, ci.sig) # combined
  ) %>%
  group_by(Response, Predictor) %>%
  mutate(group.sig = any(joint_sig)) %>%
  ungroup()

# species effects table (semEff)
species_effs_tab <- readRDS(
  "infection-modeling/data/model-objects/SEM-statistics-species.rds"
)

## ---- Lookup table and metadata ----

# Create a lookup table for the variables
var_lookup <- tribble(
  ~ID, ~var, ~clean_name, ~label,
  ~group, ~grp_lab,
  1, "expr_PC2", "Tolerance", "Tolerance\nexpression",
  "Expression", "Immune\nexpression",
  2, "Bb_infected", "Infection", "Infection\nstatus",
  "Infection", "Infection",
  3, "expr_PC1", "Resistance", "Resistance\nexpression",
  "Expression", "Immune\nexpression",
  4, "ticks_attached", "Parasitism", "Parasitism\n(ticks)",
  "Parasitism", "Parasitism\n(ticks)",
  5,"capprop_shift", "Cap. shift", "Capture\ntime \U0394",
  "Behavior", "Behavior\n(timing)",
  6, "cap_prop_night", "Cap. time", "Capture\ntime",
  "Behavior", "Behavior\n(timing)",
  7, "weight", "Weight", "Weight",
  "Phenotype", "Phenotype\n(morphology)",
  8, "sex_mature", "Reproductive", "Reproductive\nmaturity",
  "Phenotype", "Phenotype\n(morphology)",
  9, "sex_male", "Sex", "Sex",
  "Phenotype", "Phenotype\n(morphology)",
  10, "wthr_PC1", "Weather", "Weather\n(PC)",
  "Environment", "Environment",
  11, "clim_PC1", "Climate", "Climate\n(PC)",
  "Environment", "Environment",
) %>%
  mutate(across(-c(ID), ~factor(.x, levels = unique(.x))))

# cut 2pi into even sections
t = seq(0, 2*pi, length.out = length(unique(var_lookup$group)))

# and use them to calculate coordinates around a circle.
coords_tab <- tibble(
  group = c("Parasitism", "Infection", "Environment", "Phenotype",
            "Behavior", "Expression"),
  x = c(0, head(cos(t), -1)),
  y = c(0, head(sin(t), -1))
)

# Join these coordinates the lookup table
var_lookup <- var_lookup %>%
  left_join(coords_tab, by = c("group")) %>%
  rename(grp.x = x, grp.y = y)

# create a table for response variables (renaming)
 resp_lookup <- var_lookup %>%
   rename_with(~str_c("resp.", .), .cols = -var) %>%
   rename(Response = var) %>%
   select(Response, starts_with("resp.")) %>%
   filter(Response %in% full_stats$Response)

# and for predictor variables
 pred_lookup <- var_lookup %>%
   rename_with(~str_c("pred.", .), .cols = -var) %>%
   rename(Predictor = var) %>%
   select(Predictor, starts_with("pred.")) %>%
   filter(Predictor %in% full_stats$Predictor)

# Join these to the full data too
full_stats <- full_stats %>%
  left_join(resp_lookup, by = "Response") %>%
  left_join(pred_lookup, by = "Predictor")

## ---- Fig 1: Extreme scenarios (conceptual) ----

warning("Fig. 1 diagram not available")

## ---- Fig 2: SEM network diagram (conceptual) ----

# filter the data into the rows needed for the conceptual model
concept_data <- full_stats %>%
  filter(group.sig, effect_type == "total") %>%
  group_by(pred.group, resp.group, pick(matches(".*\\.(x|y)"))) %>%
  summarize(boot.eff = mean(boot.eff, na.rm = TRUE), .groups = "drop")

# specify the shape of the arrows to be triangles
triangle <- cbind(x = c(1, 0, 0),
                  y = c(0, .5, -.5))

# add a line for the statistically unsupported path(s)
missing_paths <- full_stats %>%
  filter(!is.na(Estimate)) %>%
  group_by(resp.group, pred.group, pick(matches(".*\\.(x|y)"))) %>%
  mutate(group.sig = any(group.sig)) %>%
  filter(effect_type == "direct", !group.sig) %>%
  summarize(boot.eff = mean(boot.eff, na.rm = TRUE)) %>%
  distinct() %>%
  mutate(
    fix_col = (resp.group == "Infection" & pred.group == "Expression"),
    across(matches(".*\\.(x|y)"), ~if_else(.fix_col, .x*1.5, .x))
  )

# build the conceptual diagram
var_lookup %>%
  ggplot(aes(x = grp.x, y = grp.y, label = group)) +
  geom_arrow_segment(
    data = concept_data, inherit.aes = FALSE,
    aes(x = pred.grp.x, y = pred.grp.y, xend = resp.grp.x, yend = resp.grp.y,
        linewidth = abs(boot.eff), col = boot.eff > 0),
    resect_head = 11.5, length_head = 2, stroke_color = "black",
    arrow_head = triangle
  ) +
  geom_arrow_segment(
    data = missing_paths, inherit.aes = FALSE,
    aes(x = pred.grp.x, y = pred.grp.y, xend = resp.grp.x, yend = resp.grp.y),
    color = "grey50", resect_head = 11.5, length_head = 2,
    linewidth = 2, linetype = "dashed", arrow_head = triangle
  ) +
  geom_point(size = 30, shape = 21, fill = "white", color = "black") +
  geom_text() +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    legend.position = "none"
  ) +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(expand = c(0.1, 0)) +
  scale_y_continuous(expand = c(0.2, 0)) +
  scale_linewidth_continuous(range = c(2, 9))


## ---- OLDER ----

# group_lookup <- var_lookup %>%
#   select(group, grp_lab) %>% distinct() %>%
#   mutate(
#     coords = case_when(
#       grepl("expression", group, ignore.case = TRUE) ~ tibble(x = -1, y = 2),
#       grepl("Infection", group) ~ tibble(x = 2, y = 1),
#       grepl("Parasitism", group) ~ tibble(x = 0, y = 0.1),
#       grepl("Phenotype", group) ~ tibble(x = -1, y = -2),
#       grepl("Behavior", group) ~ tibble(x = -2, y = 0),
#       grepl("Environment", group) ~ tibble(x = 2, y = -1),
#       .default = tibble(x = NA, y = NA)
#     )
#   ) %>% unnest(coords)

# # create new save directory if it doesn't exist
# if (!dir.exists("infection-modeling/data/MS-tables/")) {
#   dir.create("infection-modeling/data/MS-tables/", recursive = TRUE)
# }
#
# # Read and clean the mouse data
# Peros <- readRDS("infection-modeling/data/peromyscus-model-data.rds") %>%
#   mutate(Species = updated_taxa) %>%
#   filter(!is.na(Species)) %>%
#   mutate(
#     across(c(siteID, plotID, year, iid, Species),
#            ~as.factor(.x)) # factor vars
#   )
#
# # Read and reformat SEM objects
# fullSEM_model_objects <- readRDS(
#   "infection-modeling/data/model-objects/simplified-SEM-objects.rds"
# )
#
# # extract the fit object
# full_psem <- fullSEM_model_objects$sem_mod$fit
#
# # reformat the tables
# full_psem$dTable <- full_psem$dTable %>%
#   data.frame() %>% rename(sig = "Var.6") %>% tibble()
# full_psem$coefficients <- full_psem$coefficients %>%
#   data.frame() %>% rename(sig = "Var.9") %>% tibble()

# # join these alternate variable names into the coefficient table
# full_psem$coefficients <- full_psem$coefficients %>%
#   left_join(
#     var_lookup %>%
#       select(
#         Response = var, clean_resp = clean_name, resp_lab = label,
#         resp_grp = group, resp_grplab = grp_lab
#       ), by = "Response"
#   ) %>%
#   left_join(
#     var_lookup %>%
#       select(
#         Predictor = var, clean_pred = clean_name, pred_lab = label,
#         pred_grp = group, pred_grplab = grp_lab
#       ), by = "Predictor"
#   )

# # Construct the cumulative effects table
# full_brkdwn <- readRDS(
#   file.path(
#     "infection-modeling/data/model-objects",
#     "cumulative-effects-table_simplified-SEM.rds"
#   )
# ) %>%
#   rename(Response = To, Predictor = From) %>%
#   left_join(
#     var_lookup %>%
#       select(
#         Response = var, clean_resp = clean_name, resp_lab = label,
#         resp_grp = group, resp_grplab = grp_lab
#       ), by = "Response"
#   ) %>%
#   left_join(
#     var_lookup %>%
#       select(
#         Predictor = var, clean_pred = clean_name, pred_lab = label,
#         pred_grp = group, pred_grplab = grp_lab
#       ), by = "Predictor"
#   )
