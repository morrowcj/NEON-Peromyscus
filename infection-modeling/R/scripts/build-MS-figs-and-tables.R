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
  5,"capprop_shift", "Cap. time \U0394", "Capture\ntime \U0394",
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
  group_by(
    pred.group, pred.grp_lab, resp.group, resp.grp_lab,
    pick(matches(".*\\.(x|y)"))
  ) %>%
  summarize(boot.eff = mean(boot.eff, na.rm = TRUE), .groups = "drop") %>%
  mutate(resp.group = factor(resp.group, levels = rev(coords_tab$group)),
         pred.group = factor(pred.group, levels = rev(coords_tab$group))) %>%
  arrange(resp.group, pred.group)

# specify the shape of the arrows to be triangles
triangle <- cbind(x = c(1, 0, 0),
                  y = c(0, .5, -.5))

# add a line for the statistically unsupported path(s)
missing_paths <- tibble(resp.group = "Infection", pred.group = "Expression") %>%
  left_join(resp_lookup) %>%
  right_join(pred_lookup) %>%
  mutate(across(c(pred.grp.x, pred.grp.y, resp.grp.x, resp.grp.y), ~.x*1.1))

# build the conceptual diagram
var_lookup %>%
  ggplot(aes(x = grp.x, y = grp.y, label = grp_lab)) +
  geom_arrow_segment(
    data = concept_data, inherit.aes = FALSE,
    aes(x = pred.grp.x, y = pred.grp.y, xend = resp.grp.x, yend = resp.grp.y,
        linewidth = abs(boot.eff),
        col = boot.eff > 0
    ),
    resect_head = 11.5, length_head = 2, stroke_color = "black",
    arrow_head = triangle) +
  geom_arrow_segment(
    data = missing_paths, inherit.aes = FALSE,
    aes(x = pred.grp.x, y = pred.grp.y, xend = resp.grp.x, yend = resp.grp.y),
    color = "grey80", resect_head = 13.5, length_head = 2,
    linewidth = 1, linetype = "solid", arrow_head = triangle,
    stroke_color = "black"
  ) +
  geom_point(size = 30, shape = 21, fill = "white", color = "black") +
  geom_text(size = 3.3) +
  theme_classic() +
  ggnetwork::theme_blank() +
  theme(
    axis.line = element_blank(),
    legend.position = "none"
  ) +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(expand = c(0.1, 0)) +
  scale_y_continuous(expand = c(0.1, 0)) +
  scale_linewidth_continuous(range = c(1, 9)) +
  scale_color_manual(values = c("indianred1", "cornflowerblue"))

ggsave(
  filename = "infection-modeling/graphics/conceptual-SEM-full.png",
  width = 4.5, height = 4.5, dpi = 300
)

warning("Fig. 2 may need to use labels instead of groups")

## ---- Table S1: SEM stats ----

# Build the table of SEM effects

warning("NEED TO REARRANGE THE ROWS")

tab_S1 <- full_stats %>% filter(effect_type == "direct") %>%
  arrange(resp.group, Response, pred.group, Predictor) %>%
  mutate(
    new_resp = paste0(resp.clean_name, " (", resp.group, ")"),
    new_pred = paste0(pred.clean_name, " (", pred.group, ")"),
    DF = round(DF),
    P = Hmisc::format.pval(P.Value, digits = 3, eps = 0.001),
    across(c(Estimate, boot.eff, Std.Error),
           ~sprintf("%.3f", round(.x, digits = 3)))
  ) %>%
  select(
    Response = new_resp, Predictor = new_pred,
    Estimate, Std.Estimate = boot.eff,
    DF, SE = Std.Error, P,
  )
tab_S1

# save this object
saveRDS(
  tab_S1, "infection-modeling/data/MS-tables/full-SEM-coef-tab.rds"
)

# save as a csv
write.csv(
  tab_S1, "infection-modeling/data/MS-tables/full-SEM-coef-tab.csv",
  row.names = FALSE
)

## ---- Table SX: SEM bootstrap table ----

warning("NEED TO REARRANGE THE ROWS")
warning("NEED TO FINALIZE FORMAT")

# build the table of bootstrap statistics
tab_SX <- full_stats %>%
  filter(!effect_type %in% c("mediators")) %>%
  arrange(resp.group, Response, pred.group, Predictor, effect_type) %>%
  mutate(
    # new_resp = paste0(resp.clean_name, " (", resp.group, ")"),
    # new_pred = paste0(pred.clean_name, " (", pred.group, ")"),
    across(
      c(boot.eff.mean, boot.SE, boot.ci.low, boot.ci.up),
      ~sprintf("%.3f", round(.x, 3))
    ),
    boot.bias = sprintf("%.1e", boot.bias),
    CI = paste0("(", boot.ci.low, ", ", boot.ci.up, ")")
  ) %>%
  select(
    Response = resp.clean_name, Predictor = pred.clean_name,
    Type = effect_type, Mean = boot.eff.mean, SE = boot.SE, Bias = boot.bias,
    CI
  )  %>%
  pivot_wider(
    names_from = Type, values_from = c(Mean:CI),
  ) %>%
  relocate()

# save this object
saveRDS(
  tab_SX, "infection-modeling/data/MS-tables/full-SEM-boot-tab.rds"
)

# save as a csv
write.csv(
  tab_SX, "infection-modeling/data/MS-tables/full-SEM-boot-tab.csv",
  row.names = FALSE
)

## ---- Figure 2: Logistic correlations ----

# elongate the data for multi-panel plotting
long_peros <- Peros %>%
  filter(sex_male != 0.5, weight <= 50) %>% # remove ambiguous sex
  select(
    iid, cap_num, siteID, year, Bb_infected, ticks_attached,
    sex_male, sex_mature, weight, cap_prop_night, capprop_shift
  ) %>% distinct() %>%
  pivot_longer(
    cols = c(Bb_infected, ticks_attached), names_to = "Response",
    values_to = "resp_val"
  ) %>%
  pivot_longer(
    cols = c(sex_male:capprop_shift), names_to = "Predictor",
    values_to = "pred_val"
  ) %>%
  left_join(resp_lookup, by = "Response") %>%
  left_join(pred_lookup, by = "Predictor") %>%
  mutate(
    new_predlab = fct_rev(pred.label),
    sig_line = case_when(
      Response == "Bb_infected" & Predictor == "capprop_shift" ~ FALSE,
      resp.clean_name == "Infection" & pred.clean_name == "Sex" ~ FALSE,
      .default = TRUE),
    prob_resp = paste0("P(", resp.clean_name, ")") %>%
      factor(levels = paste0("P(", levels(resp.clean_name), ")"))
  )

# Add GLM p values
long_peros <- long_peros %>%
  group_by(Response, Predictor) %>%
  mutate(
    glm.pval = anova(
      glm(resp_val ~ pred_val, family = "binomial")
    )$`Pr(>Chi)`[2], glm.sig = glm.pval <= 0.05
  )

# break data into continuous and binary predictor variables

## continuous
long_cont <- long_peros %>%
  filter(!Predictor %in% c("sex_male", "sex_mature"))

## binary (summaries)
long_bin <- long_peros %>%
  filter(Predictor %in% c("sex_male", "sex_mature"))

# Panel data
panel_dat <- long_peros %>%
  select(prob_resp, new_predlab) %>% distinct() %>%
  arrange(prob_resp, new_predlab) %>%
  ungroup() %>%
  mutate(panel_lab = LETTERS[row_number()])

# Create the plot
ggplot(data = long_bin, aes(x = pred_val, y = resp_val)) +
  facet_grid(prob_resp ~ new_predlab, scales = "free_x", switch = "both") +
  geom_smooth(
    data = long_bin, method = "glm", method.args = list(family = "binomial"),
    aes(col = "logistic reg.", linetype = glm.sig), se = TRUE, col = "black",
    fill = "khaki3"
  ) +
  stat_summary(
    data = long_bin, fun.data = mean_cl_normal, geom = "pointrange",
    size = 1/4, linewidth = 0.75,
    aes(col = "mean\n(\U00B1 95% CI)")
  ) +
  geom_smooth(
    data = long_cont,
    method = "glm", method.args = list(family = "binomial"),
    aes(col = "logistic reg.", linetype = glm.sig), se = TRUE, col = "black",
    fill = "khaki3"
  ) +
  geom_point(
    data = long_cont,
    aes(col = "raw data"), size = 0.8
  ) +
  geom_label(
    data = panel_dat, size = 3, fontface = "bold",
    fill = NA, col = "black", label.size = NA,
    aes(x = -Inf, y = Inf, label = panel_lab), hjust = -0.1, vjust = 1.5
  ) +
  theme_bw() +
  theme(
    strip.placement = "outside", strip.background = element_blank(),
    panel.spacing.y = unit(0.1, "lines"),
    panel.spacing.x = unit(0.1, "lines"),
    legend.position = "inside",
    legend.position.inside = c(0.3, 0.58),
    legend.justification = c(0.5, 0.5),
    legend.key = element_blank(),
    legend.key.width = unit(0.75, "lines"),
    legend.background = element_rect(color = "black"),
    legend.margin = margin(l = 0, r = 3, t = 0, b = 0),
    strip.text.x = element_text(vjust = 1),
    strip.clip = "on",
    strip.switch.pad.grid = unit(-0.05, "lines")
  ) +
  labs(x = NULL, y = NULL, col = NULL) +
  scale_x_continuous(breaks = c(0, 1, 10, 25, 40),
                     minor_breaks = c(-0.5, 0.5, 17.5, 32.5)) +
  scale_color_manual(
    breaks = c("mean\n(\U00B1 95% CI)", "raw data"),
    values = c("cornflowerblue", "grey50")
  ) +
  scale_linetype_manual(
    breaks = c(TRUE, FALSE), values = c("solid", "dashed")
  ) +
  guides(linetype = "none")

# save the file
ggsave(
  filename =  "infection-modeling/graphics/parasitism-infection-fig.png",
  width = 6, height = 6*0.5, dpi = 300
)

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
