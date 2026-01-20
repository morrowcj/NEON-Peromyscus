## ---- Introduction ----

# This is a script to create the high-resolution figures for the
## manuscript.

## ---- Packages and functions ----

# Load needed functions
library(tidyverse)
library(DiagrammeR)
library(ggarrow)
library(lme4)
library(semEff)
library(piecewiseSEM)
source("infection-modeling/R/functions/diagram_psem.R")

## ---- Data ----

# Read and clean the data
Peros <- readRDS("infection-modeling/data/peromyscus-model-data.rds") %>%
  mutate(
    Species = updated_taxa, species = Species,
    # clim_PC1 = -1*clim_PC1 # reverse for increase with temp
  ) %>% filter(!is.na(Species)) %>%
  mutate(
    across(c(siteID, plotID, year, iid, Species), ~as.factor(.x)) # factor vars
  )

### PSEM ###

# Load and clean the SEM (psem)
# fullSEM_model_objects <- readRDS(
#   "infection-modeling/data/model-objects/simplified-SEM-objects.rds"
# )
fullSEM_model_objects <- readRDS(
  file.path("infection-modeling/data/model-objects",
            "unscaled-peros_SEM-objects.rds")
)

# extract the model list
modlist <- fullSEM_model_objects$component_mods

# extract the fit object
full_psem <- fullSEM_model_objects$sem_mod$fit

# # clean the d-seperation table (not used)
# full_psem$dTable <- full_psem$dTable %>%
#   data.frame() %>% rename(sig = "Var.6") %>% tibble()
#
# # clean the coefficient table
# full_psem$coefficients <- full_psem$coefficients %>%
#   data.frame() %>% rename(sig = "Var.9") %>% tibble()

species_SEM_objects <- readRDS(
  "infection-modeling/data/model-objects/unscaled-species_SEM-objects.rds"
)

## semEFF ##

# load full model statistics (semEff)
full_stats <- readRDS(
  "infection-modeling/data/model-objects/SEM-statistics-all.rds"
) %>%
  # reverse all clim_PC1 effects to be more intuitive (+ is warm and wet)
  mutate(
    boot.upr.diff = abs(boot.ci.up - boot.eff),
    boot.lwr.diff = abs(boot.eff - boot.ci.low),
    boot.eff = if_else(Predictor == "clim_PC1", boot.eff*(-1), boot.eff),
    boot.bias = if_else(Predictor == "clim_PC1", boot.bias*(-1), boot.bias),
    boot.ci.low = if_else(
      Predictor == "clim_PC1", boot.eff - boot.upr.diff, boot.ci.low
    ),
    boot.ci.up = if_else(
      Predictor == "clim_PC1", boot.eff + boot.lwr.diff, boot.ci.up
    ),
    Estimate = if_else(Predictor == "clim_PC1", Estimate*(-1), Estimate),
    boot.eff.mean = if_else(
      Predictor == "clim_PC1", boot.eff.mean*(-1), boot.eff.mean
    )
  ) %>% select(-any_of(c("boot.upr.diff", "boot.lwr.diff")))

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
species_stats <- readRDS(
  "infection-modeling/data/model-objects/SEM-statistics-species.rds"
) %>%
  # reverse all clim_PC1 effects to be more intuitive (+ is warm and wet)
  mutate(
    boot.upr.diff = abs(boot.ci.up - boot.eff),
    boot.lwr.diff = abs(boot.eff - boot.ci.low),
    boot.eff = if_else(Predictor == "clim_PC1", boot.eff*(-1), boot.eff),
    boot.bias = if_else(Predictor == "clim_PC1", boot.bias*(-1), boot.bias),
    boot.ci.low = if_else(
      Predictor == "clim_PC1", boot.eff - boot.upr.diff, boot.ci.low
    ),
    boot.ci.up = if_else(
      Predictor == "clim_PC1", boot.eff + boot.lwr.diff, boot.ci.up
    ),
    Estimate = if_else(Predictor == "clim_PC1", Estimate*(-1), Estimate),
    boot.eff.mean = if_else(
      Predictor == "clim_PC1", boot.eff.mean*(-1), boot.eff.mean
    )
  ) %>% select(-any_of(c("boot.upr.diff", "boot.lwr.diff")))

species_stats <- species_stats %>%
  mutate(
    psem.sig = P.Value <= 0.1, # raw model stats
    boot.sig = boot.p <= 0.1, # pseudo-p-value, using orig. coef. and boot SE
    boot.sig.mean = boot.p.mean <= 0.1, # pseudo-P using boot coef. and SE
    ci.sig = !(boot.ci.low < 0 & boot.ci.up > 0), # based on 90% CI
    joint_sig = if_else(effect_type == "direct", psem.sig, ci.sig) # combined
  ) %>%
  group_by(Response, Predictor, Species) %>%
  mutate(group.sig = any(joint_sig)) %>%
  ungroup()

## ---- Lookup table and metadata ----

# Create a lookup table for the variables
var_lookup <- tribble(
  ~ID, ~var, ~clean_name, ~label,
  ~group, ~grp_lab,
  1, "expr_PC2", "Tolerance", "Tolerance\nexpression",
  "Expression", "Immunity",
  2, "Bb_infected", "Infection", "Infection\nstatus",
  "Infection", "Infection",
  3, "expr_PC1", "Resistance", "Resistance\nexpression",
  "Expression", "Immunity",
  4, "ticks_attached", "Parasitism", "Parasitism\n(ticks)",
  "Parasitism", "Parasitism\n(ticks)",
  5,"capprop_shift", "Cap. time \U0394", "Capture\ntime \U0394",
  "Behavior", "Behavior\n(timing)",
  6, "cap_prop_night", "Cap. time", "Capture\ntime",
  "Behavior", "Behavior\n(timing)",
  7, "weight", "Weight", "Weight",
  "Phenotype", "Individual\nstate",
  8, "sex_mature", "Reproductive", "Reproductive\nmaturity",
  "Phenotype", "Individual\nstate",
  9, "sex_male", "Sex", "Sex",
  "Phenotype", "Individual\nstate",
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

species_stats <- species_stats %>%
  left_join(resp_lookup, by = "Response") %>%
  right_join(pred_lookup, by = "Predictor")

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
    resect_head = 10, length_head = 2, stroke_color = "black",
    arrow_head = triangle) +
  geom_arrow_segment(
    data = missing_paths, inherit.aes = FALSE,
    aes(x = pred.grp.x, y = pred.grp.y, xend = resp.grp.x, yend = resp.grp.y),
    color = "grey65", resect_head = 12, length_head = NULL,
    linewidth = 1, linetype = "longdash", arrow_head = triangle,
    # stroke_color = NULL
  ) +
  geom_point(size = 25, shape = 21, fill = "white", color = "black") +
  geom_text(size = 3) +
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

## ---- Fig. 3 ----

nudge_factor = 0.125
point_stroke = 1.3
point_size = 1.8
eb_end_width = 0.2
eb_stroke = 0.8

full_stats %>%
  mutate(Species = "both") %>%
  filter(
    Response %in% c("Bb_infected", "ticks_attached", "expr_PC1", "expr_PC2"),
    effect_type %in% c("total")
  ) %>%
  bind_rows(
    species_stats %>%
      filter(
        effect_type == "total",
        Response %in% c("Bb_infected", "ticks_attached", "expr_PC1", "expr_PC2")
      )
  ) %>%
  # complete(Species, Response, Predictor, effect_type) %>%
  mutate(
    Species = factor(Species, levels = c("PELE", "both", "PEMA"))
  ) %>%
  ggplot(
    aes(
      y = Predictor, x = boot.eff, xmin = boot.ci.low, xmax = boot.ci.up,
      color = Species
    )
  ) +
  facet_wrap(~Response, nrow = 1, scales = "free_x") +
  geom_vline(xintercept = 0, color = "grey50", linetype = "longdash") +
  geom_pointrange(
    aes(
      shape = interaction(Species, joint_sig), linetype = joint_sig
      ), fill = "white", stroke = point_stroke, linewidth = eb_stroke,
    size = 0.7, position = position_dodge(0.6)
  ) +
  theme_bw() +
  scale_shape_manual(
    breaks = c(
      "PELE.TRUE", "PELE.FALSE",
      "both.TRUE", "both.FALSE",
      "PEMA.TRUE", "PEMA.FALSE"
    ),
    values = c(
      17, 24,
      16, 21,
      15, 22
    )
  ) +
  scale_color_manual(
    breaks = c("PELE", "both", "PEMA"),
    labels = c("PELE (spec.)", "both (full)", "PEMA (spec.)"),
    values = c("orange", "black", "cornflowerblue")
  ) +
  scale_linetype_manual(
    breaks = c(TRUE, FALSE), values = c("solid", "longdash")
  ) +
  # scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5)) +
  scale_x_continuous(breaks = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6)) +
  labs(
    x = "Total standardized effect (± 90% CI)",
    shape = "Group", linetype = "Group", color = "Species (model)"
  )


# full_stats %>%
#   mutate(Species = "both") %>%
#   filter(
#     Response %in% c("Bb_infected", "ticks_attached", "expr_PC1", "expr_PC2"),
#     effect_type %in% c("direct", "total")
#   ) %>%
#   complete(Response, Predictor, effect_type) %>%
#   pivot_wider(
#     names_from = effect_type,
#     values_from = c(boot.eff, boot.ci.low, boot.ci.up, boot.eff.mean, joint_sig)
#   ) %>%
#   mutate(
#     boot.eff_direct = if_else(
#       is.na(boot.eff_direct) & !is.na(boot.eff_total), 0, boot.eff_direct
#     )
#     # boot.eff_total = if_else(
#     #   is.na(boot.eff_total) & !is.na(boot.eff_direct), 0, boot.eff_total
#     # )
#   ) %>%
#   ggplot(aes(y = Predictor)) +
#   facet_wrap(~Response, nrow = 1) +
#   geom_vline(xintercept = 0, color = "grey50", linetype = "longdash") +
#   ## Total effects
#   geom_errorbar(
#     aes(
#       xmin = boot.ci.low_total, xmax = boot.ci.up_total, width = eb_end_width,
#       color = "total", linetype = joint_sig_total
#     ), linewidth = eb_stroke,
#     position = position_nudge(y = nudge_factor)
#   ) +
#   geom_point(
#     aes(
#       x = boot.eff_total, shape = interaction(Species, joint_sig_total),
#       color = "total"), fill = "white", stroke = point_stroke,
#     size = point_size, position = position_nudge(y = nudge_factor)
#   ) +
#   ## Direct effects
#   geom_errorbar(
#     aes(
#       xmin = boot.ci.low_direct, xmax = boot.ci.up_direct, width = eb_end_width,
#       color = "direct", linetype = joint_sig_direct
#     ), linewidth = eb_stroke,
#     position = position_nudge(y = -nudge_factor)
#   ) +
#   geom_point(
#     aes(
#       x = boot.eff_direct, shape = interaction(Species, joint_sig_direct),
#       color = "direct"), fill = "white", stroke = point_stroke,
#     size = point_size, position = position_nudge(y = -nudge_factor)
#   ) +
#   ## aesthetics
#   theme_bw() +
#   labs(
#     linetype = "Significance", shape = "Species",
#     color = "Effect type"
#   ) +
#   scale_linetype_manual(
#     values = c("solid", "longdash"), breaks = c(TRUE, FALSE),
#     labels = c("P ≤ 0.1", "n.s.")
#   ) +
#   scale_shape_manual(
#     values = c(19, 21), breaks = c("both.TRUE", "both.FALSE"),
#     labels = rep("both", 2)
#   )

# full_stats %>%
#   mutate(Species = "both") %>%
#   filter(
#     Response %in% c("Bb_infected", "ticks_attached", "expr_PC1", "expr_PC2"),
#     effect_type %in% c("direct", "total")
#   ) %>%
#   complete(Response, Predictor, effect_type) %>%
#   ggplot(
#     aes(
#       x = boot.eff, xmin = boot.ci.low, xmax = boot.ci.up, y = Predictor,
#       shape = interaction(joint_sig, Species),
#       fill = interaction(joint_sig, Species, effect_type),
#       col = interaction(Species, effect_type),
#       linetype = interaction(joint_sig, Species)
#     )
#   ) +
#   facet_wrap(~Response) +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
#   geom_point() +
#   geom_errorbar(width = 1/3, , position = position_dodge(0.5)) +
#   theme_bw()

## ---- Fig. X: Species infection and parasitism comparison ----

species_stats %>%
  filter(
    Response %in% c("Bb_infected", "ticks_attached"),
    effect_type == "total"
  ) %>%
  ggplot(
    aes(
      y = Predictor, x = boot.eff.mean, xmin = boot.ci.low, xmax = boot.ci.up,
      shape = interaction(joint_sig, Species),
      fill = interaction(joint_sig, Species),
      col = interaction(joint_sig, Species),
      linetype = interaction(joint_sig, Species)
    )
  ) +
  facet_wrap(~Response, scales = "free_x") +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  # geom_col(
  #   position = position_dodge2(0.5)
  # ) +
  geom_pointrange(
    position = position_dodge2(0.5), stroke = 1.5, size = 0.7,
    linewidth = 1
  ) +
  theme_bw() +
  theme(
    legend.position = "outside", legend.position.inside = c(2/3, 0.9),
    legend.justification = c(0.5, 1),
    legend.background = element_rect(color = 'black'),
    legend.key.width = unit(4, "lines"),
    panel.grid.major.y = element_blank()

  ) +
  labs(
    x = "Total effect (mean ± 90% CI)",
    fill = NULL, shape = NULL, col = NULL, linetype = NULL
  ) +
  scale_shape_manual(values = c(21, 21, 24, 24)) +
  scale_fill_manual(values = c("white", "orange", "white", "cornflowerblue")) +
  scale_color_manual(
    values = c("orange", "orange", "cornflowerblue", "cornflowerblue")
  ) +
  scale_linetype_manual(values = c("dashed", "solid", "dashed", "solid")) +
  scale_x_continuous(breaks = seq(-.5, 0.5, by = 0.25)) +
  scale_y_discrete(minor_breaks = seq(1.5, 5.5, by = 1))




## ---- Table S1: SEM stats ----

# Build the table of SEM effects

tab_S1 <- full_stats %>% filter(effect_type == "direct") %>%
  left_join(
    full_psem$R2 %>%
      select(Response, R2.marg = Marginal, R2.cond = Conditional),
    by = "Response"
  ) %>%
  arrange(resp.group, Response, pred.group, Predictor) %>%
  mutate(
    new_resp = paste0(resp.clean_name, " (", resp.group, ")"),
    new_pred = paste0(pred.clean_name, " (", pred.group, ")"),
    DF = round(DF),
    P = Hmisc::format.pval(P.Value, digits = 3, eps = 0.001),
    # across(c(Estimate, boot.eff, Std.Error),
    #        ~sprintf("%.3f", round(.x, digits = 3)))
  ) %>%
  select(
    Response = new_resp, R2.marg, R2.cond,
    Predictor = new_pred,
    Estimate,
    DF, SE = Std.Error, P,
    Std.Estimate = boot.eff
  )

tab_S1

# save this object
saveRDS(
  tab_S1, "infection-modeling/data/MS-tables/full-SEM-coef-tab.rds"
)

# save as a csv
write.csv(
  tab_S1, "infection-modeling/data/MS-tables/full-SEM-coef-tab.csv",
  row.names = FALSE,
)

## ---- Table S2: SEM bootstrap table ----

# build the table of bootstrap statistics
tab_S2 <- full_stats %>%
  filter(!effect_type %in% c("mediators")) %>%
  arrange(resp.group, Response, pred.group, Predictor, effect_type) %>%
  mutate(
    pm_val = boot.ci.up - boot.eff.mean,
    # across( ## rounding (needed?)
    #   c(boot.eff.mean, boot.SE, boot.ci.low, boot.ci.up, pm_val),
    #   ~sprintf("%.3f", round(.x, 3))
    # ),
    boot.bias.str = sprintf("%.1e", boot.bias),
    CI = paste0("(", boot.ci.low, ", ", boot.ci.up, ")")
  ) %>%
  select(
    Response = resp.clean_name, Predictor = pred.clean_name,
    Type = effect_type, Mean = boot.eff.mean, SE = boot.SE,
    CI_pm = pm_val, Bias = boot.bias, Bias.str = boot.bias.str
  )  %>%
  pivot_wider(
    names_from = Type, values_from = -c(Response, Predictor, Type),
    names_glue = "{Type}.{.value}"
  ) %>%
  relocate(
    c(starts_with("direct."), starts_with("indirect"), starts_with("total")),
    .after = -1
  )

# save this object
saveRDS(
  tab_S2, "infection-modeling/data/MS-tables/full-SEM-boot-tab.rds"
)

# index of columns with scientific notation to preserve ast strings
str_col_inx <- grep(".*\\.str", names(tab_S2))

tab_S2 %>% select(all_of(str_col_inx)) %>% head()

str = "4.3e-04"

convert_scinote <- function(str){
  pattern = "(-*\\d\\.\\d{1,})e(-*\\d{1,})"
  val = gsub(pattern, "\\1", str)
  pow = gsub(pattern, "\\2", str)
  out = paste0(val, "\U00D7", "10^", as.integer(pow))
  if_else(is.na(str), NA, out)
}

# save as a csv
tab_S2 %>%
  mutate(
    across(
      contains("Bias.str"), ~ifelse(is.na(.x), NA, convert_scinote(.x))
    )
  ) %>%
  write.csv(
    file = "infection-modeling/data/MS-tables/full-SEM-boot-tab.csv",
    row.names = FALSE, quote = str_col_inx, fileEncoding = "UTF-8"
  )

## ---- Table SX: random effects table ----

ranef_table <- lapply(names(modlist), function(x){
  mod = modlist[[x]]
  try(
    data.frame(VarCorr(mod)) %>% select(-c(var1, var2)) %>%
      mutate(Response = x)
  )
}) %>% bind_rows() %>%
  relocate(Response, .before = 1)

write.csv(
  ranef_table,
  file = "infection-modeling/data/MS-tables/SEM-random-effects-table.csv",
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


## ---- Table S3: Species SEM ----

# R-squared values
species_R2 <- species_SEM_objects$pele$sem_mod$fit$R2 %>%
  mutate(Species = "PELE") %>%
  bind_rows(
    species_SEM_objects$pema$sem_mod$fit$R2 %>%
      mutate(Species = "PEMA")
  ) %>%
  select(Response, Species, R2.marg = Marginal, R2.cond = Conditional)

# table S3
tab_S3 <- species_R2 %>%
  right_join(
    species_stats %>% filter(effect_type == "direct"),
    by = c("Response", "Species")
  )  %>%
  arrange(resp.group, Response, Species, pred.group, Predictor) %>%
  mutate(
    new_resp = paste0(resp.clean_name, " (", resp.group, ")"),
    new_pred = paste0(pred.clean_name, " (", pred.group, ")"),
    DF = round(DF),
    P = Hmisc::format.pval(P.Value, digits = 3, eps = 0.001),
  ) %>%
  select(
    Response = resp.clean_name, Species, R2.marg, R2.cond,
    Predictor = pred.clean_name,
    Effect = boot.eff, SE = boot.SE, DF = DF,
    CI.low = boot.ci.low, CI.up = boot.ci.up
  )

# save this object
saveRDS(
  tab_S3, "infection-modeling/data/MS-tables/species-SEM-coef-tab.rds"
)

# save as a csv
write.csv(
  tab_S3, "infection-modeling/data/MS-tables/species-SEM-coef-tab.csv",
  row.names = FALSE,
)

## ---- Figure SY: Species trait comparison ----

# subset data
sp_trait_data <- Peros %>%
  group_by(siteID) %>%
  mutate(
    has_both = all(c("PELE", "PEMA") %in% species)
  ) %>% ungroup() %>%
  select(
    site = siteID, year, species,
    parasitism = ticks_attached, infection = Bb_infected,
    sex = sex_male, reproductive = sex_mature, weight,
    cap_time = cap_prop_night, cap_shift = capprop_shift,
    resistance = expr_PC1, tolerance = expr_PC2, has_both
  )

# list of traits
traits = names(
  sp_trait_data %>% select(-c(site, year, species, has_both))
)

# fit lmms to compare species across sites
trait_mods <- lapply(traits, function(v){
  formy = paste(v, "~ 0 + species + (1|site)")
  lmer(formy, data = sp_trait_data %>% filter(has_both))
})
names(trait_mods) <- traits

# Get estimated marginal means for each species x variable
trait_EMMs <- lapply(traits,
       function(x) {
         mod = trait_mods[[x]]
         emm <- emmeans::emmeans(mod, ~species)
         prs <- pairs(emm)
         p = prs %>% data.frame() %>% pull(p.value)
         cld <- emmeans:::cld.emmGrid(emm)
         cld %>% data.frame() %>% mutate(P = p, response = x)
       }
) %>% bind_rows() %>%
  mutate(
    CI_lwr = if_else(!is.na(asymp.LCL), asymp.LCL, lower.CL),
    CI_upr = if_else(!is.na(asymp.UCL), asymp.UCL, upper.CL)
  ) %>%
  select(-c(starts_with("asymp"), lower.CL, upper.CL))


# get the long-form trait data for plotting
sp_long_traits <- sp_trait_data %>%
  filter(has_both) %>% select(-has_both) %>%
  pivot_longer(
    -c(site, year, species),
    names_to = "variable", values_to = "value"
  ) %>% filter(complete.cases(.))

# build a plot
sp_long_traits %>%
  ggplot(aes(y = site, x = value, color = species)) +
  facet_wrap(~variable, scales = "free") +
  stat_summary(
    fun.data = mean_cl_normal,
    position = position_dodge(0.4)
  ) +
  stat_summary(
    fun.data = mean_cl_normal,
    aes(group = species, y = "Avg."), shape = 17,
    show.legend = FALSE
  )
  # geom_pointrange(
  #   data = trait_EMMs %>% rename(variable = response),
  #   aes(y = "Avg.", x = emmean, xmin = emmean - SE, xmax = emmean + SE,
  #       color = species),
  #   show.legend = FALSE
  # )

## ---- Lat Long comparison ----

## ---- Effect comparisons (within population) ----

eff_props <- full_stats %>%
  filter(Response == "Bb_infected", effect_type %in% c("total", "direct")) %>%
  select(Predictor, effect_type, boot.eff) %>% group_by(effect_type) %>%
  mutate(
    abs_eff = abs(boot.eff),
    eff_sum = sum(abs_eff),
    eff_prop = abs_eff / eff_sum,
    groupy = case_when(
      Predictor %in% c("ticks_attached") ~ "parasitism",
      Predictor %in% c("clim_PC1", "wthr_PC1") ~ "environment",
      .default = "individual"
    )
  )

(group_eff_props <- eff_props %>%
  group_by(effect_type, groupy) %>%
  summarize(effect_prop = sum(eff_prop, na.rm = TRUE)) %>%
  pivot_wider(names_from = effect_type, values_from = effect_prop))

group_eff_props %>%
  mutate(direct = direct*0.13, total = total*0.13)

## ---- Look for dilution effects ----

mammals <- readRDS("neon-data/data/eight_sites/mammal-trap-data.rds")
mam.vars <- mammals$variables_10072
mammals <- mammals$mam_pertrapnight

(div.tab <-
  mammals %>%
  filter(!is.na(taxonID), siteID %in% unique(Peros$siteID)) %>%
  select(plotID, siteID, trapCoordinate, collectDate, taxonID, tagID) %>%
  mutate(
    collectDate = lubridate::ymd(collectDate),
    year = factor(lubridate::year(collectDate))
  ) %>%
  filter(year %in% c(2023, 2024)) %>%
  group_by(siteID, year) %>%
  mutate(
    nCaps = n(), richness = length(unique(taxonID))
  ) %>%
  group_by(taxonID, .add = TRUE) %>%
  mutate(
    Species_n = n(), Species_prop = Species_n/nCaps
  ) %>%
  ungroup()
)


dil.tab <- div.tab %>% select(siteID, year, richness) %>% distinct() %>%
  inner_join(
    Peros %>%
      filter(!is.na(Bb_infected)) %>%
      group_by(siteID, year) %>%
      summarize(
        n = n(),
        mean_inf = mean(Bb_infected, na.rm = TRUE),
        sd_inf = sd(Bb_infected, na.rm = TRUE)
      ),
    by = c("siteID", "year")
  )

dil.fm <- lm(mean_inf ~ richness + year, data = dil.tab)
car::Anova(dil.fm) # no significant dilution effects.

cor(dil.tab %>% select(richness, mean_inf))

  dil.tab %>%
    ggplot(aes(x = richness, y = mean_inf, col = siteID, shape = year)) +
    geom_point(size = 3) +
    geom_smooth(
      method = "lm", linetype = "dashed", fill = "grey80", aes(group = 1)
    ) +
    theme_bw() +
    labs(x = "Species richness", y = "Infection rate")
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
