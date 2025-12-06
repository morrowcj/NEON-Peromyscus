## ---- Introduction ----

# This is a script to create the high-resolution figures for the
## manuscript.

## ---- Setup ----

# Load needed functions
library(tidyverse)
library(DiagrammeR)
source("infection-modeling/R/functions/diagram_psem.R")

# create new save directory if it doesn't exist
if (!dir.exists("infection-modeling/data/MS-tables/")) {
  dir.create("infection-modeling/data/MS-tables/", recursive = TRUE)
}

# Read and clean the data
Peros <- readRDS("infection-modeling/data/peromyscus-model-data.rds") %>%
  mutate(
    Species = updated_taxa
  ) %>% filter(!is.na(Species)) %>%
  mutate(
    across(c(siteID, plotID, year, iid, Species), ~as.factor(.x)) # factor vars
  )

# Read and reformat SEM objects
fullSEM_model_objects <- readRDS(
  "infection-modeling/data/model-objects/simplified-SEM-objects.rds"
)
## extract the fit object
full_mod <- fullSEM_model_objects$sem_mod$fit
## reformat the tables
full_mod$dTable <- full_mod$dTable %>%
  data.frame() %>% rename(sig = "Var.6") %>% tibble()
full_mod$coefficients <- full_mod$coefficients %>%
  data.frame() %>% rename(sig = "Var.9") %>% tibble()

# Create a lookup table for the veriables
var_lookup <- tribble(
  ~rank, ~var, ~clean_name, ~label,
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
  mutate(across(-c(rank), ~factor(.x, levels = unique(.x))))

group_lookup <- var_lookup %>%
  select(group, grp_lab) %>% distinct() %>%
  mutate(
    coords = case_when(
      grepl("expression", group, ignore.case = TRUE) ~ tibble(x = -1, y = 2),
      grepl("Infection", group) ~ tibble(x = 2, y = 1),
      grepl("Parasitism", group) ~ tibble(x = 0, y = 0.1),
      grepl("Phenotype", group) ~ tibble(x = -1, y = -2),
      grepl("Behavior", group) ~ tibble(x = -2, y = 0),
      grepl("Environment", group) ~ tibble(x = 2, y = -1),
      .default = tibble(x = NA, y = NA)
    )
  ) %>% unnest(coords)

var_lookup <- var_lookup %>%
  left_join(group_lookup, by = c("group", "grp_lab"))

# join these alternate variable names into the coefficient table
full_mod$coefficients <- full_mod$coefficients %>%
  left_join(
    var_lookup %>%
      select(
        Response = var, clean_resp = clean_name, resp_lab = label,
        resp_grp = group, resp_grplab = grp_lab
      ), by = "Response"
  ) %>%
  left_join(
    var_lookup %>%
      select(
        Predictor = var, clean_pred = clean_name, pred_lab = label,
        pred_grp = group, pred_grplab = grp_lab
      ), by = "Predictor"
  )

# Construct the cumulative effects table
full_brkdwn <- readRDS(
  file.path(
    "infection-modeling/data/model-objects",
    "cumulative-effects-table_simplified-SEM.rds"
  )
) %>%
  rename(Response = To, Predictor = From) %>%
  left_join(
    var_lookup %>%
      select(
        Response = var, clean_resp = clean_name, resp_lab = label,
        resp_grp = group, resp_grplab = grp_lab
      ), by = "Response"
  ) %>%
  left_join(
    var_lookup %>%
      select(
        Predictor = var, clean_pred = clean_name, pred_lab = label,
        pred_grp = group, pred_grplab = grp_lab
      ), by = "Predictor"
  )

## ---- Figure 1 (conceptual scenarios) ----

# This figure is an illustration...
warning("Need to reference the illustration location here")

## ---- Figure 2 (conceptual SEM) ----

group_brkdwn <- full_brkdwn %>%
  group_by(Response, resp_grp, resp_grplab, pred_grp, pred_grplab) %>%
  summarize(
    mean_effect = mean(new_direct, na.rm = TRUE),
    any_effect = any(P.Value <= 0.1),
    .groups = "drop"
  ) %>%
  group_by(resp_grp, resp_grplab, pred_grp, pred_grplab) %>%
  summarize(
    mean_effect = mean(mean_effect, na.rm = TRUE),
    any_effect = any(any_effect),
    .groups = "drop"
  ) %>%
  mutate(
    any_effect = replace_na(any_effect, FALSE)
  )

group_nodes_edge <- group_brkdwn %>%
  filter(any_effect | (resp_grp == "Infection" & pred_grp == "Expression")) %>%
  build_nodes_edges(
    from_col = "pred_grplab", to_col = "resp_grplab", val_col = "mean_effect"
  )

group_nodes_edge$edges <- group_nodes_edge$edges %>%
  filter(From != To) %>%
  mutate(
    color = if_else(any_effect, "black", "grey80"),
    penwidth = scales::rescale(Val, to = c(1, 4)),
    penwidth = if_else(is.na(penwidth), 1, penwidth),
    style = if_else(any_effect, "solid", "dashed"),
  )

group_nodes_edge$nodes <- group_nodes_edge$nodes %>%
  mutate(
    fill_color = if_else(grepl("Infection", name), "thistle1", "white")
  ) %>%
  left_join(
    group_lookup %>% select(name = grp_lab, group, x, y),
    by = "name"
  )

full_psem_plot <- build_psem_plot(
  group_nodes_edge, render = FALSE, attr_theme = "bt"
) %>%
  add_global_graph_attrs("layout", "fdp", "graph") %>%
  set_node_attrs("width", 0.9) %>%
  set_node_attrs("fontcolor", "black") %>%
  set_node_attrs("color", "black")

render_graph(full_psem_plot)

full_psem_plot %>%
  export_graph(
    "infection-modeling/graphics/conceptual-SEM-full.png",
    width = 4.5*300, height = 4.5*300
  )

## ---- Figure 3 (raw parasitism/infection effects) ----
# elongate the data for multi-panel plotting
long_peros <- Peros %>%
  filter(sex_male != 0.5, weight <= 50) %>% # remove ambiguous sex
  select(
    iid, cap_num, siteID, year,
    Bb_infected, ticks_attached, sex_male, sex_mature, weight,
    cap_prop_night, capprop_shift
  ) %>% distinct() %>%
  # filter(complete.cases(.)) %>%
  pivot_longer(
    cols = c(Bb_infected, ticks_attached),
    names_to = "Response", values_to = "resp_val"
  ) %>%
  pivot_longer(
    cols = c(sex_male:capprop_shift),
    names_to = "Predictor", values_to = "pred_val"
  ) %>%
  left_join(
    var_lookup %>%
      select(
        Response = var, clean_resp = clean_name, resp_lab = label,
        resp_grp = group, resp_grplab = grp_lab
      ), by = "Response"
  ) %>%
  left_join(
    var_lookup %>%
      select(
        Predictor = var, clean_pred = clean_name, pred_lab = label,
        pred_grp = group, pred_grplab = grp_lab
      ), by = "Predictor"
  ) %>%
  mutate(
    new_predlab = fct_rev(pred_lab),
    sig_line = case_when(
      clean_resp == "Infection" & clean_pred == "Cap. Shift" ~ FALSE,
      clean_resp == "Infection" & clean_pred == "Sex" ~ FALSE,
      .default = TRUE
    ),
    prob_resp = paste0("P(", clean_resp, ")")
  )

long_peros <- long_peros %>%
  group_by(Response, Predictor) %>%
  mutate(
    glm.pval = (anova(glm(resp_val ~ pred_val, family = "binomial"))$`Pr(>Chi)`[2]),
    glm.sig = glm.pval <= 0.05
  )

# mutate()# break down the data into continuous and binary predictor variables
## continuous
long_cont <- long_peros %>% filter(!Predictor %in% c("sex_male", "sex_mature"))
## binary (summaries)
long_bin <- long_peros %>%
  filter(
    Predictor %in% c("sex_male", "sex_mature")
  )

ggplot(data = long_bin, aes(x = pred_val, y = resp_val)) +
  facet_grid(prob_resp ~ new_predlab, scales = "free_x", switch = "both") +
  # facet_grid(clean_pred ~ clean_resp, scales = "free_y", switch = "y") +
  geom_smooth(
    data = long_bin, method = "glm", method.args = list(family = "binomial"),
    aes(col = "logistic reg.", linetype = glm.sig), se = FALSE, col = "black"
  ) +
  stat_summary(
    data = long_bin, fun.data = mean_cl_normal, geom = "pointrange",
    size = 1/4, linewidth = 0.75,
    aes(col = "mean \U00B1 95% CI")
  ) +
  geom_smooth(
    data = long_cont,
    method = "glm", method.args = list(family = "binomial"),
    aes(col = "logistic reg.", linetype = glm.sig), se = FALSE, col = "black"
  ) +
  geom_point(
    data = long_cont,
    aes(col = "raw data"), size = 1
  ) +
  labs(x = NULL, y = NULL, col = NULL) +
  scale_x_continuous(breaks = c(-1, 0, 1, 10, 25, 40)) +
  theme_bw() +
  theme(
    strip.placement = "outside", strip.background = element_blank(),
    panel.spacing.y = unit(0.25, "lines"),
    panel.spacing.x = unit(0, "lines"),
    legend.position = "inside", legend.position.inside = c(1/5, 1/2),
    legend.justification = c(0.5, 0.5),
    legend.background = element_rect(color = "black"),
    strip.text.x = element_text(vjust = 1),
    strip.clip = "on",
    strip.switch.pad.grid = unit(-0.05, "lines")
  ) +
  scale_color_manual(
    breaks = c(
      "mean \U00B1 95% CI", "raw data"#, "logistic reg."
    ),
    values = c(
      "cornflowerblue", "grey50"#, "black"
    )
  ) +
  scale_linetype_manual(
    breaks = c(TRUE, FALSE), values = c("solid", "dashed")
  ) +
  guides(linetype = FALSE)

ggsave(
  filename =  "infection-modeling/graphics/parasitism-infection-fig.png",
  width = 6, height = 6*0.5, dpi = 300
)

## ---- Figure 4 (SEM effects breakdown) ----

full_brkdwn %>%
  filter(
    Response != Predictor
    # !(total == 0 & indirect == 0 & new_direct == 0)
  ) %>%
  ggplot(aes(y = clean_pred, x = total)) +
  facet_wrap(~clean_resp) +
  # geom_vline(xintercept = 0, col = "grey50", linetype = "solid") +
  geom_col(
    aes(fill = "total"),
    width = 0.9, linewidth = 0.33, col = "black"
  ) +
  geom_col(
    aes(x = indirect, fill = "indirect"),
    position = position_nudge(y = 0.15),
    width = 0.3, linewidth = 0.25, col = "black"
  ) +
  geom_col(
    aes(
      x = new_direct, fill = "direct", linetype = P.Value <= 0.1,
      # alpha = P.Value <= 0.1
    ),
    position = position_nudge(y = -0.15),
    width = 0.3, linewidth = 0.25, col = "black"
  ) +
  theme_bw() +
  theme(
    # strip.background = element_rect(fill = NA, color = "black"),
    strip.background = element_blank(),
    legend.position = "inside", legend.position.inside = c(0.95, 0.05),
    legend.justification = c(1, 0),
    legend.background = element_blank(),
    # legend.background = element_rect(color = "black", linewidth = 0.3),
    panel.spacing.y = unit(0, "lines"),
    panel.spacing.x = unit(0, "lines")
  ) +
  labs(
    x = "Standardized effect", fill = NULL, y = NULL,
  ) +
  scale_fill_manual(values = c("cornflowerblue", "orange", "grey80")) +
  scale_linetype_manual(values = c("dotdash", "solid")) +
  # scale_alpha_manual(values = c(0.25, 1)) #+
  guides(linetype = "none")

ggsave(
  "infection-modeling/graphics/cumulative-effect-fig.png",
  width = 5, height = 1*5, dpi = 300
)

## ---- Figure 5 (species SEM effects breakdown comparison) ----
pele_model_objects <- readRDS(
  "infection-modeling/data/model-objects/PELE-SEM-objects.rds"
)
pele_sem <- pele_model_objects$sem_mod$fit
pema_model_objects <- readRDS(
  "infection-modeling/data/model-objects/PEMA-SEM-objects.rds"
)
pema_sem <- pema_model_objects$sem_mod$fit

spec_coefs <- readRDS(
  "infection-modeling/data/model-objects/joint-species-coefficients_SEM.rds"
) %>%
  rename(Species = Species)

pele_fp <- build_nodes_edges(pele_sem$coefficients)
pema_fp <- build_nodes_edges(pema_sem$coefficients)

format_edge_lab <- function(x, y){
  x.small = round(x, 2)
  y.small = round(y, 2)
  paste0(x.small, " (", y.small, ")")
}

spec_unif_coefs <- spec_coefs %>%
  select(Response, Predictor, Species, CI_overlap, Std.Estimate, P.Value) %>%
  pivot_wider(names_from = Species, values_from = c(Std.Estimate, P.Value)) %>%
  mutate(
    opposite_signs = if_else(
      Std.Estimate_PEMA * Std.Estimate_PELE < 0, TRUE, FALSE
    ),
    label = format_edge_lab(Std.Estimate_PELE, Std.Estimate_PEMA),
    tooltip = label,
    style = if_else(P.Value_PELE <= 0.1, "solid", "dashed"),
    color = case_when(
      CI_overlap ~ "black",
      # CI_overlap & Std.Estimate_PELE >= 0 ~ "cornflowerblue",
      # CI_overlap & Std.Estimate_PELE < 0 ~ "orange",
      # !CI_overlap & !opposite_signs & Std.Estimate_PELE >= 0 ~ "cornflowerblue",
      # !CI_overlap & !opposite_signs & Std.Estimate_PELE < 0 ~ "orange",
      # !CI_overlap & opposite_signs ~ "purple"
      !CI_overlap ~ "purple"
    ),
    fontcolor = color,
  ) %>% ungroup()

pele_fp$edges <- spec_unif_coefs %>%
  left_join(
    var_lookup %>%
      select(
        Response = var, clean_resp = clean_name, resp_lab = label,
        resp_grp = group, resp_grplab = grp_lab,
      ), by = "Response"
  ) %>%
  left_join(
    var_lookup %>%
      select(
        Predictor = var, clean_pred = clean_name, pred_lab = label,
        pred_grp = group, pred_grplab = grp_lab
      ), by = "Predictor"
  ) %>%
  mutate(
    # x = jitter(.data$x, factor = 0.1), y = jitter(.data$y, factor = 0.1),
    From = pred_lab, To = resp_lab
  )

pele_fp$nodes <- pele_fp$nodes %>%
  left_join(var_lookup %>% select(name = var, label, x, y)) %>%
  mutate(
    fillcolor = if_else(name == "Bb_infected", "thistle1", "white"),
    name = label,
    x = jitter(x*2, factor = 0.1),
    y = jitter(y*2, factor = 0.1),
  ) %>% select(-label, -x, -y)

build_psem_plot(pele_fp, attr_theme = "lr") %>%
  # add_global_graph_attrs("layout", "fdp", "graph") %>%
  # set_node_attrs("shape", "rectangle") %>%
  set_node_attrs("width", 0.9) %>%
  set_node_attrs("height", 0.5) %>%
  set_edge_attrs("fontsize", 12) %>%
  render_graph()

# Try to match the conceptual SEM, but with phenotype expanded:
## TODO

split_lookup <- var_lookup %>%
  mutate(
    grp_lab = if_else(group == "Phenotype", label, grp_lab),
    new_group = if_else(group == "Phenotype", var, group)
  ) %>%
  select(-group) %>%
  rename(group = new_group) %>%
  relocate(group, .before = grp_lab)

grp_split_lookup <- split_lookup %>%
  select(group, grp_lab, x, y) %>%
  distinct() %>%
  mutate(
    x = x*5, y = y*5,
    x = case_when(
      group == "sex_male" ~ x + .125 ,
      group == "sex_mature" ~ x - 0,
      group == "weight" ~ x + .2,
      TRUE ~ x,
      .default = x
    ),
    y = case_when(
      group == "sex_male" ~ y - 0,
      group == "sex_mature" ~ y + .3,
      group == "weight" ~ y - .2,
      TRUE ~ y,
      .default = y
    ),
  )

# check
grp_split_lookup %>%
  ggplot(aes(x = x, y = y, label = group)) +
  geom_label()

# Join with data
split_specs <- spec_coefs %>%
  left_join(
    split_lookup %>%
      select(
        Response = var, resp_grp = group, resp_lab = label,
        resp_grplab = grp_lab
      ),
    by = "Response"
  ) %>%
  left_join(
    split_lookup %>%
      select(
        Predictor = var, pred_grp = group, pred_lab = label,
        pred_grplab = grp_lab
      ),
    by = "Predictor"
  )

# summarize the data
group_spec <- split_specs %>%
  group_by(Response, resp_grp, resp_grplab, Species, pred_grp, pred_grplab) %>%
  summarize(
    mean_coef = mean(Std.Estimate, na.rm = TRUE),
    overlap_prop = mean(CI_overlap, na.rm = TRUE), .groups = "drop"
  ) %>%
  group_by(resp_grp, resp_grplab, Species, pred_grp, pred_grplab) %>%
  summarize(
    mean_coef = mean(mean_coef, na.rm = TRUE),
    overlap_prop = mean(overlap_prop, na.rm = TRUE), .groups = "drop"
  ) %>% group_by(resp_grplab, pred_grplab) %>%
  summarize(
    mean_coef = mean(mean_coef, na.rm = TRUE),
    overlap_prop = mean(overlap_prop, na.rm = TRUE), .groups = "drop"
  )

spec_nes <- build_nodes_edges(
  coef_tab = group_spec, from_col = "pred_grplab",
  to_col = "resp_grplab", val_col = "mean_coef"
)

spec_nes$nodes <- spec_nes$nodes %>%
  full_join(grp_split_lookup %>% select(name = grp_lab, x, y), by = "name")

build_psem_plot(spec_nes, render = F) %>%
  add_global_graph_attrs("layout", "fdp", "graph") %>%
  set_node_attrs("width", 0.9) %>%
  set_node_attrs("fontcolor", "black") %>%
  set_node_attrs("color", "black") %>%
  render_graph()

## This is too tedious... need to look into ggnetwork

# pheno_preds <- grp_specs %>% filter(pred_grp == "Phenotype") %>%
#   mutate(
#     pred_grp = Predictor,
#     pred_grplab = pred_lab,
#     mean_coef = Std.Estimate,
#     overlap_prop = as.numeric(CI_overlap)
#   ) %>% ungroup()
#
# non_phenopreds <- grp_specs %>% filter(pred_grp != "Phenotype") %>%
#   group_by(Response, resp_lab, resp_grplab, Species, pred_grp, pred_lab) %>%
#   summarize(
#     mean_coef = mean(Std.Estimate, na.rm = TRUE),
#     overlap_prop = mean(CI_overlap, na.rm = TRUE)
#   ) %>% ungroup()
#
# grp_specs <- bind_rows(
#   non_phenopreds,
#   pheno_preds %>% select(c(colnames(non_phenopreds)))
# )

## ---- Table S1 (full SEM coefficients) ----

full_coef_tab <- full_mod$coefficients %>%
  select(
    Response = resp_lab, Predictor = pred_lab, Coef = Estimate,
    SE = Std.Error, DF, P = P.Value, StdCoef = Std.Estimate
  ) %>%
  left_join(
    full_brkdwn %>%
      select(
        Response = resp_lab, Predictor = pred_lab,
        StdCoef = direct, Indirect = indirect, Total = total
      ),
    by = c("Response", "Predictor", "StdCoef")
  ) %>%
  mutate(
    Response = gsub("\\n", " ", Response),
    Predictor = gsub("\\n", " ", Predictor)
  )

full_coef_tab

saveRDS(
  full_coef_tab, "infection-modeling/data/MS-tables/full-SEM-coef-tab.rds"
)

write.csv(
  full_coef_tab, "infection-modeling/data/MS-tables/full-SEM-coef-tab.csv"
)

## ---- Table S2 (full SEM fit stats)----
# TODO: combine with Table S1?

full_R2_tab <- full_mod$R2 %>%
  left_join(
    var_lookup %>%
      select(Response = var, resp_lab = label),
    by = "Response"
  ) %>%
  select(
    Response = resp_lab, Family = family, Marginal, Conditional
  ) %>%
  mutate(Response = gsub("\\n", " ", Response))

full_R2_tab

saveRDS(
  full_R2_tab, "infection-modeling/data/MS-tables/full-SEM-R2-tab.rds"
)

write.csv(
  full_R2_tab, "infection-modeling/data/MS-tables/full-SEM-R2-tab.csv"
)

## ---- Figure S1 (full SEM diagram) ----
# TODO: needed??

## ---- Table S3 (species SEM coefficients) ----

sp_cumeffs <- readRDS(
  file.path(
    "infection-modeling/data/model-objects",
    "species_cumulative-effects-table_simplified-SEM.rds"
  )
) %>%
  rename(Response = To, Predictor = From)

sp_cumeffs <- full_join(
  spec_coefs, sp_cumeffs, by = c("Response", "Predictor", "P.Value", "Species")
)

sp_cumeffs <- sp_cumeffs %>%
  left_join(
    var_lookup %>%
      select(
        Response = var, clean_resp = clean_name, resp_lab = label,
        resp_grp = group, resp_grplab = grp_lab
      ), by = "Response"
  ) %>%
  left_join(
    var_lookup %>%
      select(
        Predictor = var, clean_pred = clean_name, pred_lab = label,
        pred_grp = group, pred_grplab = grp_lab
      ), by = "Predictor"
  )

species_coef_tab <- sp_cumeffs %>%
  select(
    Species, Response = resp_lab, Predictor = pred_lab,
    Coef = Estimate, SE = Std.Error, DF, P = P.Value,
    direct, indirect, total
  ) %>%
  mutate(DF = round(DF)) %>%
  pivot_wider(
    names_from = Species, values_from = Coef:total, names_sep = "."
  ) %>%
  relocate(ends_with("PELE"), .after = "Predictor")


species_coef_tab

saveRDS(
  species_coef_tab,
  "infection-modeling/data/MS-tables/species-SEM-coef-tab.rds"
)

write.csv(
  species_coef_tab,
  "infection-modeling/data/MS-tables/species-SEM-coef-tab.csv"
)

## ---- Table S4 (species SEM fit status) ----
# TODO: combine with Table S3?

## ---- Figure S2 ----
# TODO: needed??


## ---- Experimental stuff ----

full_stats <- readRDS(
  "infection-modeling/data/model-objects/SEM-statistics-all.rds"
)

# Compare psem breakdown with semEff breakdown
full_stats <- full_brkdwn %>%
  select(Response, Predictor, new_direct:total) %>%
  pivot_longer(
    c(new_direct:total), names_to = "effect_type", values_to = "psem.eff"
  ) %>%
  mutate(
    effect_type = factor(
      effect_type, levels = c("new_direct", "indirect", "total"),
      labels = c("direct", "indirect", "total")
    )
  ) %>%
  full_join(
    full_stats, by = c("Response", "Predictor", "effect_type")
  )

# Flag significance based on various methods
full_stats <- full_stats %>%
  mutate(
    psem.sig = P.Value <= 0.1, boot.sig = boot.p <= 0.1,
    boot.sig.mean = boot.p.mean <= 0.1,
    ci.sig = !(boot.ci.low < 0 & boot.ci.up > 0),
    joint_sig = if_else(effect_type == "direct", psem.sig, ci.sig)
  )

# Places where significance do not match across methods
full_stats %>%
  filter(
    psem.sig != boot.sig | psem.sig != boot.sig.mean |
      boot.sig != boot.sig.mean | ci.sig != psem.sig |
      ci.sig != boot.sig | ci.sig != boot.sig.mean,
    effect_type != "mediators"
  ) %>%
  select(
    Response, Predictor, effect_type, psem.eff, boot.eff,
    P.Value = P.Value, boot.p, boot.p.mean, ci.sig, joint_sig
  )

# Figure for effects breakdown
plot_dat <- full_stats %>%
  filter(
    # effect_type %in% c("direct", "indirect", "total"),
    Response %in% c("Bb_infected", "expr_PC1", "expr_PC2", "ticks_attached")
  ) %>%
  group_by(Response, Predictor) %>%
  complete(effect_type) %>%
  ungroup() %>%
  mutate(
    effect_type = factor(
      effect_type, levels = c("direct", "indirect", "mediators", "total")
    ),
    Predictor = factor(
      Predictor, levels = var_lookup$var,
      labels = gsub("Cap. shift", "Cap. time \U0394", var_lookup$label)
    ),
    Response = factor(
      Response,
      levels = c("Bb_infected", "ticks_attached", "expr_PC1", "expr_PC2"),
      labels = c("Infection", "Parasitism", "Resistance", "Tolerance")
    ),
    across(c(boot.eff, boot.ci.low, boot.ci.up), ~replace_na(.x, 0)),
    across(c(P.Value, boot.p), ~replace_na(.x, 1)),
    across(c(psem.sig:joint_sig), ~replace_na(.x, FALSE))
  ) %>%
  arrange(Response, Predictor, effect_type, joint_sig) %>%
  group_by(Response, Predictor) %>%
  mutate(drop = all(boot.eff == 0 | is.na(boot.eff))) %>%
  ungroup() %>%
  filter(
    effect_type %in% c("direct", "indirect", "total"),
    !drop
  ) %>%
  mutate(effect_type = factor(
    effect_type, levels = c("indirect", "direct", "total"),
    labels = c("Indirect", "Direct", "Total")
    ),
    panel_labs = factor(
      Response, levels = levels(Response),
      label = c("A", "B", "C", "D")
    ),
    Predictor = factor(Predictor, levels = rev(levels(Predictor)))
  )

label_tab <- tibble(
  id = 1:4, lab = LETTERS[id],
  Response = c("Infection", "Parasitism", "Resistance", "Tolerance"),
  Predictor = "Infection\nstatus",
  x = -Inf, y = Inf
) %>%
  mutate(
    Resposne = factor(Response, levels = levels(plot_dat$Response)),
    Predictor = factor(Predictor, levels = levels(plot_dat$Predictor))
  )


pd = position_dodge(0.9)
type_colors = c("black", "firebrick", "cornflowerblue")
lighten_amnt = c(0.8, 0.6, 0.6)

library(lemon)
plot_dat %>%
  ggplot(
    aes(
      y = Predictor, x = boot.eff, xmin = boot.ci.low, xmax = boot.ci.up,
      col = effect_type
    )
  ) +
  facet_wrap(~Response, nrow = 1, scales = "free_x") +
  # facet_grid(Predictor ~ Response, scale = "free_y") +
  geom_text(
    data = label_tab, inherit.aes = FALSE, aes(label = lab, x = -Inf, y = Inf),
    hjust = -.5, vjust = 1.5, fontface = "bold", size = 3.5
  ) +
  geom_hline(
    yintercept = seq_len(length(unique(plot_dat$Predictor)) - 1) + 0.5,
    color = "grey80", linetype = "solid"
  ) +
  geom_vline(xintercept = 0, color = "grey80", linetype = "dashed") +
  geom_col(
    aes(fill = effect_type, group = effect_type), col = "black",
    position = pd, width = 0.9
  ) +
  geom_pointrange(
    aes(group = effect_type, shape = joint_sig, linetype = joint_sig), fill = "white",
    position = pd, size = 1/3,
    linewidth = 3/4, stroke = 1
  ) +
  scale_color_manual(
    breaks = c("Total", "Direct", "Indirect"),
    values = type_colors
  ) +
  scale_fill_manual(
    breaks = c("Total", "Direct", "Indirect"),
    values = colorspace::lighten(type_colors, lighten_amnt)
  ) +
  scale_shape_manual(values = c(21, 16)) +
  scale_linetype_manual(values = c("twodash", "solid")) +
  scale_x_continuous(
    breaks = c(-0.4, 0, 0.4),
    sec.axis = sec_axis(transform = . ~ ., name = "Response")
  ) +
  guides(shape = "none", linetype = "none") +
  labs(
    x = "Standardized effect (\U00B1 90% CI)",
    fill = NULL, color = NULL,
  ) +
  theme_bw() +
  theme(
    # Text properties
    axis.text = element_text(color = "black", size = 10),
    legend.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 12),
    legend.title = element_text(color = "black", size = 12),
    strip.text = element_text(color = "black", size = 10),

    # Legend properties
    legend.position = "inside",
    # legend.position.inside = c(1/2, 1/4),
    # legend.justification = c(0.5, 0.5),
    legend.position.inside = c(1/2, 1/2),
    legend.justification = c(1/2, 1/2),
    legend.background = element_rect(color = "black"),
    legend.margin = margin(l = -0.6, r = 2, t = -0.6, b = -0.6),
    legend.key = element_blank(), legend.key.spacing.y =  unit(-0.1, "lines"),

    # panel properties
    strip.background = element_blank(),
    # panel.spacing.x = unit(0.2, "lines"),
    # panel.grid.minor.x = element_blank()
    panel.spacing.y = unit(-0.2, "lines"),
    panel.spacing.x = unit(0.25, "lines"),
    strip.text.y = element_blank(),
    panel.grid = element_blank(),
    # axis.ticks.y = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text.x.top = element_blank(),
    panel.border = element_rect(color = "black", linewidth = 1)
  )

ggsave(
  filename =  "infection-modeling/graphics/bootstrap_SEMcoefs.png",
  width = 6*0.9, height = 6, dpi = 300
)

species_effs_tab <- readRDS(
  "infection-modeling/data/model-objects/SEM-statistics-species.rds"
)

species_effs_tab %>%
  filter(
    effect_type %in% c("direct", "total"),
    Response %in% c("Bb_infected", "ticks_attached")
  ) %>%
  group_by(Response, Predictor, effect_type) %>%
  mutate(
    overlap = first(boot.ci.low) <= last(boot.ci.up) &
      last(boot.ci.low) <= first(boot.ci.up),
  ) %>% ungroup() %>%
  mutate(sig.ci = !(boot.ci.low < 0 & boot.ci.up > 0)) %>%
  ggplot(
    aes(
      y = Predictor, x = boot.eff, xmin = boot.ci.low, xmax = boot.ci.up,
      color = Species, shape = sig.ci, linetype = sig.ci
    )
  ) +
  geom_vline(xintercept = 0, color = "grey50", linetype = "dotted") +
  facet_grid(effect_type~Response, scales = "free_x") +
  geom_pointrange(position = position_dodge(0.33), fill = "white") +
  theme_bw() +
  theme(
    panel.spacing = unit(0, "lines")
  ) +
  labs(x = "Effect", linetype = NULL, shape = NULL) +
  scale_linetype_manual(
    values = c("dotdash", "solid"), labels = c("P > 0.1", "P \U2264 0.1")
  ) +
  scale_shape_manual(
    values = c(21, 16), labels = c("P > 0.1", "P \U2264 0.1")
  ) +
  scale_x_continuous(breaks = c(-.5, 0, .5))

## ---- Rebuild network ----

var_lookup <- var_lookup %>%
  rename(group.x = x, group.y = y) %>%
  group_by(group) %>%
  mutate(n = n())

var_lookup %>%
  ggplot(aes(x = group.x*4, y = group.y*4)) +
  geom_point(size = 30, shape = 21, col = "black", fill = "white") +
  geom_label(aes(label = group)) +
  scale_x_binned(expand = c(0.1, 0)) +
  scale_y_binned(expand = c(0.1, 0)) +
  theme_blank()

t = seq(0, 2*pi, length.out = length(unique(var_lookup$group)))
x_coords = c(0, cos(t))
y_coords = c(0, sin(t))

coords_tab <- tribble(
  ~group, ~x, ~y,
  "Parasitism", 0, 0.5,
  "Infection", 1.5, 2,
  "Environment", 2, -1,
  "Phenotype", -1, -2,
  "Behavior", -2, 0,
  "Expression", -1, 2
)

coords_tab$x <- x_coords[1:nrow(coords_tab)]
coords_tab$y <- y_coords[1:nrow(coords_tab)]

coords_tab <- coords_tab %>%
  left_join(var_lookup %>% select(var, group)) %>%
  mutate(x = x, y = y)

# full_mod$coefficients %>%

all_paths <- full_stats %>%
  filter(boot.eff != 0, !is.na(boot.eff)) %>%
  group_by(Response, Predictor) %>%
  mutate(
    eff.n = n(),
    any.sig = any(joint_sig)
  )

var_coords <- all_paths %>% filter(any.sig) %>%
  select(Response, Predictor) %>% distinct() %>%
  left_join(
    coords_tab %>%
      select(
        Response = var, resp.grp = group, resp.x = x, resp.y = y
      )
  ) %>%
  left_join(
    coords_tab %>%
      select(
        Predictor = var, pred.grp = group, pred.x = x, pred.y = y
      )
  )

missing <- tibble(resp.grp = "Infection", pred.grp = "Expression") %>%
  left_join(
    coords_tab %>%
      select(
        resp.grp = group, resp.x = x, resp.y = y
      ) %>% distinct()
  ) %>%
  left_join(
    coords_tab %>%
      select(
        pred.grp = group, pred.x = x, pred.y = y
      ) %>% distinct()
  ) %>%
  mutate(across(c(pred.x, pred.y, resp.x, resp.y), ~.x*1.1))

# Plot coordinates with large paths
var_coords %>% select(contains("resp."), contains("pred.")) %>%
  distinct() %>%
  left_join(
    full_stats %>%
      filter(effect_type == "total") %>%
      select(Response, Predictor, boot.eff)
  ) %>%
  group_by(resp.grp, pred.grp, resp.x, resp.y, pred.x, pred.y) %>%
  summarize(boot.eff = mean(boot.eff, na.rm = TRUE)) %>%
  mutate(pair = paste(c(resp.grp, pred.grp), collapse = ".")) %>%
  ggplot() +
  geom_arrow_segment(
    aes(
      x = pred.x, y = pred.y, xend = resp.x, yend = resp.y,
      linewidth = abs(boot.eff), group = pair,
      col = boot.eff > 0
    ),
    resect_head = 11.5, length_head = 2, stroke_color = "black"
  ) +
  geom_arrow_segment(
    data = missing, color = "grey50", linetype = "dashed",
    linewidth = 1, resect_head = 16,
    aes(x = pred.x, y = pred.y, xend = resp.x, yend = resp.y),

  ) +
  geom_point(
    data = coords_tab %>% select(group, x, y) %>% distinct(),
    aes(x = x, y = y), size = 30,
    shape = 21, fill = "white", col = "black"
  ) +
  geom_text(
    data = coords_tab %>% select(group, x, y) %>% distinct(),
    aes(x = x, y = y, label = group),
  ) +
  scale_x_continuous(expand = c(0.15, 0)) +
  scale_y_continuous(expand = c(0.15, 0)) +
  scale_linewidth_continuous(range = c(2, 9)) +
  scale_color_manual(values = c("orange", "forestgreen")) +
  # theme_blank() +
  theme_bw() +
  theme(legend.position = "none")

## TODO: can still add coefficient labels

# ## adjust - This just doesn't work. it is way too messy.
# coords_tab <- coords_tab %>%
#   mutate(
#     slope = (y - first(y)) / (x - first(x)),
#   ) %>%
#   group_by(group) %>%
#   mutate(
#     group.n = n(),
#     group.t = (2*pi / group.n) * (row_number() - 1),
#     adj.x = if_else(group.n > 1, cos(group.t), 0),
#     adj.y = if_else(group.n > 1, sin(group.t), 0)
#   )
#
# var_coords <- var_coords %>%
#   left_join(
#     coords_tab %>%
#       select(
#         Response = var, resp.grp = group, resp.x = x, resp.y = y,
#         resp.adj.x = adj.x, resp.adj.y = adj.y
#       )
#   ) %>%
#   left_join(
#     coords_tab %>%
#       select(
#         Predictor = var, pred.grp = group, pred.x = x, pred.y = y,
#         pred.adj.x = adj.x, pred.adj.y = adj.y
#       )
#   )
#
# coords_tab %>%
#   ggplot(aes(x = x + adj.x, y = y + adj.y)) +
#   geom_point()
#
#
# var_coords %>% select(contains("resp."), contains("pred.")) %>%
#   distinct() %>%
#   ggplot() +
#   geom_point(
#     data = coords_tab %>% select(group, x, y, adj.x, adj.y) %>% distinct(),
#     aes(x = x, y = y), size = 30,
#     shape = 21, fill = "white", col = "black"
#   ) +
#   geom_arrow_segment(
#     aes(x = pred.x + pred.adj.x, y = pred.y + pred.adj.y,
#         xend = resp.x + resp.adj.x, yend = resp.y + resp.adj.y),
#     resect = 5
#   ) +
#   geom_label(
#     data = coords_tab %>% select(var, group, x, y, adj.x, adj.y) %>% distinct(),
#     aes(x = x + adj.x, y = y + adj.y, label = var),
#   ) +
#   scale_x_continuous(expand = c(0.18, 0)) +
#   scale_y_continuous(expand = c(0.18, 0))
