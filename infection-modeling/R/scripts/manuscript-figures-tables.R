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
    species = updated_taxa
  ) %>% filter(!is.na(species)) %>%
  mutate(
    across(c(siteID, plotID, year, iid, species), ~as.factor(.x)) # factor vars
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
  2, "Bb_infected", "Infection", "Infection",
    "Infection", "Infection",
  3, "expr_PC1", "Resistance", "Resistance\nexpression",
    "Expression", "Immune\nexpression",
  4, "ticks_attached", "Parasitism", "Parasitism\n(ticks)",
    "Parasitism", "Parasitism\n(ticks)",
  5,"capprop_shift", "Cap. Shift", "Capture\ntime \U0394",
    "Behavior", "Behavior\n(timing)",
  6, "cap_prop_night", "Cap. Time", "Capture\ntime",
    "Behavior", "Behavior\n(timing)",
  7, "weight", "Weight", "Weight",
    "Phenotype", "Phenotype\n(morphological)",
  8, "sex_mature", "Reproductive", "Reproductive\nmaturity",
    "Phenotype", "Phenotype\n(morphological)",
  9, "sex_male", "Sex", "Sex",
    "Phenotype", "Phenotype\n(morphological)",
  10, "wthr_PC1", "Weather", "Weather\n(PC)",
    "Environment", "Environment",
  11, "clim_PC1", "Climate", "Climate\n(PC)",
    "Environment", "Environment",
) %>%
  mutate(across(-c(rank), ~factor(.x, levels = unique(.x))))

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

group_nodes_edge$nodes %>%
  mutate(
    fill_color = if_else(grepl("Infection", name), "thistle1", "white"),
    # coords = case_when(
    #   grepl("expression", name) ~ tibble(x = -1, y = 2),
    #   grepl("Infection", name) ~ tibble(x = 2, y = 1),
    #   # name == "Parasitism\n(ticks)" ~ tibble(x = 0, y = 0.1),
    #   # name == "Phenotype" ~ tibble(x = -1, y = -2),
    #   # name == "Behavior\n(timing)" ~ tibble(x = -2, y = 0),
    #   # name == "Environment" ~ tibble(x = 2, y = -1),
    #   .default = tibble(x = NA, y = NA)
    # )
  )

## TODO: stopping point 11/10/2025 - need to finish replicating conceptSEM.

build_psem_plot(
  group_nodes_edge, render = TRUE, attr_theme = "bt"
)

# ---- Figure 3 (raw parasitism/infection effects) ----
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

saveRDS(
  full_coef_tab, "infection-modeling/data/MS-tables/full-SEM-coef-tab.rds"
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

saveRDS(
  full_R2_tab, "infection-modeling/data/MS-tables/full-SEM-R2-tab.rds"
)

## ---- Figure S1 (full SEM diagram) ----
# TODO: needed??

## ---- Table S3 (species SEM coefficients) ----

## ---- Table S4 (species SEM fit status) ----
# TODO: combine with Table S3?

## ---- Figure S2 ----
# TODO: needed??
