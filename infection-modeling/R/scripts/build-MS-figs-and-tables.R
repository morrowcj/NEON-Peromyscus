## ---- Introduction ----

# This is a script to create the high-resolution figures for the
## manuscript.

## ---- Packages and functions ----

# Load needed functions
library(ggimage)
library(ggtext)
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
  1, "expr_PC2", "Tolerance", "Tolerance\n(gene expr.)",
  "Expression", "Immunity",
  2, "Bb_infected", "Infection", "Infection\nstatus",
  "Infection", "Infection",
  3, "expr_PC1", "Resistance", "Resistance\n(gene expr.)",
  "Expression", "Immunity",
  4, "ticks_attached", "Parasitism", "Parasitism\n(ticks)",
  "Parasitism", "Parasitism\n(ticks)",
  5, "capprop_shift", "Cap. time \U0394", "Capture\ntime \U0394",
  "Behavior", "Behavior\n(timing)",
  6, "cap_prop_night", "Cap. time", "Capture\ntime",
  "Behavior", "Behavior\n(timing)",
  7, "weight", "Weight", "Weight",
  "Phenotype", "Individual\nstate",
  8, "sex_mature", "Reproductive", "Repro.\nstatus",
  "Phenotype", "Individual\nstate",
  9, "sex_male", "Sex", "Sex",
  "Phenotype", "Individual\nstate",
  10, "wthr_PC1", "Weather", "Weather\n(warm, wet)",
  "Environment", "Environment",
  11, "clim_PC1", "Climate", "Climate\n(warm, wet)",
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

# Create a lookup table to the icon images.
icon_lookup <- tribble(
  ~name, ~path, ~group,
  "clock", "clock.png", "Behavior",
  "cell", "immune-cell.png", "Expression",
  "mouse", "mouse-silhouette.png", "Phenotype",
  "bacteria", "spirochete.png", "Infection",
  "thermometer", "thermometer.png", "Environment",
  "tick", "tick.png", "Parasitism"
) %>%
  mutate(path = file.path("infection-modeling/graphics/icons", path)) %>%
  left_join(coords_tab, by = "group")

# Create a color lookup table for the colors that Sarah provided
color_lookup <- tribble(
  ~hex, ~from,
  "3c354a", "night_sky",
  "47599d", "blue_sky",
  "e36e46", "sunset",
  "d04e46", "immune_nucleus",
  "d96552", "immune_cell",
  "f8ec32", "borellia",
  "a9bb95", "immunity",
  "a7c37a", "grass",
  "535947", "night_ground",
  "7a6452", "mouse_fur"
) %>%
  mutate(
    hex = paste0("#", hex),
    gradient_rank = row_number(),
    discrete_rank = case_when(
      from == "blue_sky" ~ 1,
      from == "sunset" ~ 2,
      from == "grass" ~ 3,
      from == "mouse_fur" ~ 4,
      from == "night_sky" ~ 5,
      .default = NA
    )
  )

# add in icons to the axis labels
full_stats <- full_stats %>%
  # add icon to y-label
  left_join(
    icon_lookup %>% select(pred_icon_path = path, pred.group = group),
    by = "pred.group"
  ) %>%
  left_join(
    icon_lookup %>% select(resp_icon_path = path, resp.group = group),
    by = "resp.group"
  ) %>%
  mutate(
    # resplab_md = gsub("\\n", "<br>", resp.label),
    # predlab_md = gsub("\\n", "<br>", pred.label),
    resp_icon_label = glue::glue(
      "<img src='{resp_icon_path}' height=12 />",
      " {resp.label}"
    ) %>% fct_reorder(as.numeric(resp.label)),
    pred_icon_label = glue::glue(
      "<img src='{pred_icon_path}' height=12 style='transform: translate(0px,-120px);' />",
      " {pred.label}"
    ) %>% fct_reorder(as.numeric(pred.label)),
  )


## ---- Fig 1: Extreme scenarios (conceptual) ----

fig_path = file.path(
  "infection-modeling/graphics/icons",
  "lyme risk figure-draft-2026-01-20.png"
)
magick::image_read(fig_path) # large

## ---- Fig 2A: SEM network diagram (conceptual) ----

## TODO: make this a multi-panel figure, where the second panel
## is a host-range + NEON site map.

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

## nudge x and y of the icons
icon_lookup <- icon_lookup %>%
  mutate(
    nudge_x = case_when(
      name == "mouse" ~ 0.08,
      name == "clock" ~ 0.22,
      name == "cell" ~ 0.25,
      name == "thermometer" ~ 0.25,
      name == "bacteria" ~ 0.02,
      name == "tick" ~ 0.18,
      .default = 0
    ),
    nudge_y = case_when(
      name == "mouse" ~ 0.22,
      name == "clock" ~ -0.12,
      name == "cell" ~ 0,
      name == "thermometer" ~ -0.1,
      name == "bacteria" ~ 0.26,
      name == "tick" ~ -0.15,
      .default = 0
    )
  )

# build the conceptual diagram
(SEM_dgrm <- (
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
      color = with(color_lookup, hex[from == "mouse_fur"]),
      resect_head = 12, length_head = NULL,
      linewidth = 1, linetype = "longdash", arrow_head = triangle,
      # stroke_color = NULL
    ) +
    geom_point(
      size = 25, shape = 21,
      # fill = "white",
      fill = with(color_lookup, hex[from == "immunity"]) %>%
        colorspace::lighten(0.25),
      color = "black"
    ) +
    ## Add the icons
    geom_image(
      data = icon_lookup, inherit.aes = FALSE, size = 0.1,
      aes(image = path, x = x + nudge_x, y = y + nudge_y),
    ) +
    geom_text(size = 3) +
    # annotate(
    #   geom = "text", label = "A", fontface = "bold", size = 5,
    #   x = -Inf, y = Inf, vjust = 1, hjust = -1
    # ) +
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
    scale_color_discrete(
      palette = color_lookup %>% filter(from %in% c("sunset", "blue_sky")) %>%
        pull(hex) %>%
        rev() %>% colorspace::lighten(0.25)
    )
))

ggsave(
  plot = SEM_dgrm,
  filename = "infection-modeling/graphics/conceptual-SEM-full.png",
  width = 4.5, height = 4.5, dpi = 300
)

## ---- Fig 2B, map panel (host range map) ----

pero_map <- readRDS("infection-modeling/data/peromyscus-range-neon.rds")

# show the map
pero_map

library(ggpubr)


fig_2 <- ggarrange(
  SEM_dgrm, pero_map, nrow = 1,
  labels = c("A", "B")
)

ggsave(
  plot = fig_2,
  filename = "infection-modeling/graphics/full-figure-2.png",
  width = 8, height = 8/2
)

## ---- Fig 3: Logistic regressions ----

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

ggplot(
  data = long_bin,
  aes(x = pred_val, y = resp_val)
) +
  facet_grid(
    prob_resp ~ new_predlab, scales = "free_x", switch = "both",
    labeller = as_labeller(function(s){gsub("\\n", " ",s)})
  ) +
  geom_smooth(
    data = long_bin, method = "glm", method.args = list(family = "binomial"),
    formula = "y ~ x",
    aes(col = "logistic reg.", linetype = glm.sig), se = TRUE,
    # col = "black",
    col = with(color_lookup, hex[from == "night_sky"]) %>%
      colorspace::lighten(0.25),
    fill = with(color_lookup, hex[from == "grass"]) %>%
      colorspace::lighten(0.25)
  ) +
  stat_summary(
    data = long_bin, fun.data = mean_cl_normal, geom = "errorbar", width = 0.05,
    size = 1/4, linewidth = 0.7,
    aes(col = "mean\n(\U00B1 95% CI)")
  ) +
  stat_summary(
    data = long_bin, fun.data = mean_cl_normal, geom = "point",
    size = 1.6,
    aes(col = "mean\n(\U00B1 95% CI)")
  ) +
  geom_smooth(
    data = long_cont,
    method = "glm", method.args = list(family = "binomial"),
    formula = "y ~ x",
    aes(col = "logistic reg.", linetype = glm.sig), se = TRUE,
    # col = "black",
    col = with(color_lookup, hex[from == "night_sky"]) %>%
      colorspace::lighten(0.25),
    fill = with(color_lookup, hex[from == "grass"]) %>%
      colorspace::lighten(0.25)
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
    legend.position.inside = c(0.10, 0.62),
    legend.justification = c(0.5, 0.5),
    legend.key = element_blank(),
    legend.key.width = unit(1, "lines"),
    legend.background = element_rect(color = "black", linewidth = 1/3),
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
    # values = c("cornflowerblue", "grey50")
    values = with(
      color_lookup, hex[from %in% c("blue_sky", "sunset")]
    ) %>% rev() %>%
      colorspace::lighten(0.25)
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

## ---- Fig 4: SEM effects breakdown ----

# table for plotting just the total effects
tot_stats <- full_stats %>%
  complete(Response, Predictor, resp.label, pred.label, pred_icon_path) %>%
  filter(
    effect_type %in% c("total"),
    Response %in% c("Bb_infected", "ticks_attached", "expr_PC1", "expr_PC2"),
  ) %>%
  filter(
    !(
      Response == "ticks_attached" &
        Predictor %in% c("Bb_infected", "expr_PC1", "ticks_attached")
    ),
    !(Response == "expr_PC1" & Predictor %in% c("Bb_infected", "expr_PC1")),
    !(Response == "Bb_infected" & Predictor %in% c("Bb_infected"))
  )

# table for plotting just the indirect effects
ind_stats <- inner_join(
  x = full_stats %>%
    complete(
      Response, Predictor, resp.label, pred.label, effect_type,
      pred_icon_label, resp_icon_label
    ) %>%
    filter(effect_type == "indirect"),
  y = tot_stats %>%
    select(
      Predictor, Response, resp.label, pred.label, pred_icon_label,
      resp_icon_label
    ),
  by = c(
    "Predictor", "Response", "resp.label", "pred.label",
    "pred_icon_label", "resp_icon_label")
) %>%
  mutate(
    across(c(boot.eff, boot.eff.mean), ~replace_na(.x, 0)),
    joint_sig = replace_na(joint_sig, FALSE),
    ci.sig = replace_na(ci.sig, FALSE)
  )

# plot parameters
nudge_factor = 0.125
point_stroke = 1
point_size = 1
eb_end_width = 0.22
eb_stroke = 0.7
# bg_color = with(color_lookup, hex[from == "immunity"]) %>%
#   colorspace::lighten(0.25)
bg_color = "white"

# Build the plot
tot_stats %>% ggplot(
  aes(
    # y = pred.label,
    y = pred_icon_label %>% forcats::fct_rev(),
    x = boot.eff, xmin = boot.ci.low, xmax = boot.ci.up,
    col = effect_type,
    # shape = ci.sig, linetype = ci.sig
    shape = joint_sig, linetype = joint_sig
  )
) +
  facet_wrap(
    ~forcats::fct_reorder(resp_icon_label, as.numeric(Response)) %>%
      forcats::fct_rev(),
    nrow = 1, scales = "free_x",
    labeller = as_labeller(function(s){gsub("\\n", "<br>",s)})
  ) +
  geom_vline(
    xintercept = 0, linetype = "longdash",
    color = with(color_lookup, hex[from == "mouse_fur"])
  ) +
  # add the indirect effects
  geom_errorbar(
    data = ind_stats, width = eb_end_width, linewidth = eb_stroke,
    position = position_nudge(y = -nudge_factor)
  ) +
  geom_point(
    data = ind_stats,
    fill = bg_color,
    stroke = point_stroke,
    size = point_size,
    position = position_nudge(y = -nudge_factor)
  ) +
  # add total effects
  geom_errorbar(
    width = eb_end_width, linewidth = eb_stroke,
    position = position_nudge(y = nudge_factor)
  ) +
  geom_point(
    position = position_nudge(y = nudge_factor),
    fill = bg_color, stroke = point_stroke, size = point_size,
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = -10, b = 0),
    legend.spacing.x = unit(2, "lines"),
    legend.key.width = unit(1.5, "lines"),
    legend.key.spacing = unit(0.25, "lines"),
    strip.background = element_blank(),
    text = element_text(color = "black"),
    axis.text.y = ggtext::element_markdown(
      hjust = 1, vjust = 0.3, color="black"
    ),
    panel.background = element_rect(fill = bg_color),
    legend.key = element_rect(fill = NA),
    strip.text = ggtext::element_markdown(hjust = 1, color = "black"),
    axis.title.y = element_text(margin = margin(r = -0.5, unit = "lines")),
    panel.spacing.x = unit(0.2, "lines")
  ) +
  labs(
    x = "Standardized effect (± 90% CI)",
    y = "Predictor",
    # color = "Effect type",
    linetype = NULL, shape = NULL, color = NULL
  ) +
  scale_color_manual(
    values = with(color_lookup, hex[from %in% c("blue_sky", "sunset")]) %>%
      colorspace::lighten(0.25) %>% rev(),
    breaks = c("total", "indirect"),
    labels = c("total eff.", "indirect eff.")
  ) +
  scale_linetype_manual(
    values = c("solid", "longdash"), breaks = c(TRUE, FALSE),
    labels = c("P ≤ 0.1", "n.s.")
  ) +
  scale_shape_manual(
    values = c(19, 21), breaks = c("TRUE", "FALSE"),
    labels = c("P ≤ 0.1", "n.s.")
  ) +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4)) +
  guides(
    color = guide_legend(order = 1)
  )

ggsave(
  filename =  "infection-modeling/graphics/SEM-effect_coef-plot.png",
  width = 6, height = 6*0.5, dpi = 300
)

# # Alternative: Total effects (full model vs species)
# full_stats %>%
#   mutate(Species = "both") %>%
#   filter(
#     Response %in% c("Bb_infected", "ticks_attached", "expr_PC1", "expr_PC2"),
#     effect_type %in% c("total")
#   ) %>%
#   bind_rows(
#     species_stats %>%
#       filter(
#         effect_type == "total",
#         Response %in% c("Bb_infected", "ticks_attached", "expr_PC1", "expr_PC2")
#       )
#   ) %>%
#   # complete(Species, Response, Predictor, effect_type) %>%
#   mutate(
#     Species = factor(Species, levels = c("PELE", "both", "PEMA"))
#   ) %>%
#   ggplot(
#     aes(
#       y = Predictor, x = boot.eff, xmin = boot.ci.low, xmax = boot.ci.up,
#       color = Species
#     )
#   ) +
#   facet_wrap(~Response, nrow = 1, scales = "free_x") +
#   geom_vline(xintercept = 0, color = "grey50", linetype = "longdash") +
#   geom_pointrange(
#     aes(
#       shape = interaction(Species, joint_sig), linetype = joint_sig
#     ), fill = "white", stroke = point_stroke, linewidth = eb_stroke,
#     size = 0.7, position = position_dodge(0.6)
#   ) +
#   theme_bw() +
#   scale_shape_manual(
#     breaks = c(
#       "PELE.TRUE", "PELE.FALSE",
#       "both.TRUE", "both.FALSE",
#       "PEMA.TRUE", "PEMA.FALSE"
#     ),
#     values = c(
#       17, 24,
#       16, 21,
#       15, 22
#     )
#   ) +
#   scale_color_manual(
#     breaks = c("PELE", "both", "PEMA"),
#     labels = c("PELE (spec.)", "both (full)", "PEMA (spec.)"),
#     values = c("orange", "black", "cornflowerblue")
#   ) +
#   scale_linetype_manual(
#     breaks = c(TRUE, FALSE), values = c("solid", "longdash")
#   ) +
#   # scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5)) +
#   scale_x_continuous(breaks = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6)) +
#   labs(
#     x = "Total standardized effect (± 90% CI)",
#     shape = "Group", linetype = "Group", color = "Species (model)"
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

## ---- Fig 5: Species effect comparison ----

# add colors to the variable lookup for plotting this figure
spvar_lookup <- var_lookup %>%
  mutate(
    used = group %in% c("Expression", "Behavior"),
    point_col = c("black", "grey40")[used + 1],
    point_fill = c(
      colorspace::lighten(with(color_lookup, hex[from %in% c("grass")]), 0.25),
      "grey80"
    )[used + 1]
  )

# Summarize the species data for path arrows
spec_conc_dat <- species_stats %>%
  filter(effect_type == "total") %>%
  select(
    Species, Response, Predictor,
    boot.eff, boot.ci.low, boot.ci.up,
  ) %>%
  pivot_wider(
    names_from = Species,
    id_cols = c(Response, Predictor),
    values_from = c(boot.eff, boot.ci.low, boot.ci.up)
  ) %>%
  mutate(
    PELE_PEMA_diff = boot.eff_PELE - boot.eff_PEMA,
    overlap = boot.ci.low_PELE <= boot.ci.up_PEMA &
      boot.ci.low_PEMA <= boot.ci.up_PELE
  ) %>% select(
    Response, Predictor, PELE_PEMA_diff, overlap
  ) %>% right_join(
    species_stats, by = c("Response", "Predictor")
  ) %>%
  group_by(Species, pred.group, resp.group) %>%
  mutate(
    any_sig = any(joint_sig), # any variables in the group are sig for spec.
  ) %>%
  ungroup() %>%
  mutate(
    PELE_sig = any_sig & Species == "PELE",
    PEMA_sig = any_sig & Species == "PEMA"
  ) %>%
  group_by(
    pred.group, pred.grp_lab, resp.group, resp.grp_lab,
    pick(ends_with(".x")), pick(ends_with(".y"))
  ) %>%
  summarise(
    mean_diff = mean(PELE_PEMA_diff, na.rm=TRUE),
    sig_group = case_when(
      any(PELE_sig) & any(PEMA_sig) ~ "both",
      any(PELE_sig) ~ "PELE",
      any(PEMA_sig) ~ "PEMA",
      .default = "neither"
    ),
    all_overlap = all(overlap),
    .groups = "drop"
  ) %>%
  filter(complete.cases(.)) %>%
  mutate(
    sig_path = !(all_overlap & sig_group == "both"),
    path_col_group = case_when(
      !sig_path ~ "n.s.",
      mean_diff <= 0 ~ "negDiff",
      mean_diff > 0 ~ "posDiff"
    )
  )

spvar_lookup %>%
  ggplot(aes(x = grp.x, y = grp.y, label = grp_lab)) +
  geom_arrow_segment(
    data = spec_conc_dat, inherit.aes = FALSE,
    aes(
      x = pred.grp.x, y = pred.grp.y, xend = resp.grp.x, yend = resp.grp.y,
      linewidth = abs(mean_diff),
      col = path_col_group,
    ),
    resect_head = 10, length_head = 2, stroke_color = "black",
    arrow_head = triangle) +
  geom_point(
    col = spvar_lookup$point_col,
    fill = spvar_lookup$point_fill,
    shape = 21, size = 25
  ) +
  geom_image(
    data = icon_lookup, inherit.aes = FALSE, size = 0.1,
    aes(image = path, x = x + nudge_x, y = y + nudge_y),
  ) +
  geom_text(size = 3, color = spvar_lookup$point_col) +
  ggnetwork::theme_blank() +
  theme(
    axis.line = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(1, .165),
    legend.justification = c(1, 0),
    legend.background = element_rect(
      fill = NA, color = "black", linewidth = 1/3
    ),
    legend.margin = margin(l = 2, b = 1, r = 2)
  ) +
  scale_x_continuous(expand = c(0.11, 0)) +
  scale_y_continuous(expand = c(0.11, 0)) +
  scale_linewidth_continuous(range = c(1, 5)) +
  scale_color_manual(
    breaks = c("n.s.", "negDiff", "posDiff"),
    labels = c("PELE = PEMA", "PEMA > PELE", "PELE > PEMA"),
    values = c(
      "grey80",
      colorspace::lighten(
        with(color_lookup, hex[from %in% c("blue_sky", "sunset")])
      )
    )
  ) +
  guides(
    colour = guide_legend(title = "Effect comparison"),
    linewidth = "none"
  )

ggsave(
  filename = "infection-modeling/graphics/species-SEM-comparison.png",
  width = 4.5, height = 4.5, dpi = 300
)

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
trait_EMMs <- lapply(
  traits,
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

## --- Compare models without traits ----

# get infection model dataframe
inf_dat <- model.frame(modlist$infection) %>%
  group_by(siteID, year, species) %>%
  mutate(
    group_sex = mean(sex_male == 1, na.rm = TRUE),
    group_mat = mean(sex_mature == 1, na.rm = TRUE),
    group_weight = mean(weight, na.rm = TRUE),
    group_shift = mean(capprop_shift, na.rm = TRUE),
    group_ticks = mean(ticks_attached == 1, na.rm = TRUE)
  )

# copy the model with this new dataframe
full_inf <- modlist$infection %>%
  update(data = inf_dat)

# refit without traits
simple_inf <- modlist$infection %>%
  update(
    . ~ . - sex_male - sex_mature - weight - capprop_shift,
    data = inf_dat
  )

# refit with population average traits
group_inf <- simple_inf %>%
  update(
    . ~ . + group_sex + group_mat + group_weight + group_shift,
    data = inf_dat
  )

# calculate R-squred for tick model
inf_r2 <- piecewiseSEM:::rsquared(list(full_inf, simple_inf, group_inf)) %>%
  mutate(mod = c("full", "simple", "group")) %>%
  relocate(mod, .before = 0)

# show r-squared table
inf_r2 %>%
  select(Response, mod, Marginal, Conditional) %>%
  mutate(across(-c(mod, Response), ~round(.x, 3)))

# Compare the infection models
anova(full_inf, simple_inf, group_inf) # full wins P = 0.0016

# extract tick model data
tick_dat <- model.frame(modlist$ticks) %>%
  group_by(siteID, year, species) %>%
  mutate(
    group_sex = mean(sex_male == 1, na.rm = TRUE),
    group_mat = mean(sex_mature == 1, na.rm = TRUE),
    group_weight = mean(weight, na.rm = TRUE),
    group_shift = mean(capprop_shift, na.rm = TRUE),
    group_ticks = mean(ticks_attached == 1, na.rm = TRUE)
  )

# refit original model
full_tick <- modlist$ticks %>%
  update(data = tick_dat)

# refit without traits
simple_tick <- modlist$ticks %>%
  update(
    . ~ . - sex_male - sex_mature - weight - capprop_shift,
    data = tick_dat
  )

# refit with population average traits
group_tick <- simple_tick %>%
  update(
    . ~ . + group_sex + group_mat + group_weight + group_shift,
    data = tick_dat
  )

# compare tick models
anova(full_tick, simple_tick, group_tick) # full wins P = 1.343e-5

# calculate R-squared for tick models:
tick_r2 <- piecewiseSEM:::rsquared(list(full_tick, simple_tick, group_tick)) %>%
  mutate(mod = c("full", "simple", "group")) %>%
  relocate(mod, .before = 0)

# show r-squared
tick_r2 %>%
  select(Response, mod, Marginal, Conditional) %>%
  mutate(across(-c(mod, Response), ~round(.x, 3)))

# compare the models

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

## ---- Update Fig data (PELE only) ----

# Load PELE data
Peros <- readRDS("infection-modeling/data/peromyscus-model-data.rds") %>%
  mutate(species = updated_taxa) %>%
  filter(!is.na(species)) %>%
  mutate(
    across(c(siteID, plotID, year, iid, species), ~as.factor(.x)) # factor vars
  ) %>% 
  filter(species == "PELE")

# Sem objects
sem_objects <- readRDS(
  "infection-modeling/data/model-objects/pele_2026_full-sem-objects.rds"
)

modlist = sem_objects$component_mods
modlist = lapply(modlist, function(fm){update(fm, data = Peros)})
semfit = sem_objects$sem_mod$fit

# Boostrap statistics - with some cleaning
full_stats <- readRDS(
  "infection-modeling/data/model-objects/pele-SEM-2026_boot-statistics-all.rds"
) %>% 
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
  ) %>% select(-any_of(c("boot.upr.diff", "boot.lwr.diff"))) %>% 
  mutate(
    psem.sig = P.Value <= 0.1, # raw model stats
    boot.sig = boot.p <= 0.1, # pseudo-p-value, using orig. coef. and boot SE
    boot.sig.mean = boot.p.mean <= 0.1, # pseudo-P using boot coef. and SE
    ci.sig = !(boot.ci.low < 0 & boot.ci.up > 0), # based on 90% CI
    joint_sig = if_else(effect_type == "direct", psem.sig, ci.sig) # combined
  ) %>%
  group_by(Response, Predictor) %>%
  mutate(group.sig = any(joint_sig)) %>%
  ungroup() %>% 
  left_join(resp_lookup, by = "Response") %>%
  left_join(pred_lookup, by = "Predictor") %>% 
  # add icon to y-label
  left_join(
    icon_lookup %>% select(pred_icon_path = path, pred.group = group),
    by = "pred.group"
  ) %>%
  left_join(
    icon_lookup %>% select(resp_icon_path = path, resp.group = group),
    by = "resp.group"
  ) %>%
  mutate(
    # resplab_md = gsub("\\n", "<br>", resp.label),
    # predlab_md = gsub("\\n", "<br>", pred.label),
    resp_icon_label = glue::glue(
      "<img src='{resp_icon_path}' height=12 />",
      " {resp.label}"
    ) %>% fct_reorder(as.numeric(resp.label)),
    pred_icon_label = glue::glue(
      "<img src='{pred_icon_path}' height=12 style='transform: translate(0px,-120px);' />",
      " {pred.label}"
    ) %>% fct_reorder(as.numeric(pred.label)),
  )


## ---- Updated Fig 3 ----

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
  ) %>% 
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

ggplot(
  data = long_bin,
  aes(x = pred_val, y = resp_val)
) +
  facet_grid(
    prob_resp ~ new_predlab, scales = "free_x", switch = "both",
    labeller = as_labeller(function(s){gsub("\\n", " ",s)})
  ) +
  geom_smooth(
    data = long_bin, method = "glm", method.args = list(family = "binomial"),
    formula = "y ~ x",
    aes(col = "logistic reg.", linetype = glm.sig), se = TRUE,
    # col = "black",
    col = with(color_lookup, hex[from == "night_sky"]) %>%
      colorspace::lighten(0.25),
    fill = with(color_lookup, hex[from == "grass"]) %>%
      colorspace::lighten(0.25)
  ) +
  stat_summary(
    data = long_bin, fun.data = mean_cl_normal, geom = "errorbar", width = 0.05,
    size = 1/4, linewidth = 0.7,
    aes(col = "mean\n(\U00B1 95% CI)")
  ) +
  stat_summary(
    data = long_bin, fun.data = mean_cl_normal, geom = "point",
    size = 1.6,
    aes(col = "mean\n(\U00B1 95% CI)")
  ) +
  geom_smooth(
    data = long_cont,
    method = "glm", method.args = list(family = "binomial"),
    formula = "y ~ x",
    aes(col = "logistic reg.", linetype = glm.sig), se = TRUE,
    # col = "black",
    col = with(color_lookup, hex[from == "night_sky"]) %>%
      colorspace::lighten(0.25),
    fill = with(color_lookup, hex[from == "grass"]) %>%
      colorspace::lighten(0.25)
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
    legend.position.inside = c(0.10, 0.62),
    legend.justification = c(0.5, 0.5),
    legend.key = element_blank(),
    legend.key.width = unit(1, "lines"),
    legend.background = element_rect(color = "black", linewidth = 1/3),
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
    # values = c("cornflowerblue", "grey50")
    values = with(
      color_lookup, hex[from %in% c("blue_sky", "sunset")]
    ) %>% rev() %>%
      colorspace::lighten(0.25)
  ) +
  scale_linetype_manual(
    breaks = c(TRUE, FALSE), values = c("solid", "dashed")
  ) +
  guides(linetype = "none")

# save the file
ggsave(
  filename =  "infection-modeling/graphics/PELE_parasitism-infection-fig.png",
  width = 6, height = 6*0.5, dpi = 300
)

## ---- Updated Fig 4 ----


stat_plot_dat <- full_stats %>% 
  filter(
    Response %in% c("Bb_infected", "ticks_attached", "expr_PC1", "expr_PC2"),
    !is.na(resp_icon_label), effect_type != "mediators"
  ) %>% 
  complete(
    effect_type, 
    nesting(
      Response, Predictor, resp.label, pred.label, pred_icon_label, 
      resp_icon_label
    )
  ) %>% 
  filter(!is.na(resp_icon_label)) %>% 
  replace_na(list(boot.eff = 0, joint_sig = FALSE)) %>% 
  mutate(effect_type = fct_relevel(effect_type, "direct", after = 1)) %>% 
  group_by(Response, Predictor) %>% 
  mutate(biggest = max(abs(boot.eff), na.rm = TRUE)) %>% ungroup()

dodge_width = 2/3

stat_plot_dat %>% 
  ggplot(
    aes(
      y = pred_icon_label %>% forcats::fct_rev(),
      x = boot.eff,
      xmin = boot.ci.low, xmax = boot.ci.up, col = effect_type,
      shape = joint_sig, linetype = joint_sig
    )
  ) + 
  facet_wrap(
    ~forcats::fct_reorder(resp_icon_label, as.numeric(Response)) %>%
      forcats::fct_rev(),
    nrow = 1, scales = "free_x",
    labeller = as_labeller(function(s){gsub("\\n", "<br>",s)})
  ) +
  geom_vline(
    xintercept = 0, linetype = "solid",
    linewidth = 0.2,
    color = with(color_lookup, hex[from == "mouse_fur"])
  ) +
  geom_col(
    aes(fill = effect_type),
    width = dodge_width, # alpha = 0.2,
    col = NA,
    show.legend = FALSE,
    position = position_dodge(dodge_width)
  ) +
  geom_errorbar(
    width = eb_end_width, linewidth = eb_stroke,
    position = position_dodge(dodge_width)
  ) + 
  geom_point(
    fill = bg_color,
    stroke = point_stroke,
    size = point_size*0.9,
    position = position_dodge(dodge_width)
  ) + 
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = -10, b = 0),
    legend.spacing.x = unit(2, "lines"),
    legend.key.width = unit(1.5, "lines"),
    legend.key.spacing = unit(0.25, "lines"),
    strip.background = element_blank(),
    text = element_text(color = "black"),
    axis.text.y = ggtext::element_markdown(
      hjust = 1, vjust = 0.3, color="black"
    ),
    panel.background = element_rect(fill = bg_color),
    legend.key = element_rect(fill = NA),
    strip.text = ggtext::element_markdown(hjust = 1, color = "black"),
    axis.title.y = element_text(margin = margin(r = -0.5, unit = "lines")),
    panel.spacing.x = unit(0.05, "lines")
  ) +
  labs(
    x = "Standardized effect (± 90% CI)",
    y = "Predictor",
    linetype = NULL, shape = NULL, color = NULL
  ) +
  scale_color_manual(
    values = with(
      color_lookup, hex[match(c("blue_sky", "sunset", "grass"), from)]
    ) %>%
      colorspace::lighten(0),
    breaks = c("total", "direct", "indirect"),
    labels = c("total eff.", "direct eff.", "indirect eff.")
  ) +
  scale_fill_manual(
    values = with(
      color_lookup, hex[match(c("blue_sky", "sunset", "grass"), from)]
    ) %>%
      colorspace::lighten(0.5),
    breaks = c("total", "direct", "indirect"),
    labels = c("total eff.", "direct eff.", "indirect eff.")
  ) +
  scale_linetype_manual(
    values = c("solid", "longdash"), breaks = c(TRUE, FALSE),
    labels = c("P ≤ 0.1", "n.s.")
  ) +
  scale_shape_manual(
    values = c(19, 21), breaks = c("TRUE", "FALSE"),
    labels = c("P ≤ 0.1", "n.s.")
  ) +
  scale_x_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4)) +
  guides(
    color = guide_legend(order = 1)
  )

ggsave(
  filename =  "infection-modeling/graphics/PELE_SEM-effect_coef-plot.png",
  width = 6.5, height = 6.5*0.65, dpi = 300
)

# save raw table
stat_plot_dat %>% 
  select(
    Response, Predictor, effect_type, 
    boot.eff, boot.eff.mean, boot.bias,
    boot.SE, boot.ci.low, boot.ci.up,
    boot.z, boot.p, psem.sig, ci.sig, joint_sig
  ) %>% 
  write.csv(
    file = "infection-modeling/data/pele-sem-boot-results_uncleaned.csv", 
    row.names = FALSE
  )

## ---- Updated Tab S1 ----

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

# save as a csv
write.csv(
  tab_S1, "infection-modeling/data/MS-tables/PELE_full-SEM-coef-tab.csv",
  row.names = FALSE,
)

## ---- Updated Tab S2 ----
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

# index of columns with scientific notation to preserve ast strings
str_col_inx <- grep(".*\\.str", names(tab_S2))

tab_S2 %>% select(all_of(str_col_inx)) %>% head()

str = "4.3e-04"

# save as a csv
tab_S2 %>%
  mutate(
    across(
      contains("Bias.str"), ~ifelse(is.na(.x), NA, convert_scinote(.x))
    )
  ) %>%
  write.csv(
    file = "infection-modeling/data/MS-tables/PELE_full-SEM-boot-tab.csv",
    row.names = FALSE, quote = str_col_inx, fileEncoding = "UTF-8"
  )


## ---- Updated Tab S4 ----
modlist[["captime"]] = modlist[["captime"]] %>% update(. ~ . + (1|year))


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
  file = "infection-modeling/data/MS-tables/PELE_SEM-random-effects-table.csv",
  row.names = FALSE
)
