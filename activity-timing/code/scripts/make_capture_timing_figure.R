# ---- load package ----
library(tidyverse)
library(lemon)

# ---- Script parameters ----

# location of capture data object
capture_path <- "data/ibuttons/capture-time-trapping-data_2022-2024.rds"

# location to save figures
fig_save_dir = file.path("graphics/capture-time-exploration")

# ---- Load data ----

# load the capture data
capture_data <- readRDS(capture_path)

# unnest the neon columns
full_captures <- capture_data |> unnest(neon_trap_data)

# ---- Make Figures ----

if (!dir.exists(fig_save_dir)) {
  dir.create(fig_save_dir, recursive = TRUE)
}

theme_set(
  theme_bw() +
    theme(
      text = element_text(size = 12, colour = "black"),
      axis.title = element_text(size = 12, color = "black"),
      legend.text = element_text(size = 10, color = "black"),
      axis.text = element_text(size = 10, color = "black"),
      strip.text = element_text(size = 10, color = "black"),
    )
)

# calculate the capture time density overlap
tmp <- full_captures |> filter(!is.na(cap_prop_night)) 
overlap_list <- split(tmp$cap_prop_night, tmp$year)
ovrlp <- overlapping::overlap(overlap_list)

# Capture time densities
full_captures |> 
  filter(!is.na(cap_prop_night)) |> 
  ggplot(aes(x = cap_prop_night, fill = factor(year), color = factor(year))) +
  annotate(
    geom = "text", x = 0.6, y = 2, label = paste("overlap:", round(ovrlp$OV, 2))
  ) +
  geom_density(alpha = 0.33, linewidth = 1) +
  labs(
    x = "Relative capture time", y = "Density", col = NULL, fill = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "inside", legend.position.inside = c(1, 1),
    legend.justification = c(1, 1), legend.background = element_blank()
  )
ggsave(
  filename = file.path(fig_save_dir, "capture-overlap_all-years.jpg"), 
  width = 5, height = 0.8*5, dpi = 150
)

# Relative capture time by site within year
full_captures |> 
  filter(!is.na(cap_prop_night)) |> 
  ggplot(aes(x = siteID, y = cap_prop_night)) + 
  facet_wrap(~year, ncol = 1) + 
  stat_summary(fun = mean, geom = "line", aes(group = 1)) + 
  geom_violin(
    fill = "grey90", col = "grey50", # draw_quantiles = c(0.25, 0.5, 0.75)
  ) + 
  stat_summary(
    fun.data = mean_se, geom = "pointrange", col = "grey20", size = 0.1
  ) +
  theme(panel.spacing.y = unit(-0.1, "lines")) +
  labs(x = "Site", y = "Relative capture time")
ggsave(
  filename = file.path(fig_save_dir, "capture-violins_site-by-year.jpg"), 
  width =6, height = 0.9*6, dpi = 150
)

# site shifts through time
full_captures |> 
  filter(!is.na(cap_prop_night)) |> 
  ggplot(aes(x = cap_prop_night, fill = factor(year), col = factor(year))) +
  facet_rep_wrap(~siteID, strip.position = "top", scales = "free_y") +
  geom_density(alpha = 0.33) +
  labs(y = NULL, x = "Relative capture time", col = NULL, fill = NULL) +
  theme(
    legend.position = "inside", legend.position.inside = c(5/6, 1/6),
    legend.justification = c(.5, .5),
    legend.margin = margin(t = -2, b = -2),
    panel.spacing = unit(-.2, 'lines'), # panel.grid = element_blank(),
    strip.placement = "inside", 
    axis.text = element_blank(), axis.ticks = element_blank(),
  )
ggsave(
  filename = file.path(fig_save_dir, "capture-overlap_site-by-year.jpg"), 
  width = 8, height = 0.66*8, dpi = 150
)

# get most common species
common_captures <- full_captures |> 
  group_by(year, scientificName, taxonID) |> 
  tally() |> 
  arrange(desc(n)) |> 
  filter(!is.na(taxonID), !is.na(scientificName), n >= 20) |> 
  group_by(scientificName) |> 
  mutate(all_n = sum(n)) |> 
  ungroup() |> 
  mutate(
    scientificName = fct_reorder(scientificName, desc(all_n)), 
    taxonID = fct_reorder(taxonID, desc(all_n)), 
  )

common_summary_captures <- common_captures |> 
  select(scientificName, taxonID, n = all_n) |> 
  distinct() |> arrange(desc(n))

common_species <- common_summary_captures$scientificName
common_taxa <- common_summary_captures$taxonID

# plot the capture counts
common_captures |> 
  filter(!is.na(n)) |> 
  ggplot(aes(y = scientificName, x = n)) + 
  geom_col(
    aes(fill = , fill = factor(year)), width = .95, col = "grey30",
    linewidth = 0.25,
  ) +
  geom_text(aes(x = all_n, label = all_n), size = 4, nudge_x = 180) +
  labs(y = NULL, x = "count", fill = NULL) + 
  theme_classic() +
  theme(
    legend.position = "inside", legend.background = element_blank(),
    legend.position.inside = c(1, 1), legend.justification = c(1, 1),
    plot.margin = margin(r = .75, unit = "lines")
  )
ggsave(
  filename = file.path(fig_save_dir, "captured-species-counts.jpg"), 
  width = 6, height = 0.66*6, dpi = 150
)

# calculate the capture time density overlap
tmp <- full_captures |>   
  filter(
    scientificName %in% common_captures$scientificName, 
    !is.na(cap_prop_night),
    year == 2024
  )
sp_overlap_list <- split(tmp$cap_prop_night, tmp$taxonID)
sp_ovrlp <- overlapping::overlap(sp_overlap_list)

# species capture time distribution across sites
full_captures |> 
  filter(
    scientificName %in% common_species, 
    !is.na(cap_prop_night),
    year == 2024
  ) |> 
  mutate(
    scientificName = factor(scientificName, common_species),
    taxonID = factor(taxonID, common_taxa),
  ) |> 
  ggplot(aes(x = cap_prop_night, fill = taxonID, col = taxonID)) +
  geom_density(alpha = .33, linewidth = 1) +
  annotate(
    geom = "text", x = 0.5, y = 2,
    label = paste("overlap:", round(sp_ovrlp$OV, 2))
  ) +
  labs(fill = NULL, col = NULL, y = "Density", x = "Relative capture time") +
  theme_classic() +
  theme(
    legend.position = "inside", legend.position.inside = c(1, 1),
    legend.justification = c(1, 1), legend.background = element_blank()
  )
ggsave(
  filename = file.path(fig_save_dir, "captured-species_capture-overlap.jpg"), 
  width = 6, height = 0.8*6, dpi = 150
)


# ---- Recaptures ----

recap_dat <- full_captures |> 
  filter(taxonID %in% common_taxa, !is.na(cap_prop_night), 
         # recapture != "U"
  ) |> 
  group_by(year, taxonID, recapture) |>
  tally() |> 
  pivot_wider(names_from = "recapture", values_from = "n") |> 
  filter(
    # !is.na(N), !is.na(Y), 
    N >= 4, Y >= 4
  ) |> 
  left_join(full_captures) |> 
  mutate(is_recapture = recapture == "Y") |> 
  filter(!is.na(cap_prop_night))

# calculate the capture time density overlap
recap_overlap_list <- split(recap_dat$cap_prop_night, recap_dat$recapture)
recap_ovrlp <- overlapping::overlap(recap_overlap_list)
# recap_perm <- overlapping::perm.test(recap_overlap_list)

# timing differences by recapture
recap_dat |> 
  ggplot(aes(x = cap_prop_night, fill = recapture, col = recapture)) +
  geom_density(alpha = 0.33) +
  annotate(
    geom = "text", x = 0.5, y = 2, 
    label = paste("overlap:", round(recap_ovrlp$OV, 2))
  ) +
  labs(
    x = "Relative capture time", y = "Density", 
  ) +
  theme(
    legend.position = "inside", legend.position.inside = c(1, 1), 
    legend.justification = c(1, 1), legend.background = element_blank()
  )
ggsave(
  filename = file.path(fig_save_dir, "recapture-overlap.jpg"), 
  width = 6, height = 0.8*6, dpi = 150
)

grouped_recaps <- recap_dat |> 
  filter(recapture != "U", Y > 2, N > 2) |> 
  group_by(recapture, taxonID) |> 
  select(cap_prop_night) |> 
  mutate(group_name = paste(taxonID, recapture, sep = ".")) |> 
  nest(captimes = cap_prop_night)

sp_recap_overlap_list <- list()
for (tax in unique(grouped_recaps$taxonID)) {
  this <- grouped_recaps |> filter(taxonID == tax)
  sprecap_list <- lapply(this$captimes, function(df)df$cap_prop_night)
  names(sprecap_list) <- this$group_name
  sprecap_ovrlp <- overlapping::overlap(sprecap_list)
  perm_test <- overlapping::perm.test(sprecap_list)
  sp_recap_overlap_list[[tax]] <- list(
    overlap = sprecap_ovrlp, permtest = perm_test
  )
}

sp_recap_tab <- tibble(
  taxonID = factor(unique(grouped_recaps$taxonID), common_taxa),
  overlap = lapply(sp_recap_overlap_list, function(L){L$overlap$OV}) |> 
    unlist(),
  pval = lapply(sp_recap_overlap_list, function(L){L$permtest$pval}) |> 
    unlist(),
  sig = pval < 0.05
)

# timing overlap by species
recap_dat |> 
  mutate(taxonID = factor(taxonID, common_taxa)) |> 
  filter(recapture != "U") |> 
  ggplot(aes(x = cap_prop_night, fill = recapture, col = recapture)) +
  facet_rep_wrap(~taxonID, scales = "free_y") +
  geom_density(alpha = 0.33) +
  geom_text(
    data = sp_recap_tab, inherit.aes = FALSE, size = 3,
    aes(x = Inf, y = Inf, label = paste("overlap:", round(overlap, 2))),
    hjust = 1.2, vjust = 1.2, fontface = ifelse(sp_recap_tab$sig, "bold", "plain")
  ) +
  labs(
    x = "Relative capture time", y = "Density", 
  ) +
  theme(legend.position = "inside", legend.position.inside = c(5/6, 1/6),
        legend.justification = c(.5, .5),
        axis.text.y = element_blank(),
        panel.spacing.y = unit(-0.5, "lines")
        )
ggsave(
  filename = file.path(fig_save_dir, "species-recapture-overlap.jpg"), 
  width = 6, height = 0.66*6, dpi = 150
)

# distributions by sex
sex_group <- full_captures |> ungroup() |> 
  filter(sex != "U", !is.na(cap_prop_night)) |> 
  group_by(sex) |> 
  select(sex, cap_prop_night) |> 
  nest(cap_times = cap_prop_night)
sex_list <- lapply(sex_group$cap_times, function(x){x$cap_prop_night})
sex_overlap <- overlapping::overlap(sex_list)
# sex_perm <- overlapping::perm.test(sex_list)
  
full_captures |> 
  filter(!is.na(cap_prop_night), !is.na(sex), sex != "U") |> 
  ggplot(aes(x = cap_prop_night, col = sex, fill = sex)) +
  geom_density(alpha = 0.33, linewidth = 1) +
  annotate(
    geom = "text", x = 0.5, y = 2, 
    label = paste("overlap:", round(sex_overlap$OV, 2))
  ) +
  labs(x = "Relative capture time", y = "Density") +
  theme(
    legend.position = "inside", legend.position.inside = c(1, 1),
    legend.justification = c(1, 1), legend.background = element_blank()
  )
ggsave(
  filename = file.path(fig_save_dir, "sex-overlap.jpg"), 
  width = 5, height = 0.8*5, dpi = 150
)

# relationship between weight and capture time
full_captures |> 
  filter(
    taxonID %in% c("PELE", "PELEPEMA", "PEMA"),
    !is.na(cap_prop_night), !is.na(weight)
  ) |> 
  ggplot(aes(x = weight, y = cap_prop_night)) +
  facet_wrap(~taxonID, scales = "free_x") +
  geom_point(col = "grey50", size = 1) +
  geom_smooth(method = "lm", formula = "y ~ x", col = "black") +
  labs(y = "Relative capture time", x = "Weight") +
  theme(
    legend.position = "none", panel.spacing = unit(0.2, "lines")
  )
ggsave(
  filename = file.path(fig_save_dir, "captime-by-weight.jpg"), 
  width = 6, height = 0.4*6, dpi = 150
)

# weight and capture time, grouped by recap
full_captures |> 
  filter(
    !is.na(cap_prop_night), !is.na(weight),
    recapture != "U", 
    taxonID %in% c("PELE", "PEMA", "PELEPEMA")
  ) |> 
  group_by(year, recapture) |> 
  ggplot(aes(x = weight, y = cap_prop_night, col = recapture)) + 
  geom_point(size = 0.5) + 
  geom_smooth(method = "lm", aes(group = recapture), fill = "grey80") +
  labs(x = "Peromyscus weight", y = "Relative capture time") + 
  theme_bw() +
  theme(legend.position = "inside", legend.position.inside = c(1, 1),
        legend.justification = c(1, 1), legend.background = element_blank())
ggsave(
  filename = file.path(fig_save_dir, "Peromyscus-captime-by-weight-and-recap.jpg"), 
  width = 6, height = 0.9*6, dpi = 150
)

full_captures |> 
  filter(!is.na(tickNumber)) |> 
  mutate(tickNumber = factor(tickNumber, c("1-5", "6-20", ">20"), ordered = T)) |> 
  ggplot(aes(x = tickNumber, y = cap_prop_night)) +
  geom_boxplot() +
  labs(y = "Relative capture time", x = "Tick number") +
  theme_classic()
ggsave(
  filename = file.path(fig_save_dir, "captime-by-attached-ticks.jpg"), 
  width = 6, height = 1*6, dpi = 150
)

# capture time by longitude
full_captures |> 
  ggplot(aes(x = decimalLongitude, y = cap_prop_night)) +
  geom_point() + 
  geom_smooth(method = "lm", aes(group = 1)) +
  labs(x = "Longitude", y = "Relative capture time") +
  theme_classic()
ggsave(
  filename = file.path(fig_save_dir, "captime-by-longitude.jpg"), 
  width = 6, height = 1*6, dpi = 150
)

# capture time by latitude
full_captures |> 
  ggplot(aes(x = decimalLatitude, y = cap_prop_night)) +
  geom_point() + 
  geom_smooth(method = "lm", aes(group = 1)) +
  labs(x = "Latitude", y = "Relative capture time") +
  theme_classic()
ggsave(
  filename = file.path(fig_save_dir, "captime-by-latitude.jpg"), 
  width = 6, height = 1*6, dpi = 150
)


shift_caps <- full_captures |> 
  filter(!is.na(capture_time)) |> 
  mutate(individual = as.numeric(factor(tagID))) |> 
  group_by(siteID, individual) |> 
  arrange(individual, capture_time) |> 
  mutate(
    this_cap_num = row_number(), 
    captured_ntimes = length(capture_time),
    is_new_trap = trapCoordinate == first(trapCoordinate),
    cap_prop_shift = cap_prop_night - first(cap_prop_night) # pos = later
  ) |> 
  relocate(
    siteID, trapCoordinate, individual, capture_time, recapture, this_cap_num, 
    cap_prop_night, captured_ntimes, cap_prop_shift, is_new_trap,
    .before = 0
  ) |> 
  filter(captured_ntimes > 1)

bout_shifts <- 
  full_captures |> 
  filter(!is.na(capture_time)) |> 
  mutate(individual = as.numeric(factor(tagID))) |> 
  ungroup() |> 
  group_by(year, siteID, individual, plot_bout) |> 
  arrange(year, individual, capture_time) |> 
  mutate(
    bout_capture_num = row_number(), 
    bout_capture_ntimes = n(),
    bout_is_new_trap = trapCoordinate == first(trapCoordinate),
    bout_cap_prop_shift = cap_prop_night - first(cap_prop_night) # pos = later
  )
  # ungroup() |> 

bout_shifts |> 
  filter(bout_capture_ntimes > 1) |> 
  group_by(bout_capture_num) |> 
  summarize(mean = mean(bout_cap_prop_shift),
            sd = sd(bout_cap_prop_shift))


# how does capture time change with subsequent captures?
shift_caps |> 
  filter(this_cap_num > 1) |> 
  ggplot(aes(x = this_cap_num, y = cap_prop_shift)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_point( 
    size = 0.66,
    aes(x = jitter(this_cap_num, amount = 0.25)), col = "grey50"
  ) +
  geom_smooth(method = "lm", fill = "grey80") + 
  geom_boxplot(
    aes(group = this_cap_num), fill = NA, linewidth = 0.5, outliers = FALSE,
  ) + 
  # stat_summary(fun.data = mean_cl_normal) + 
  theme_bw() + 
  theme(panel.grid.minor.x = element_blank()) + 
  scale_x_continuous(breaks = seq(2, 20, by = 1)) + 
  scale_y_continuous(breaks = seq(-1, 1, by = 0.5)) + 
  labs(x = "Capture number (across all bouts)", 
       y = "Relative capture time shift")
ggsave(
  filename = file.path(fig_save_dir, "recapture_time_shift.jpg"), 
  width = 6, height = 1*6, dpi = 150
)

# does variation increase with subsequent captures?
shift_caps |> 
  group_by(this_cap_num) |> 
  summarize(
    cap_prop_SE = sd(cap_prop_night) / sqrt(n()),
    cap_shift_SE = sd(cap_prop_shift) / sqrt(n()),
    cap_prop_CV = sd(cap_prop_night) / mean(cap_prop_night),
    cap_shift_CV = sd(cap_prop_shift) / mean(cap_prop_shift)
  ) |> 
  filter(this_cap_num <= 16) |> 
  ggplot(aes(x = this_cap_num)) + 
  # geom_col() + 
  geom_point(
    aes(y = cap_prop_CV), col = "forestgreen"
  ) + 
  geom_smooth(
    aes(y = cap_prop_CV), formula = "y ~ x",
    method = "lm", fill = colorspace::lighten("forestgreen", amount = .9), 
    col = "forestgreen", linewidth = 0.5
  ) + 
  geom_point(
    aes(y = (cap_prop_SE*10) + 0.5), col = "cornflowerblue"
  ) + 
  geom_smooth(
    aes(y = (cap_prop_SE*10) + 0.5), formula = "y ~ x",
    method = "lm", fill = colorspace::lighten("cornflowerblue", amount = .75), 
    col = "cornflowerblue", linewidth = 0.5
  ) + 
  scale_y_continuous(
    sec.axis = sec_axis(
      transform = ~(. - 0.5)/10, name = "Capture time variation (SE)"
    )
  ) + 
  scale_x_continuous(breaks = seq(1, 16, by = 2)) + 
  theme_bw() + 
  labs(
    x = "Capture number (across all bouts)", 
    y = "Capture time variation (CV)"
  ) + 
  theme(
    axis.title.y.left = element_text(color = "forestgreen", face = "plain"),
    axis.text.y.left = element_text(color = "forestgreen", face = "plain"),
    axis.title.y.right = element_text(color = "cornflowerblue", face = "plain"),
    axis.text.y.right = element_text(color = "cornflowerblue", face = "plain")
  )
ggsave(
  filename = file.path(fig_save_dir, "variation_in_captime_by_recapture.jpg"), 
  width = 4, height = 0.9*4, dpi = 150
)

bout_shifts |> 
  filter(bout_capture_ntimes > 1, bout_capture_num > 1) |> 
  ggplot(aes(x = bout_capture_num, y = bout_cap_prop_shift)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_point( 
    size = 0.66,
    aes(x = jitter(bout_capture_num, amount = 0.25)), col = "grey50"
  ) +
  geom_smooth(method = "lm", fill = "grey80") + 
  geom_boxplot(
    aes(group = bout_capture_num), fill = NA, linewidth = 0.5, outliers = FALSE,
  ) + 
  # stat_summary(fun.data = mean_cl_normal) + 
  theme_bw() + 
  theme(panel.grid.minor.x = element_blank()) + 
  scale_x_continuous(breaks = seq(2, 20, by = 1)) + 
  scale_y_continuous(breaks = seq(-1, 1, by = 0.5)) + 
  labs(x = "Capture number (across all bouts)", 
       y = "Relative capture time shift")

# build compact letter display
fm <- bout_shifts |> 
  filter(bout_capture_ntimes > 1) |> 
  mutate(
    capture_number = factor(bout_capture_num, levels = 1:6, ordered = FALSE)
  ) %>%
  lm(bout_cap_prop_shift ~ capture_number, data = .)
em <- emmeans::emmeans(fm, "capture_number")
CLD <- emmeans:::cld.emmGrid(em, Letters = letters)
CLD

# plot the relative shift
CLD |> 
  # mutate(capture_number = as.numeric(capture_number)) |> 
  ggplot(
    aes(x = capture_number, y = emmean, ymin = lower.CL, ymax = upper.CL)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") + 
  # geom_point(
  #   data = bout_shifts |> 
  #     mutate(capture_number = jitter(bout_capture_num, amount = 0.25)), 
  #   inherit.aes = FALSE, aes(x = capture_number, y = bout_cap_prop_shift),
  #   col = "grey50", size = 0.5
  # ) +
  geom_pointrange(aes(color = .group), show.legend = FALSE) + 
  geom_text(
    aes(label = stringr::str_trim(.group), color = .group), 
    nudge_x = 0.1, nudge_y = -.01, show.legend = FALSE
  ) + 
  labs(
    x = "Capture number (within a bout)", y = "Relative capture time shift (± 95% CI)"
  ) +
  scale_color_manual(values = c("grey50", "black"))
ggsave(
  filename = file.path(fig_save_dir, "within-bout_recapture_time_shift.jpg"), 
  width = 4, height = 1*4, dpi = 150
)  
  
fm2 <- bout_shifts |> 
  filter(bout_capture_ntimes > 1) |> 
  mutate(
    capture_number = factor(bout_capture_num, levels = 1:6, ordered = FALSE)
  ) %>%
  lm(cap_prop_night ~ capture_number, data = .)
em2 <- emmeans::emmeans(fm2, "capture_number")
CLD2 <- emmeans:::cld.emmGrid(em2, Letters = letters)
CLD2

# plot the relative shift
CLD2 |> 
  ggplot(
    aes(x = capture_number, y = emmean, ymin = lower.CL, ymax = upper.CL)
  ) +
  geom_pointrange(aes(color = .group), show.legend = FALSE) + 
  geom_text(
    aes(label = stringr::str_trim(.group), color = .group), 
    nudge_x = 0.1, nudge_y = -.01, show.legend = FALSE
  ) + 
  labs(
    x = "Capture number (within a bout)", y = "Relative capture time (± 95% CI)"
  ) + 
  scale_color_manual(values = c("forestgreen", "orange", "cornflowerblue"))
ggsave(
  filename = file.path(fig_save_dir, "within-bout_recapture_time_comparison.jpg"), 
  width = 4, height = 1*4, dpi = 150
) 

