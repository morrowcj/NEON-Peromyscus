library(tidyverse)
library(lubridate)

weather_prods <- readRDS("data-curation/data/eight_sites/weather-summaries.rds")

weather_prods$variables_00001 |> tibble()

# Mouse reproduction by bout

mammal_prods <- readRDS("R:/morrow5_data_repo/NEON-products/mammal-trap-data.rds")

bout_table <- mammal_prods$mam_perplotnight |> 
  tibble() |> 
  mutate(
    year = year(collectDate), date = date(collectDate), boutID = eventID
  ) |> 
  separate_wider_delim(
    eventID, delim = ".", names = c("IDsite", "IDyear", "IDnum")
  ) |> 
  mutate(IDyear = as.numeric(IDyear), IDnum = as.numeric(IDnum)) |>
  arrange(year, siteID, IDnum) |> 
  mutate(boutID = factor(boutID, levels = unique(boutID))) |> 
  select(nightuid, siteID, plotID, boutID, boutNum = IDnum) |> 
  distinct()

mammals <- mammal_prods$mam_pertrapnight |> tibble() |> 
  mutate(year = year(collectDate), date = date(collectDate))

n_mamms <- nrow(mammals)

mammals <- mammals |> 
  full_join(bout_table, by = c("nightuid", "siteID", "plotID"))

rm(mammal_prods, bout_table)
gc()

# filter to only include Peromyscus or non-captures
mammals <- mammals |> 
  filter(
    grepl("Peromyscus", scientificName) | 
      # is.na(scientificName) | # no scientific name
      !grepl("4|5", trapStatus) # no animal trapped
  )

# reproductive vars
mammals$vagina |> unique()
vag_lvls <- c("neither", "not plugged", "swollen", "plugged", "both")
vag_mature <- c("neither", "swollen", "plugged")

mammals$nipples |> unique()
nips_lvls <- c("nonenlarged", "enlarged")
nips_mature <- "enlarged"

mammals$testes |> unique()
test_lvls <- c("nonscrotal", "scrotal")
test_mature <- "scrotal"

# update sexual maturity variables
mammals <- mammals |> 
  mutate(
    vagina = factor(vagina, levels = vag_lvls, ordered = TRUE),
    testes = factor(testes, levels = test_lvls),
    nipples = factor(nipples, levels = nips_lvls),
    sex_mature = vagina %in% vag_mature |
      nipples %in% nips_mature |
      testes %in% test_mature
  )

bout_maturity_stats <- mammals |> 
  filter(
    !is.na(sex_mature), sex != "U",
    taxonID %in% c("PELE", "PEMA")
  ) |>
  group_by(year, siteID, plotID, boutID, scientificName, taxonID, sex) |>
  arrange(date) |>
  summarize(
    first_dt = first(date), last_dt = last(date),
    med_dt = median(c(first_dt, last_dt)),
    mean_mature = mean(sex_mature, na.rm = TRUE),
    sd_mature = sd(sex_mature, na.rm = TRUE),
    n = sum(!is.na(sex_mature)),
    se_mature = sd_mature / sqrt(n),
    .groups = "drop"
  ) |> 
  mutate(
    month = month.abb[month(first_dt)],
    month = factor(month, levels = month.abb)
  ) 
  
summary(bout_maturity_stats)

mammals |> 
  filter(taxonID %in% c("PELE", "PEMA")) |> 
  group_by(taxonID, lifeStage, sex_mature) |> 
  tally() |> 
  pivot_wider(names_from = sex_mature, values_from = n)

bout_maturity_stats |> 
  filter(year > 2016, month %in% month.abb[3:10]) |> 
  ggplot(
    aes(x = year, y = mean_mature, col = taxonID)
  ) + 
  facet_grid(sex~month, scales = "free_y") + 
  geom_point(size = 0.33) +
  geom_smooth(
    method = "lm", formula = "y ~ x", fill = "grey80", linewidth = 0.5
  ) + 
  theme_bw() + 
  theme(
    legend.position = "top", panel.spacing = unit(0, "lines"),
    legend.margin = margin(0, 0, 0, 0), plot.margin = margin(l = 2, r = 2)
  ) +
  labs(col = NULL, y = "Reproductive proportion", x = "Years after 2000") +
  scale_x_continuous(
    breaks = seq(2000, 2030, by = 3), 
    labels = gsub("\\d{2}(\\d{2})", "\\1", seq(2000, 2030, by = 3))
  ) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.3), limits = c(0, NA)) +
  scale_color_manual(values = c("orange", "cornflowerblue"))
  # geom_smooth(method = "glm", method.args = list(family = "binomial")) +

ggsave(
  "infection-modeling/graphics/peromyscus-sexual-maturity-through-time.jpg",
  width = 8, height = 0.33*8
)  
  
  # ggplot(aes(x = year, y = sex_mature, col = siteID)) + 
  # geom_smooth()
  # geom_point()
  # stat_summary(fun = mean_se, aes(group = boutID))
