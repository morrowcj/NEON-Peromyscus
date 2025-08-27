library(tidyverse)
library(lubridate)

mammals <- readRDS("infection-modeling/data/mammal-data.rds")

## ---- Species assignment ----

# Table to help with consistent species assignment
missID <- mammals %>% 
  group_by(iid) %>% 
  summarize(
    cap_n = n(),
    ID_n = sum(length(unique(taxonID)), na.rm = TRUE),
    pele_prop = sum(taxonID == "PELE", na.rm = TRUE)/cap_n,
    pema_prop = sum(taxonID == "PEMA", na.rm = TRUE)/cap_n,
    pelepema_prop = sum(taxonID == "PELEPEMA", na.rm = TRUE)/cap_n,
    pesp_prop = sum(taxonID == "PESP", na.rm = TRUE)/cap_n,
    pero_prop = round(pele_prop + pema_prop + pelepema_prop + pesp_prop, 10),
    pele_pp = pele_prop/pero_prop,
    pema_pp = pema_prop/pero_prop,
    pelepema_pp = pelepema_prop/pero_prop,
    pesp_pp = pesp_prop/pero_prop,
    first_taxon = first(taxonID),
    alt_taxonID = case_when(
      pero_prop < 0.5 ~ "not_pero",
      pele_pp > 0.5 ~ "PELE",
      pema_pp > 0.5 ~ "PEMA",
      pelepema_pp > 0.5 ~ "PELEPEMA",
      pesp_pp > 0.5 ~ "PESP",
      .default = first_taxon
      # .default = NA
    )
  ) %>% 
  filter(ID_n > 1) %>% 
  arrange(desc(pero_prop))

# individuals without clarifying
missID %>% filter(is.na(alt_taxonID)) %>% nrow()
# individuals labeled as Peromyscus sp.
missID %>% filter(alt_taxonID == "PESP") %>% nrow()

# table with updated taxonIDs for peromyscus
pero_taxonIDs <- left_join(
  mammals %>% select(uid, iid, old_taxonID = taxonID) %>% 
    filter(!is.na(old_taxonID)),
  missID %>% select(iid, new_taxonID = alt_taxonID),
  by = "iid"
) %>% 
  arrange(iid) %>% 
  mutate(new_taxonID = if_else(is.na(new_taxonID), old_taxonID, new_taxonID)) %>% 
  distinct()

## TODO This is an ideal place for discriminant analysis to go.

# save this table
saveRDS(
  pero_taxonIDs, "infection-modeling/data/peromyscus-taxonID-updates.rds"
)

# add the updated IDs to the mammal table
mammals <- left_join(
  mammals,
  pero_taxonIDs %>% select(-old_taxonID),
  by = c("uid", "iid")
) %>% 
  mutate(
    new_taxonID = if_else(is.na(new_taxonID), taxonID, new_taxonID)
  )

## ---- Subset the Peromyscus from the mammals ----

# include only peromyscus spp.
Peros <- mammals %>% 
  filter(
    new_taxonID %in% c("PELE", "PEMA", "PELEPEMA", "PESP"),
    !is.na(tagID)
  )

## ---- Sex reassignment ----

# build a table of the updated sex designations
sex_tab <- Peros %>% 
  filter(!is.na(iid)) %>% 
  group_by(iid) %>% 
  summarize(
    cap_n = n(),
    sexing_n = sum(!is.na(sex) & sex != "U", na.rm = TRUE),
    sex_male = sum(sex == "M", na.rm = TRUE)/sexing_n,
    ever_preg = sum(pregnancyStatus == "pregnant", na.rm = TRUE) > 0,
    swollen_female_n = sum(
      vagina %in% c("swollen", "both", "plugged") | nipples == "enlarged", 
      na.rm = TRUE
    ), 
    swollen_male_n = sum(testes == "scrotal", na.rm = TRUE),
    testes_n = sum(!is.na(testes) & testes != "unknown"),
    vagina_n = sum(!is.na(vagina) & vagina != "unknown")
  ) %>% 
  ungroup() %>% 
  full_join(
    Peros %>% 
      filter(lifeStage == "adult", !is.na(iid)) %>% 
      group_by(iid) %>% 
      summarize(
        adult_obs = n(),
        adult_sex_male = sum(sex == "M", na.rm = TRUE)/adult_obs,
        adult_swollen_female = sum(
          vagina %in% c("swollen", "both", "plugged") | nipples == "enlarged",
          na.rm = TRUE
        ),
        adult_swollen_male = sum(testes == "scrotal", na.rm = TRUE)
      ) %>% 
      ungroup(),
    by = "iid"
  ) %>% 
  mutate(
    sex_male = case_when(
      ever_preg ~ 0, # has been pregnant
      adult_sex_male < 0.5 ~ 0, # adult obs are female on average
      adult_sex_male > 0.5 ~ 1, # adult obs are male on average
      sex_male < 0.5 ~ 0, # female on average (all obs)
      sex_male > 0.5 ~ 1, # male on average (all obs)
      adult_swollen_female > adult_swollen_male ~ 0, # obvious adult female genitals
      swollen_female_n > swollen_male_n ~ 0, # obv. female genitals (all obs)
      vagina_n > testes_n ~ 0, # more vaginal observations
      vagina_n < testes_n ~ 1, # more testes observations
      adult_swollen_female > 0 ~ 0, # female parts
      swollen_female_n > 0 ~ 0,
      adult_swollen_male > 0 ~ 1,
      swollen_male_n > 0 ~ 1,
      # adult_swollen_male > 0 & adult_swollen_female <= 0 ~ 1, # adult scrotal male
      # adult_swollen_female > 0 & adult_swollen_male <= 0 ~ 0, # adult repro female
      # swollen_male_n > 0 & swollen_female_n <= 0 ~ 1, # scrotal male
      # swollen_female_n > 0 & swollen_male_n <= 0 ~ 0, # reproductive female
      .default = 0.5 # unresolved sex...
    )
  ) %>% 
  ungroup()

# show the sex ratios
sex_tab %>% group_by(sex_male) %>% tally() %>% mutate(prop = n/sum(n))  

# join the new sex info into the Peromyscus table
Peros <- Peros %>% select(-sex_male) %>% 
  left_join(
    sex_tab %>% select(iid, sex_male, ever_pregnant = ever_preg),
    by = "iid"
  )

# show site-specific sex ratios
Peros %>% 
  group_by(siteID, sex_male) %>% 
  tally() %>% 
  mutate(
    prop = n / sum(n),
    sex_male = factor(
      sex_male, levels = c(0, 0.5, 1), labels = c("F", "UNK", "M")
    )
  ) %>% 
  select(-n) %>% 
  pivot_wider(names_from = sex_male, values_from = prop) %>% 
  knitr::kable(digits = 2)

# show the overlap of individuals flagged as pregnant and scrotal...
Peros %>% 
  mutate(scrote = testes == "scrotal") %>% 
  group_by(ever_pregnant, scrote) %>% 
  summarize(n = length(unique(iid))) %>% 
  ungroup() %>% 
  filter(complete.cases(.)) 

# show ambiguous individuals with some sex characteristics measured.
Peros %>% 
  filter(
    sex_male == 0.5,
    !is.na(testes) | !is.na(vagina) | !is.na(nipples) | !is.na(pregnancyStatus)
  ) %>% 
  select(iid, sex_male, testes, nipples, vagina, pregnancyStatus)

## ---- Check the iids ----

# ## Probe "tagIDs" with multiple "recapture=N"
# Peros %>%
#   group_by(tagID) %>%
#   mutate(tagObs = n(), NrecapObs = sum(recapture=="N")) %>%
#   group_by(siteID, tagID) %>%
#   mutate(siteTagObs = n(), siteNrecapObs = sum(recapture == "N")) %>%
#   select(siteID, tagID, tagObs:siteNrecapObs) %>%
#   arrange(desc(NrecapObs), desc(tagObs)) %>%
#   ungroup() %>%
#   summarize(
#     total_obs = n(),
#     unique_tags = length(unique(tagID)),
#     unique_siteTags = length(unique(paste0(siteID, tagID))),
#     reobserved_tags = sum(tagObs > 1, na.rm = TRUE),
#     tags_withNrecap = sum(NrecapObs > 1, na.rm = TRUE),
#     reobserved_sitetags = sum(siteTagObs > 1, na.rm = TRUE),
#     sitetags_withNrecap = sum(siteNrecapObs > 1, na.rm = TRUE)
#   ) %>% t() %>% knitr::kable()

# # build a table with the putative (Peromyscus) individual IDs
# tmp <- Peros %>% 
#   group_by(siteID, tagID) %>% 
#   arrange(siteID, date) %>% 
#   mutate(tag_ind_num = cumsum(recapture == "N")) %>% 
#   ungroup() %>% 
#   mutate(
#     # new_tag = gsub("NEON\\.MAM\\..*\\.(.*)", "\\1", tagID),
#     pero_ind_id = paste(siteID, tagID, tag_ind_num, sep = "."),
#     pero_iid = as.numeric(factor(pero_ind_id))
#   )
# 
# # 
# tmp2 <- tmp %>% 
#   filter(ind_id != pero_ind_id) %>% 
#   group_by(ind_id, pero_ind_id) %>% 
#   tally()
# 
# # are these ID columns synonymous?
# (tmp2$ind_id %>% unique() %>% length()) ==
#   (tmp2$pero_ind_id %>% unique() %>% length())
# 
# # merge these iids into the mammal table
# Peros <- full_join(Peros, iid_tab, by = "uid")

## ---- Check species ----

Peros %>%
  group_by(iid) %>%
  summarize(
    taxon_desig_n = length(unique(new_taxonID))
  ) %>%
  filter(taxon_desig_n > 1) %>%
  arrange(desc(taxon_desig_n)) %>% 
  nrow() %>% 
  all.equal(0)

## ---- Check sex ----

# any individuals with inconsistent sex
Peros %>% 
  group_by(iid) %>% 
  summarize(
    sex_desig_n = length(unique(sex_male)),
    taxon_desig_n = length(unique(new_taxonID))
  ) %>% 
  filter(sex_desig_n > 1) %>% 
  nrow() %>% 
  all.equal(0) %>% isTRUE()

## ---- Maturity ----

# Probe the sexual maturity of mice by sex
Peros %>%
  group_by(sex_male) %>%
  mutate(total = n()) %>%
  group_by(sex_male, sex_mature, testes, nipples, vagina, total) %>%
  tally(name = "n") %>%
  mutate(
    total_prop = round(n / nrow(Peros), 2),
    sex_prop = round(n / total, 2),
    nipples = factor(
      nipples, levels = c("nonenlarged", "unknown", "enlarged")
    ),
    vagina = factor(
      vagina,
      levels = c(
        "neither", "not plugged", "unknown", "swollen", "plugged", "both"
      )
    )
  ) %>%
  filter(
    !(sex_male == 0 & (is.na(vagina) | is.na(nipples) |
                      nipples == "unknown" | vagina == "unknown")),
    !(sex_male == 1 & is.na(testes)),
    !(sex_male == 1 & (!is.na(nipples) | !is.na(vagina) | testes == "unknown")),
    # !sex == "U",
    !(is.na(testes) & (is.na(nipples) & is.na(vagina))),
  ) %>%
  arrange(sex_male, sex_mature, !is.na(testes), testes, nipples, vagina, sex_prop) %>%
  select(-total) # %>%
  # write.csv("infection-modeling/data/peromyscus-maturity-table.csv")

## ---- Save the peromyscus table ----

saveRDS(Peros, "infection-modeling/data/peromyscus-data.rds")