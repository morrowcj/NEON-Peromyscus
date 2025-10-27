library(tidyverse)

Peros <- readRDS("infection-modeling/data/peromyscus-model-data.rds")

# Ensure that all observations of an individual use the Gel_Taxon ID when available
new_Peros <- Peros %>%
  group_by(iid) %>%
  arrange(is.na(Gel_Taxon)) %>%
  mutate(
    Gel_Taxon = first(Gel_Taxon),
    agreement = Gel_Taxon == new_taxonID,
    tail_body_rat = tailLength/weight
  ) %>%
  ungroup()

# fit linear discriminat analysis using weight, ear, and foot
DFAfit_earFoot <- MASS::lda(
  # Gel_Taxon ~ earLength + tailLength + weight + tail_body_rat,
  Gel_Taxon ~ weight*earLength*hindfootLength*
    sex_male*sex_mature, # +
    # year + DOY,
  data = new_Peros
)

DFA_pred_earFoot <- predict(DFAfit_earFoot)

PELE_prob_earFoot <- predict(DFAfit_earFoot, newdata = new_Peros)$posterior[, "PELE"]

# DFAfit_weightSex <- MASS::lda(
#   Gel_Taxon ~ weight*sex_male*sex_mature*DOY,
#   data = new_Peros
# )
#
# DFA_pred_weightSex <- predict(DFAfit_weightSex, newdata = new_Peros)$posterior[, "PELE"]
#
# new_Peros %>% ungroup() %>%
#   mutate(
#     y = DFA_pred_weightSex,
#     x = case_when(
#       Gel_Taxon == "PELE" & y >= 0.5 ~ TRUE,
#       Gel_Taxon == "PEMA" & y < 0.5 ~ TRUE,
#       !is.na(Gel_Taxon) & !is.na(y) ~ FALSE,
#       .default = NA
#       )
#   ) %>%
#   filter(!is.na(Gel_Taxon), !is.na(y)) %>%
#   group_by(siteID, Gel_Taxon) %>%
#   summarize(
#     true_n = n(), dfa_n = sum(x, na.rm = TRUE),
#     p = sum(x, na.rm = TRUE) / sum(!is.na(x))
#   )

new_Peros <- new_Peros %>% ungroup() %>%
  mutate(
    PELE_prob = .env$PELE_prob_earFoot,
    PELE_prob = if_else(!is.finite(PELE_prob), NA, PELE_prob)
  ) %>%
  group_by(iid) %>%
  mutate(
    avg_PELE_prob = mean(.data$PELE_prob, na.rm = TRUE),
    DFA_agreement = case_when(
      Gel_Taxon == "PELE" & .data$PELE_prob >= 0.5 ~ TRUE,
      Gel_Taxon == "PEMA" & .data$PELE_prob < 0.5 ~ TRUE,
      !is.na(Gel_Taxon) ~ FALSE,
      .default = NA
    ),
    iid_DFA_agreement = case_when(
      Gel_Taxon == "PELE" & avg_PELE_prob >= 0.5 ~ TRUE,
      Gel_Taxon == "PEMA" & avg_PELE_prob < 0.5 ~ TRUE,
      !is.na(Gel_Taxon) ~ FALSE,
      .default = NA
    )
  )


## Agreement among Gels
obs_rate = new_Peros %>% filter(!is.na(Gel_Taxon), !is.na(PELE_prob)) %>%
  group_by(Gel_Taxon, siteID) %>%
  summarize(
    n_obs = n(),
    correct_rate_obs = sum(DFA_agreement) / n(),
  )

ind_rate = new_Peros %>% ungroup() %>%
  filter(!is.na(Gel_Taxon), !is.na(avg_PELE_prob)) %>%
  select(siteID, iid, Gel_Taxon, avg_PELE_prob, iid_DFA_agreement) %>%
  distinct() %>%
  group_by(Gel_Taxon, siteID) %>%
  summarize(
    n_inds = n(),
    correct_rate_ind = sum(iid_DFA_agreement) / n()
  )

# DFA success rate:
left_join(obs_rate, ind_rate) %>%
  rename() %>%
  arrange(siteID, Gel_Taxon)

## overall success rate:
new_Peros %>%
  ungroup() %>%
  filter(!is.na(Gel_Taxon), !is.na(avg_PELE_prob)) %>%
  # group_by(Gel_Taxon) %>%
  summarize(n = n(), p = sum(iid_DFA_agreement, na.rm = TRUE) / n())

## Proportion of ambiguous individuals that can be IDd this way:
new_Peros %>%
  ungroup() %>%
  filter(new_taxonID %in% c("PESP", "PELEPEMA")) %>%
  summarize(fixed_prop = sum(!is.na(avg_PELE_prob)) / n())

# DFA_Peros <- complete_PEROS %>%
#   # select(iid, cap_num, year, Gel_Taxon, DFA_PELE_PROB) %>%
#   mutate(
#     agreement = case_when(
#       Gel_Taxon == "PELE" & DFA_PELE_PROB >= 0.5 ~ TRUE,
#       Gel_Taxon == "PEMA" & DFA_PELE_PROB < 0.5 ~ TRUE,
#       .default = FALSE
#     )
#   )
#
# new_Peros %>% filter(new_taxonID %in% c("PELEPEMA", "PESP"),
#   !is.na(weight), !is.na(earLength),
#   !is.na(hindfootLength), !is.na(sex_male), !is.na(sex_mature)
# ) %>% nrow()
#
# DFA_Peros %>% summarize(sum(agreement)/n())

new_Peros %>%
  filter(year >= 2020) %>%
  select(
    iid, cap_num, year, taxonID, new_taxonID, Gel_Taxon,
    PELE_prob_obs = PELE_prob, PELE_prob_iid = avg_PELE_prob,
    DFA_agreement, iid_DFA_agreement
  ) %>% arrange(is.na(PELE_prob_iid), is.na(Gel_Taxon), year, iid, cap_num) %>%
  write.csv("C:/Users/morrow5/Downloads/species_ID_table.csv")


new_Peros %>% ungroup() %>%
  filter(!is.na(PELE_prob)) %>%
  mutate(x = row_number()) %>%
  ggplot(aes(x = x, y = PELE_prob)) +
  geom_point()
