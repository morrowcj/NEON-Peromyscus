## This file depends upon the data created by the "01_prepare-mammal-data.R"
## script. The full pipeline is tracked in the "index.Rmd" file (project root)

# reformat the immune data to be usable.
library(tidyverse)
library(vegan)

# read mammal trap data
mammals <- readRDS("infection-modeling/data/mammal-data.rds")
# count the trapping observations
n_mammals = nrow(mammals)

# read the excel file containing immune + infection data
immunity <- readxl::read_excel(
  "infection-modeling/data/Immune and burden Table.xlsx", 
  na = c("", "NA")
) %>% tibble()

# clean it and keep only needed columns
immunity <- immunity %>% 
  select(
    # Linking ID variables
    plotID, tagID = tagID.x, collectDate, trapCoordinate, 
    # updated taxon information
    DFA_Taxon = `DFA ID`, Gel_Taxon = `Gel ID`, 
    # geneetic info
    genetics_ID = `RNA/DNA ID #`, 
    Bb_burden = `Bb burden (OspA/10kRpp30)`, Bb_status = `Bb Pos/Neg`,
    `IL-10`:`GATA-3`,
    # individual traits
  ) %>% 
  distinct() %>% 
  mutate(Bb_infected = as.numeric(Bb_status == "Positive")) %>% 
  relocate(Bb_infected, .after = Bb_status) %>% 
  relocate(`TGF-B`, .before = `GATA-3`)


# rows in mammals that aren't in immunity
unmatched <- anti_join(
  immunity, mammals, by = c("plotID", "tagID", "collectDate", "trapCoordinate")
)

# save the unmatched as a table
if (FALSE) {
  unmatched %>% arrange(tagID) %>%
    write.csv(file = "infection-modeling/data/unmatched_rows_immunteTable.csv")
}

# get the rows which are duplicated
duplicates <- immunity %>% 
  group_by(tagID) %>% 
  tally() %>% 
  full_join(immunity, by = "tagID") %>% 
  filter(n > 1) %>% 
  arrange(tagID, plotID, ymd(collectDate), trapCoordinate) %>% 
  select(-n)

# save the duplicates as a table
if (FALSE) {
  duplicates %>%
    write.csv(file = "infection-modeling/data/duplicated_tagIDs_immuneTable.csv")
}

# remove the duplicated sample (with two slightly different Bb_burdens)
immunity <- immunity %>% 
  filter( !(tagID == "NEON.MAM.D07.R5240" & Bb_burden > 25.2) )

# add the trap obvservation ID
immunity <- immunity %>% 
  left_join(
    mammals %>% select(uid, year, plotID, trapCoordinate, collectDate, tagID), 
    by = c("plotID", "trapCoordinate", "collectDate", "tagID")
  ) %>% 
  select(uid, year, DFA_Taxon:`GATA-3`)
  # relocate(uid, .before = 0)

# lookup table for gene funcitonal groups
gene_lookup <- tibble(
  gene = immunity %>% select(`IL-10`:`GATA-3`) %>% names(),
  triplex = rep(1:2, each = 3),
  group = c(rep("resistance", 4), rep("tolerance", 2))
)

# save gene lookup table
saveRDS(gene_lookup, "infection-modeling/data/expression-lookup.rds")

# save cleaned version
saveRDS(immunity, "infection-modeling/data/immune-data.rds")

## ---- PCA ----

# complete expression data for PCA
ipca_dat <- immunity %>% 
  select(uid, `IL-10`:`GATA-3`) %>% 
  filter(complete.cases(.))

# conduct PCA
ipca <- ipca_dat %>% select(-uid) %>% 
  mutate(across(gene_lookup$gene, ~log(.x + 1))) %>% 
  vegan::pca(scale = TRUE)

# fit the "environmental" borrelia variables 
Bb_envfit <- immunity %>% filter(uid %in% ipca_dat$uid) %>% 
  select(Bb_burden, Bb_status) %>% 
  envfit(ipca, ., na.rm = TRUE)

# extract the scores from PCA
ipca_scores <- scores(ipca)

# construct the species scores table (with gene info)
ipca_sp <- ipca_scores$species %>% data.frame() %>% 
  rownames_to_column("gene") %>% 
  full_join(gene_lookup, by = "gene") %>% 
  tibble()

# and the site scores table (with uid)
ipca_st <- bind_cols(
  ipca_dat, 
  ipca_scores$sites %>% data.frame()
) %>% 
  tibble()

# build a list of PCA objects to save
ipca_list <- list(
  site_scores = ipca_st %>% 
    select(uid, expr_PC1 = PC1, expr_PC2 = PC2),
  species_scores = ipca_sp %>% 
    rename(expr_PC1 = PC1, expr_PC2 = PC2),
  pca_obj = ipca,
  data = ipca_dat,
  envfit_obj = Bb_envfit
)

# save the output
saveRDS(ipca_list, "infection-modeling/data/immune-PCA.rds")

## ---- PCA (2023 - 2024) ----

# This is because there is a correlation...

# subset data to exclude 2022
ipca_dat_2324 <- immunity %>% 
  filter(year != 2022) %>% 
  select(uid, `IL-10`:`GATA-3`) %>% 
  filter(complete.cases(.)) 

# conduct PCA
ipca2324 <- ipca_dat_2324 %>% select(-uid) %>% 
  mutate(across(gene_lookup$gene, ~log(.x + 1))) %>% 
  vegan::pca(scale = TRUE)

# fit the "environmental" borrelia variables 
Bb_envfit2324 <- immunity %>% filter(uid %in% ipca_dat_2324$uid) %>% 
  select(Bb_burden, Bb_status) %>% 
  envfit(ipca2324, ., na.rm = TRUE)

# ordiplot(ipca2324) %>% 
#   orditorp(display = "species"); plot(Bb_envfit, add = TRUE)

# extract the scores from PCA
ipca2324_scores <- scores(ipca2324)

# construct the species scores table (with gene info)
ipca2324_sp <- ipca2324_scores$species %>% data.frame() %>% 
  rownames_to_column("gene") %>% 
  full_join(gene_lookup, by = "gene") %>% 
  tibble()

# and the site scores table (with uid)
ipca2324_st <- bind_cols(
  ipca_dat_2324, 
  ipca2324_scores$sites %>% data.frame()
) %>% 
  tibble()

# build a list of PCA objects to save
ipca2324_list <- list(
  site_scores = ipca2324_st %>% 
    select(uid, expr_PC1 = PC1, expr_PC2 = PC2),
  species_scores = ipca2324_sp %>% 
    rename(expr_PC1 = PC1, expr_PC2 = PC2),
  pca_obj = ipca2324,
  data = ipca_dat_2324,
  envfit_obj = Bb_envfit2324
)

# save the output
saveRDS(ipca2324_list, "infection-modeling/data/immune-PCA_no2022.rds")
