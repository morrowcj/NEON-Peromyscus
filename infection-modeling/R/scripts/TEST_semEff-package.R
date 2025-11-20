# load libraries
library(tidyverse)
library(piecewiseSEM)
library(semEff)

# load data
data("shipley")
shipley <- tibble(shipley)

# ----  fit with semEff ----

## Note: much of the code below is commented out, because the values have
## been pre-calculated and included with the semEff package

# # model list
# shipley.sem <- list(
#   DD = lme4::lmer(DD ~ lat + (1 | site) + (1 | tree), data = shipley),
#   Date = lme4::lmer(Date ~ DD + (1 | site) + (1 | tree), data = shipley),
#   Growth = lme4::lmer(Growth ~ Date + (1 | site) + (1 | tree),
#                       data = shipley),
#   Live = lme4::glmer(Live ~ Growth + (1 | site) + (1 | tree), binomial,
#                      data = shipley)
# )
# # bootsrap effects
# shipley.sem.boot <- bootEff(shipley.sem, R = 1000, seed = 13,
#                             ran.eff = "site")
# # get the effects
# shipley.sem.eff <- semEff(shipley.sem.boot)

# View the effects
shipley.effects <- summary(shipley.sem.eff)
shipley.direct <- getDirEff(shipley.sem.eff, type = "orig")

# ---- Fit with piecewiseSEM ----
shipley.psem <- as.psem(shipley.sem)
shipley.psem.coefs <- coefs(shipley.psem)

# ---- Compare ----

# table comparing the effects:
shipley.psem.coefs %>%
  select(Response, Predictor, psem = Std.Estimate) %>%
  mutate(
    semEff = lapply(shipley.direct, function(x) x[-1]) %>% unlist(),
    across(c(psem, semEff), ~round(.x, 3))
  )
## Note that Growth->Live effect differs between the methods.
## This is because the two packages handle binomial/glms differently.
## I need to figure out how to transform

# Check to see if perhaps the alternative standardization is matched
coefs(shipley.psem, standardize.type = "Menard.OE") %>%
  select(Response, Predictor, Std.Estimate) # nope.
