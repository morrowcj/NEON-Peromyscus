# load libraries
library(dplyr)
library(piecewiseSEM)
library(semEff)

# load data
data("shipley")

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
# shipley.effects <- summary(shipley.sem.eff)
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
  data.frame() %>%
  filter(Response == "Live") %>% pull(Std.Estimate) # 0.5292

## Check if unique.eff helps
stdEff(shipley.sem, unique.eff = FALSE)$Live["Growth"] # 0.368

## ---- piecewiseSEM standardization (binomial) ----

## Within piecewiseSEM, coefficient standardizations are done by...

## 1. extracting the unstandardized effects (B)

## 2. estimating standard deviations (sd.x, sd.y) via piecewiseSEM:::GetSDx()

## 3. standarize it: B * (sd.x / sd.y)

## sd.x is simply the sd of the predictor variables in a model.

## sd.y is simply the sd(Y) for gaussian data but otherwise uses scaleGLM()

## latent.linear: sigmaE = pi^2/3 (logit) or 1 (probit) and sd.y = sqrt(var(preds) + sigmaE)
## In this case, preds = predict(model, type = "link").

## Menard.OE: R = cor(cbind(X, y), preds2) and sd.y = sqrt(var(preds)) / R
## In this case, preds2 = predict(model, type = "response") and preds is as above.

## ---- semEff ----

# output structure is a named list of coefficients
(seffs <- stdEff(shipley.sem))

# piecewiseSEM version's outputs a table...
pseffs <- piecewiseSEM:::stdCoefs(shipley.sem, data = shipley)

# reformat pseffs to be like seffs
Lpseffs <- lapply(
  unique(pseffs$Response),
  function(x){
    out = pseffs$Std.Estimate[pseffs$Response == x]
    names(out) = pseffs$Predictor[pseffs$Response == x]
    out
  }
)
names(Lpseffs) <- unique(pseffs$Response)
Lpseffs
## Clearly something is wrong with piecewiseSEM's intercept calculation...

# f <- function(x) {return(x)}
#
# x = 4
# do.call(f, list(x = x))
#
# f <- match.fun("f")
#
# do.call(f, list(x = x))
#
# f = match.fun(function(x)return(x))
#
# do.call(f, list(x = x))

## ---- Conversion tests.
if (!exists("shipley.boot.og") | !exists("shipley.boot.alt")) {
  shipley.boot.og <- bootEff(
    shipley.sem, R = 100, seed = 13, ran.eff = "site", ncpus = 8
  )
  shipley.boot.alt <- bootEff(
    shipley.sem, R = 100, seed = 13, ran.eff = "site", ncpus = 8,
    std.y = FALSE, std.x = FALSE,
  )
}

alt.copy <- shipley.boot.alt

SDy_list <- lapply(
  shipley.sem, function(fm){
    piecewiseSEM:::GetSDy(model = fm, data = shipley)
  }
)

SDx_list <- lapply(
  shipley.sem, function(fm){
    piecewiseSEM:::GetSDx(model = fm, data = shipley)
  }
)


responses <- names(shipley.sem)

for (resp in names(shipley.sem)) {
  sdy = SDy_list[[resp]]

  for (x in names(SDx_list[[resp]])) {
    sdx = SDx_list[[resp]][x]
    shipley.boot.alt[[resp]]$t[, x] <- shipley.boot.alt[[resp]]$t[, x] * (sdx / sdy)
    shipley.boot.alt[[resp]]$t0[x] <- shipley.boot.alt[[resp]]$t0[x] * (sdx / sdy)
  }
}


og.effs <- semEff(shipley.boot.og)
alt.effs <- semEff(shipley.boot.alt)

getDirEff(alt.effs)

shipley.og.recalc <- shipley.boot.alt
sub_mods <- names(shipley.og.recalc)

for(sub in sub_mods) {
  shipley.og.recalc[[sub]]$t = shipley.boot.og[[sub]]$t
  shipley.og.recalc[[sub]]$t0 = shipley.boot.og[[sub]]$t0
}

# lapply(shipley.boot.alt, function(x)x$t)
# shipley.og.recalc$Growth$t <- shipley.boot.og$Growth$t
# shipley.og.recalc$Growth$t0 <- apply(shipley.og.recalc$Growth$t, 2, mean)

summary(semEff(shipley.boot.alt), "Growth")
summary(semEff(shipley.og.recalc), "Growth")
summary(semEff(shipley.boot.og), "Growth")

sdGrowth <- piecewiseSEM:::GetSDy(model = shipley.sem$Growth, data = shipley)


### ----

## https://github.com/murphymv/semEff/issues/56

# Bootstrap shipley.sem without standardization
boot.raw <- bootEff(
  shipley.sem, R = 100, seed = 13, ran.eff = "site", ncpus = 8,
  std.y = FALSE, std.x = FALSE,
)

# Get response standardizations
SDy_list <- lapply(
  shipley.sem, function(fm){
    piecewiseSEM:::GetSDy(model = fm, data = shipley)
  }
)

# Get predictor standardizations
SDx_list <- lapply(
  shipley.sem, function(fm){
    piecewiseSEM:::GetSDx(model = fm, data = shipley)
  }
)

# Mannually standardize the bootstrap results
for (resp in names(shipley.sem)) {
  # sd of this response
  sdy = SDy_list[[resp]]

  for (pred in names(SDx_list[[resp]])) {
    # sd of this predictor
    sdx = SDx_list[[resp]][pred]
    # rescale the bootstrapped coefficients
    boot.raw[[resp]]$t[, pred] <- boot.raw[[resp]]$t[, pred] * (sdx / sdy)
    # rescale the mean boostrapped coefficient
    boot.raw[[resp]]$t0[pred] <- boot.raw[[resp]]$t0[pred] * (sdx / sdy)
  }
}

# Calculate the effects from the boot object
alt_effs <- semEff(boot.raw)

# extract direct effects
getDirEff(alt_effs)

# compare to direct effects of psem version
pseffs <- piecewiseSEM:::stdCoefs(shipley.sem, data = shipley)
pseffs %>% select(Response, Predictor, Std.Estimate) # same!

# But... can the SEs, CIs, etc. be trusted?
summary(alt_effs, "Live")
summary(shipley.sem.eff, "Live") # conclusions don't differ for this data set.
