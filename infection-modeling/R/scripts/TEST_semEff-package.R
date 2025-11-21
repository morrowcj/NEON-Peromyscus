# load libraries
library(dplyr)
library(piecewiseSEM)
library(semEff)

# load data
# data("shipley")

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
pseffs <- piecewiseSEM:::stdCoefs(shipley.sem, data = shipley, intercepts = TRUE)

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
