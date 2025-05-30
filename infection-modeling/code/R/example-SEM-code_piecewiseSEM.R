# SEM walkthrough

# load tidyverse (for visuals)
library(tidyverse)

# load the SEM packages
library(lavaan)
library(piecewiseSEM)

# load the keeley dataset (plant diversity following disturbance in CA)
data("keeley", package = "piecewiseSEM")

# convert the dataset to a tibble
keeley <- keeley |> tibble()

## ---- Simple linear regression ----

# fit an SEM with lavaan (character string formula)
sem1 <- sem("firesev ~ age", data = keeley)

# look at the results
summary(sem1)

# fit a linear model for comparison
lm1 <- lm("firesev ~ age", data = keeley)

# look at the coefficients
coefficients(summary(lm1))

# add intercept into SEM
sem1_int <- sem("firesev ~ age", data = keeley, meanstructure = TRUE)

# indentical slope and intercept.
summary(sem1_int)

# lm coefficients
coefs(lm1)

# sem coefficients
standardizedsolution(sem1_int)

# add standardized coefficients and R2 to the output
summary(sem1_int, standardize = TRUE, rsq = TRUE)


## ---- SEM ----

# create a formula, using lavaan syntax (one equation per line)
# model: (age -> firesev -> cover)
sem_form <- "
firesev ~ age
cover ~ firesev
"

# fit new model
sem2 <- sem(sem_form, data = keeley)

# output
summary(sem2, standardized = TRUE, rsq = TRUE)

# ALL the fit statistics
fitmeasures(sem2)

# new formula, specifying coefficient names and that we want the indirect effect of age on cover
sem_form.2 <- "
firesev ~ B1 * age
cover ~ B2 * firesev

indirect := B1 * B2
"

# fit
sem2.update <- sem(sem_form.2, data = keeley)

# view results # note the indirect coef = B1 * B2 (from "Std.all")
summary(sem2.update, standardized = TRUE, rsq = TRUE)

## ---- alternate configuration SEM ----

# Here, age affects cover directly, and indirectly through fire severity
formula3 <- '
firesev ~ age
cover ~ firesev + age
'

# fit
sem3 <- sem(formula3, data = keeley)
# results
summary(sem3, standardize = T, rsq = TRUE)
# compare to previous model
anova(sem3, sem2)

## ---- Local estimation ----

# 1. each component is fitted separately, and need only meet assumptions of that piece
# 2. pieces are "strung" together.

## piecewiseSEM

# fit a simple pSEM
keeley_psem <- psem(
  lm(cover ~ firesev, data = keeley),
  lm(firesev ~ age, data = keeley)
)

# look at the basis set (missing links)
basisSet(keeley_psem)

# directed separation tests
dSep(keeley_psem, .progressBar = FALSE)
## P > 0.05 indicates that the missing link is not significant (right to exclude)


# calculate fischer C 
fisherC(keeley_psem)
## P > 0.05 indicates a good fit (model chisq not different from zero, which is a perfect fit)

# Refit saturated model (include missing link)
keeley_psem2 <- psem(
  lm(cover ~ firesev + age, data = keeley),
  lm(firesev ~ age, data = keeley),
  data = keeley
)
summary(keeley_psem2)

# get the log-likelihoods (original model)
LLchisq(keeley_psem)

# AIC
AIC(keeley_psem)
# using fisher's C and dsep test
AIC(keeley_psem, AIC.type = "dsep")

## ---- GLM pSEM ----

# load GLM packages
library(nlme)
library(lme4)

# load shipley (hierarchical) dataset
data("shipley", package = "piecewiseSEM")
shipley <- shipley |> tibble()

# model lat -> DD -> Date -> Growth -> Survival. Survival (Live) is binary
## fit pSEM
shipley_psem <- psem(
  # 1. latitude -> degree day, with tree nested within site rand. effect
  lme(DD ~ lat, random = ~ 1 | site / tree, na.action = na.omit,
      data = shipley),
  # 2. degree day -> bud burst date, with tree nested within site
  lme(Date ~ DD, random = ~ 1 | site / tree, na.action = na.omit,
      data = shipley),
  # 3. budburst date -> growth, with tree nested within site
  lme(Growth ~ Date, random = ~ 1 | site / tree, na.action = na.omit,
      data = shipley),
  # 4. logistic GLMM for growth -> survival, with tree within site
  glmer(Live ~ Growth + (1 | site) + (1 | tree),
        family = binomial(link = "logit"), data = shipley)
)

## ---- Nonliner ----
set.seed(100)
n <- 100
x1 <- rchisq(n, 7)
mu2 <- 10*x1/(5 + x1)
x2 <- rnorm(n, mu2, 1)
x2[x2 <= 0] <- 0.1
x3 <- rpois(n, lambda = (0.5*x2))
x4 <- rpois(n, lambda = (0.5*x2))
p.x5 <- exp(-0.5*x3 + 0.5*x4)/(1 + exp(-0.5*x3 + 0.5*x4))
x5 <- rbinom(n, size = 1, prob = p.x5)
dat2 <- data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5) |> tibble()

## Strictly linear SEM, assuming multivariate normality (bad assumption)
shipley_psem2 <- psem(
  lm(x2 ~ x1, data = dat2),
  lm(x3 ~ x2, data = dat2),
  lm(x4 ~ x2, data = dat2),
  lm(x5 ~ x3 + x4, data = dat2)
)
LLchisq(shipley_psem2) # actually a good fit, surprisingly

library(mgcv)

# fit with GAM and GLM models:
shipley_psem3 <- psem(
  gam(x2 ~ s(x1), data = dat2, family = gaussian),
  glm(x3 ~ x2, data = dat2, family = poisson),
  gam(x4 ~ x2, data = dat2, family = poisson),
  glm(x5 ~ x3 + x4, data = dat2, family = binomial)
)
LLchisq(shipley_psem3) # also adequate fit

# compare
AIC(shipley_psem2, shipley_psem3) # second SEM is better

# view best results
summary(shipley_psem3)


# ---- Missing data ----

# copy the original data
ship_NA <- shipley
NA_index <- sample(x = nrow(ship_NA), size = 0.5*nrow(ship_NA))
ship_NA[NA_index, "Growth"] <- NA

# fit individual models
DD_lat <-   lme(
  DD ~ lat, random = ~ 1 | site / tree, na.action = na.omit, data = ship_NA
)
n1 <- summary(DD_lat)$dims$N
Date_DD <- lme(
  Date ~ DD, random = ~ 1 | site / tree, na.action = na.omit, data = ship_NA
)
n2 <- summary(Date_DD)$dims$N
Growth_Date <- lme(
  Growth ~ Date, random = ~ 1 | site / tree, na.action = na.omit, 
  data = ship_NA
)
n3 <- summary(Growth_Date)$dims$N
Live_Growth <- glmer(
  Live ~ Growth + (1 | site) + (1 | tree), family = binomial(link = "logit"), 
  data = ship_NA
)
n4 <- nrow(Live_Growth@frame)
# print the sample size for each model
c(n1, n2, n3, n4)

# fit the SEM model
ship_NA_psem <- psem(
  DD_lat, Date_DD, Growth_Date, Live_Growth
)

# look at results
summary(ship_NA_psem)

# ---- binomial response ----
x <- rnorm(20)
x <- x[order(x)]
y <- c(rbinom(10, 1, 0.8), rbinom(10, 1, 0.2))
glm_model <- glm(y ~ x, data = data.frame(x = x, y = y), "binomial")
xpred <- seq(min(x), max(x), 0.01)
ypred <- predict(glm_model, list(x = xpred), type = "response")
plot(x, y)
lines(xpred, ypred)

# ypred_link <- predict(glm_model, list(x = xpred), type = "link")
# plot(xpred, ypred_link)

# get beta from model
beta <- summary(glm_model)$coefficients[2, 1]
preds <- predict(glm_model, type = "link") # linear predictions
# latent theoretic
sd.ystar <- sqrt(var(preds) + (pi^2)/3) # for default logit-link
beta_lt <- beta * sd(x)/sd.ystar
## obtain using `coefs`
coefs(glm_model, standardize.type = "latent.linear"); beta_lt
# observation empirical
R2 <- cor(y, predict(glm_model, type = "response"))^2 # non-linear predictions
sd.yhat <- sqrt(var(preds)/R2)
beta_oe <- beta * sd(x)/sd.yhat
## obtain using `coefs`
coefs(glm_model, standardize.type = "Menard.OE"); beta_oe

# Poisson model
set.seed(100)
count_data <- data.frame(y = rpois(100, 10))
count_data$x <- count_data$y * runif(100, 0, 5)

# bivariate coefficient (i.e., lm regression coef)
with(count_data, cor(x, log(y)))

# fit a GLM
glm_model2 <- glm(y ~ x, family = poisson(link = "log"), count_data)
coef(glm_model2)[2] # unstandardized (link-scale)

# calculate observation-empiracal variance
R2 <- cor(count_data$y, predict(glm_model2, type = "response"))^2 # non-linear predictions
sd.yhat <- sqrt(var(predict(glm_model2, type = "link"))/R2)
coef(glm_model2)[2] * sd(count_data$x)/sd.yhat # equivalent to lm coef above

# ---- Categorical Variables ----

set.seed(120)
dat <- tibble(
  group = rep(c(1, 2), each = 50), x = runif(100), 
  y = x + group*0.5 + runif(100) + 0.3*x*group, z = y + runif(100)
)
model <- lm( ~ group*x + z, data = dat)
summary(model)

# estimate the marginal means for each group, with pairwise test
library(emmeans)
emmeans(model, list(pairwise ~ group)) # where specs is the variable or list of variables whose means are to be estimated

# piecewiseSEM's `coef` does this internally!
coefs(model)

# ---- break one path into two models?? ----

lm1 <- lm(Survival ~ log(Growth), data = shipley)
lm2 <- lm(Survival ~ log(Date), data = shipley)
lm3 <- lm(log(Growth) ~ Date, data = shipley)
psem1 <- try(psem(lm1, lm2, lm3)) # Does not work...
try(coefs(psem1))

psem2 <- psem(
  lm(Survival ~ log(Growth) + Date, data = shipley),
  lm(log(Growth) ~ Date, data = shipley)
)

coefs(psem2)

# ---- multigroup analysis (global) ----
set.seed(111)
dat <- data.frame(x = runif(100), group = rep(letters[1:2], each = 50))
dat$y <- dat$x + runif(100)
dat$z <- dat$y + runif(100)
multigroup.model <- '
y ~ x
z ~ y
'

library(lavaan)

# lavan multigroup model
multigroup1 <- sem(multigroup.model, dat, group = "group")

summary(multigroup1)

# constrained multigroup model
multigroup1.constrained <- sem(multigroup.model, dat, group = "group", group.equal = c("intercepts", "regressions"))

summary(multigroup1.constrained)

# compare
anova(multigroup1, multigroup1.constrained)

# now with constraints (coefficient fixed among groups)
multigroup.model2 <- '
y ~ c("b1", "b1") * x
z ~ y
'

# fit 
multigroup2 <- sem(multigroup.model2, dat, group = "group")
# compare
anova(multigroup1, multigroup2)

# now with constraint on the second path
multigroup.model3 <- '
y ~ x
z ~ c("b2", "b2") * y
'
multigroup3 <- sem(multigroup.model3, dat, group = "group")
anova(multigroup1, multigroup3)

summary(multigroup3)

# ---- multigroup analysis (local) ----

# library(piecewiseSEM)
# 
# pmodel <- psem(
#   lm(y ~ x, data.frame(dat)),
#   lm(z ~ y, data.frame(dat))
# )
# 
# (pmultigroup <- multigroup(pmodel, group = "group"))


# ---- multigroup analysis (example) ----

data(meadows)
meadows <- tibble(meadows)

# basic SEM
jutila_model <- '
rich ~ elev + mass
mass ~ elev
'
jutila_lavaan_free <- sem(jutila_model, meadows, group = "grazed")
summary(jutila_lavaan_free)

# constrain all paths
jutila_lavaan_constrained <- sem(
  jutila_model, meadows, group = "grazed", 
  group.equal = c("intercepts", "regressions")
)
# significantly different (but poor fit)
anova(jutila_lavaan_free, jutila_lavaan_constrained)
# sequentially relax constraints
jutila_model2 <- '
rich ~ elev + mass
mass ~ c("b1", "b1") * elev
'
jutila_lavaan2 <- sem(jutila_model2, meadows, group = "grazed")
anova(jutila_lavaan_free, jutila_lavaan2)
# elev -> rich
jutila_model3 <- '
rich ~ c("b2", "b2") * elev + mass
mass ~ elev
'
jutila_lavaan3 <- sem(jutila_model3, meadows, group = "grazed")
anova(jutila_lavaan_free, jutila_lavaan3)
# mass -> rich
jutila_model4 <- '
rich ~ elev + c("b3", "b3") * mass
mass ~ elev
'
jutila_lavaan4 <- sem(jutila_model4, meadows, group = "grazed")
anova(jutila_lavaan_free, jutila_lavaan4)
# only the elev -> rich path is not different from free, indicating it can be restrained
summary(jutila_lavaan3)

# now with piecewise
jutila_psem <- psem(
  lm(rich ~ elev + mass, meadows),
  lm(mass ~ elev, meadows)
)

# the coefficients for rich->mass and mass->elev differ among grazed groups
  # while rich->elev does not:
(mg <- multigroup(jutila_psem, group = "grazed"))

# ---- Latent variables ----
set.seed(11)
x <- rnorm(10)
# x sampled 4 times
x.list <- lapply(1:5, function(i) x + runif(10, 0, 2))
x. <- unlist(x.list)
# compute correlation among trials
combos <- combn(1:5, 2)
cors <- c()
for(i in 1:ncol(combos)) cors <- c(cors, cor(x.list[[combos[1, i]]], x.list[[combos[2, i]]]))
(r <- mean(cors)) 
# obtain path coefficient and error variance
sqrt(r) # path coefficient
1 - r # standardized error variance
(1 - r) * var(unlist(x.list)) # unstandardized error variance

# exogenous and endogenous
set.seed(3)
y <- x + runif(10, 0, 5)
y.list <- lapply(1:5, function(i) y + runif(10, 0, 2))
y. <- unlist(y.list)
xy_model <- lm(y. ~ x.)
beta <- summary(xy_model)$coefficients[2, 1]
(beta_std <- beta * (sd(x.) / sd(y.))) # standardized
cor(x., y.) # same as the standardized coefficient for simple regression
# estimate of loading (for x's indicator)coefficient between 2 latent variables
(gamma <- beta_std / sqrt(r))
# unexplained variance
1 - gamma^2
# compare to regression residual variance
1 - summary(xy_model)$r.squared

# ---- lavaan latent variables ----
library(lavaan)
(1 - r) * var(x.) # unstandardized error variance
# model spec
latent_formula1 <- '
xi =~ x # exogenous latent
eta =~ y # endogenous latent

eta ~ xi # path model

x ~~ 0.213 * x # fix error variance
'
latent_model1 <- sem(latent_formula1, data.frame(x = x., y = y.))
summary(latent_model1, standardize = T, rsq = T)

# fix y variance now
(1 - r.y) * var(y.) # unstandardized error variance
latent_formula2 <- '
xi =~ x # exogenous latent
eta =~ y # endogenous latent

eta ~ xi # path model

y ~~ 0.338 * y # fix error variance
'
latent_model2 <- sem(latent_formula2, data.frame(x = x., y = y.))
summary(latent_model2, standardize = T, rsq = T)

# ---- Composite ----
library(piecewiseSEM)
data(keeley)
keeley = tibble(keeley)
cover_model <- lm(rich ~ cover + I(cover^2), keeley)
summary(cover_model)
cover_coef <- coefficients(summary(cover_model))["cover", "Estimate"]
sqrcover_coef <- coefficients(summary(cover_model))["I(cover^2)", "Estimate"]
keeley <- keeley |> 
  mutate(
    cover_sq = cover^2,
    cover_composite = cover_coef*cover + sqrcover_coef*cover_sq
  )

comp_fm <- lm(rich ~ cover_composite, data = keeley)
summary(comp_fm)
coefs(comp_fm)

keeley_psem <- psem(
  lm(cover ~ firesev, keeley),
  lm(rich ~ cover_composite + firesev, keeley)
)

summary(keeley_psem, .progressBar = FALSE)
