---
title: "Resistance and Tolerance"
author: "Clay Morrow"
date: "2025-07-09"
output: 
  bookdown::html_document2:
params:
  force: FALSE
  
---









## Introduction

This document will track the analysis of the resistance to and tolerance of
*Borrelia* (B.b.) in *Peromyscus* mice at NEON.

## Data

First, we'll load in the data from Vania and rename a few of the columns.


``` r
# read the excel file containing immune + infection data
df <- readxl::read_excel(
  "infection-modeling/data/Immune and burden Table.xlsx", na = c("", "NA")
) %>% tibble()

# clean it and keep only needed columns
df <- df %>% 
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
  ) %>% distinct() %>% 
  relocate(`TGF-B`, .before = `GATA-3`)
```

Then we'll merge these data with the small mammal trapping data from NEON.


``` r
# read mammal trap data
mammals <- readRDS("infection-modeling/data/full-data.rds")
# count the trapping observations
n_mammals = nrow(mammals)
```




``` r
# join the tables together
mammals <- left_join(
  mammals, df, by = c("plotID", "trapCoordinate", "collectDate", "tagID")
)
# check that the rows haven't changed
stopifnot(nrow(mammals) == n_mammals)

mammals <- mammals %>% 
  mutate(Bb_infected = (Bb_status == "Positive"))
```

## Genetic correlations

First, let's look at how the genetic traits are correlated with each other. 
The figure below shows that the genetic traits are nearly all significantly
intercorrelated (the lone exception being TGF-B and IL-6, which are 
uncorrelated). Notably, the resistance traits (IL-10, IFN-y, IL-6, TLR-2) are
most strongly correlated with each other and, similarly, the tolerance traits
(TGF-B and GATA-3) are most strongly correlated with each each other. Both
TGF-B and GATA-3 appear to be zero inflated and all the genetic traits 
are highly right-skewed (i.e., many low values).

<div class="figure">
<img src="F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/gene-cors-1.png" alt="Genetic correlations"  />
<p class="caption">(\#fig:gene-cors)Genetic correlations</p>
</div>

### PCA

The resistance and tolerance genetic traits also broadly group together
in a PCA (expression variables log(x + 1) transformed prior to ordination): 

<div class="figure">
<img src="F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/gene-PCA-1.png" alt="Genetic PCA"  />
<p class="caption">(\#fig:gene-PCA)Genetic PCA</p>
</div>

It appears that Bb infection status is significantly associated with our
expression PCs, while burden is only marginally significant (based on
permutation tests):


```
## 
## ***VECTORS
## 
##                   PC1       PC2     r2    Pr(>r)    
## Bb_infected -0.129167  0.991620 0.0489 0.0009995 ***
## Bb_burden    0.007576  0.999970 0.0145 0.0999500 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## Permutation: free
## Number of permutations: 2000
```

And below are visualizations of the relationships between the individual PC
axes and our Bb variables:

![](F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/unnamed-chunk-2-1.png)<!-- -->![](F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/unnamed-chunk-2-2.png)<!-- -->![](F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/unnamed-chunk-2-3.png)<!-- -->![](F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/unnamed-chunk-2-4.png)<!-- -->

A multivariate regression of the Bb variables reveals that the significance
that we saw before is almost entirely due to the tolerance axis (PC2):


```
## Response Bb_infected :
## 
## Call:
## lm(formula = Bb_infected ~ PC1 + PC2, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.6733 -0.9726  0.3977  0.9742  1.1984 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -2.218e-16  5.504e-02   0.000    1.000    
## PC1         -7.692e-02  1.484e-01  -0.518    0.605    
## PC2          5.905e-01  1.484e-01   3.980 8.57e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.9783 on 313 degrees of freedom
## Multiple R-squared:  0.04895,	Adjusted R-squared:  0.04287 
## F-statistic: 8.054 on 2 and 313 DF,  p-value: 0.0003882
## 
## 
## Response Bb_burden :
## 
## Call:
## lm(formula = Bb_burden ~ PC1 + PC2, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.6012 -0.2693 -0.1973 -0.1290  9.5398 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)  
## (Intercept) 6.571e-17  5.602e-02   0.000   1.0000  
## PC1         2.453e-03  1.510e-01   0.016   0.9871  
## PC2         3.237e-01  1.510e-01   2.143   0.0329 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.9959 on 313 degrees of freedom
## Multiple R-squared:  0.01446,	Adjusted R-squared:  0.008166 
## F-statistic: 2.297 on 2 and 313 DF,  p-value: 0.1023
```

Note though, that the R-squared values are quite low.

### Transformations

Because of the distribution of these genetic trait values, transformations
are likely necessary such as the square-root transformation shown in the next
figure, but we will also group these values by quartile and decile later on.







Square-root or log transforming the resistance variables does help their 
distributions a bit, though they are still not fully Guassian. Unfortunately,
a lot of data is lost with log-transformations (due to zeros), especially 
among tolerance traits.

<div class="figure">
<img src="F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/Gene-hists-1.png" alt="Genetic traits histogram, before and after transformations"  />
<p class="caption">(\#fig:Gene-hists)Genetic traits histogram, before and after transformations</p>
</div>

## B.b. variable distributions

*Borrelia* burden data are heavily zero-inflated, unsurprisingly, and there
are about equal numbers of B.b. positive and negative individuals.

<div class="figure">
<img src="F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/Bb-hists-1.png" alt="B.b. variable histograms"  />
<p class="caption">(\#fig:Bb-hists)B.b. variable histograms</p>
</div>

Similar to the genetic traits, square-root transforming the burden data helps
with the skew. Log-transforming does an even better job, but can't handle the
zeros. This latter option is likely to be used with an infection hurdle model.

<div class="figure">
<img src="F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/Bb-trans-1.png" alt="distribution of B.b. burden before and after transformations"  />
<p class="caption">(\#fig:Bb-trans)distribution of B.b. burden before and after transformations</p>
</div>

## Infection status modeling

Because of the zero-inflation of the B.b. burden data, we will look at the 
factors affecting B.b. in two parts (i.e., hurdle model). The first part is
to look at the factors affecting the probability of being infected (binary) and
the second part is to model the burden given that an individual is infected
(i.e., zeros removed).

The figures below assess the relationships between the genetic traits and 
B.b. infection status. 

The first shows that, there aren't any strong patterns between the untransformed
genetic traits and B.b. status. It also reveals that there are some rather 
extreme outliers among the genetic traits:

<div class="figure">
<img src="F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/infect-raw-1.png" alt="Relationships between genetic traits and B.b. infection"  />
<p class="caption">(\#fig:infect-raw)Relationships between genetic traits and B.b. infection</p>
</div>

The next figure shows the relationship between the transformed variables and
infection status, with the top 1% of the genetic trait values removed. Note
that we log-transform the resistance traits and square-root-transform the
tolerance traits. It appears that the transformed resistance traits are still
not strongly associated with B.b. status (true for both transformations), but
the transformed tolerance traits do appear to be **positively** associated
with B.b. infection status. 

<div class="figure">
<img src="F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/infect-sqrt-1.png" alt="Relationships between sqrt-transformed genetic traits and B.b. infection"  />
<p class="caption">(\#fig:infect-sqrt)Relationships between sqrt-transformed genetic traits and B.b. infection</p>
</div>

If we break the genetic traits into quartiles, we observe a slightly stronger
patterns. Again, the strongest and clearest relationships are with the
tolerance traits. Note that the blue points are mean proportion of infected
individuals ± 95% confidence intervals for each quartile:

<div class="figure">
<img src="F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/infect-quarts-1.png" alt="Relationships between genetic traits quartiles and B.b. infection"  />
<p class="caption">(\#fig:infect-quarts)Relationships between genetic traits quartiles and B.b. infection</p>
</div>

And the same patterns exist when we look at deciles:

<div class="figure">
<img src="F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/infect-decs-1.png" alt="Relationships between genetic traits deciles and B.b. infection"  />
<p class="caption">(\#fig:infect-decs)Relationships between genetic traits deciles and B.b. infection</p>
</div>

### Statistical models

While most bivariate relationships are weak, the multivariate relationships
may be more interesting. This section conducts multiple regression modeling
of infection status from these 6 genetic traits.

Unfortunately, a generalized linear mixed effects (logistic regression) model
using raw responses reveals that shows no significant effects (and very small
effect sizes):


Table: (\#tab:infect-mod-raw)Infection logistic GLMM model, with raw genetic traits.

|            |   Coef|    SE|      z|     P|
|:-----------|------:|-----:|------:|-----:|
|(Intercept) | -0.022| 0.708| -0.031| 0.975|
|`IL-10`     | -0.002| 0.005| -0.514| 0.607|
|`IFN-y`     | -0.005| 0.003| -1.858| 0.063|
|`IL-6`      |  0.005| 0.005|  1.007| 0.314|
|`TGF-B`     |  0.002| 0.004|  0.366| 0.714|
|`GATA-3`    |  0.004| 0.006|  0.663| 0.507|

square-root transforming the predictor variables does not improve the 
significance:


Table: (\#tab:infect-mod-trans)Infection logistic GLMM model, with sqrt-transformed genetic traits.

|              |   Coef|    SE|      z|     P|
|:-------------|------:|-----:|------:|-----:|
|(Intercept)   |  0.193| 0.738|  0.262| 0.794|
|`sqrt_IL-10`  | -0.085| 0.115| -0.737| 0.461|
|`sqrt_IFN-y`  | -0.097| 0.052| -1.879| 0.060|
|`sqrt_IL-6`   |  0.123| 0.122|  1.005| 0.315|
|`sqrt_TGF-B`  |  0.067| 0.100|  0.674| 0.500|
|`sqrt_GATA-3` |  0.022| 0.117|  0.187| 0.852|

Nor does standardizing the predictor variables:


Table: (\#tab:infect-mod-scaled)Infection logistic GLMM model, with z-scaled genetic traits.

|                |   Coef|    SE|      z|     P|
|:---------------|------:|-----:|------:|-----:|
|(Intercept)     |  0.015| 0.695|  0.022| 0.983|
|`scaled_IL-10`  | -0.286| 0.555| -0.516| 0.606|
|`scaled_IFN-y`  | -0.493| 0.265| -1.864| 0.062|
|`scaled_IL-6`   |  0.599| 0.593|  1.009| 0.313|
|`scaled_TGF-B`  |  0.189| 0.520|  0.362| 0.717|
|`scaled_GATA-3` |  0.445| 0.677|  0.658| 0.510|

However, if we exclude the top 5% of all the genetic traits, then the two
tolerance variables (TGF-B and GATA-3) are significantly associated with
B.b. infection. Probability of infection *incraeses* with increasing TGF-B 
expression and *decreases* with GATA-3, after accounting for the effects of
the other variables:


Table: (\#tab:infect-mod-95pct)Infection logistic GLMM model, with extreme genetic traits removed.

|            |   Coef|    SE|      z|     P|
|:-----------|------:|-----:|------:|-----:|
|(Intercept) |  0.145| 0.598|  0.243| 0.808|
|`IL-10`     | -0.008| 0.008| -0.997| 0.319|
|`IFN-y`     | -0.005| 0.004| -1.155| 0.248|
|`IL-6`      |  0.009| 0.008|  1.108| 0.268|
|`TGF-B`     |  0.060| 0.029|  2.056| 0.040|
|`GATA-3`    | -0.111| 0.051| -2.183| 0.029|

The same is likely true for using quartiles/deciles, but I haven't fit those
models yet.



## B.b. burden

TBD: Because the infection status did not seem to be associated with any of the
genetic variables, I am going to hold off on running the second part of the 
analysis ($P(\text{burden}|\text{infected})$).

## Weather

One important component of the SEMs will be a measure of climate. PCA allows
us to take a series of climatic variables and simplify them to a single
index variable that encompasses them. Here we include both the mean and 
coefficient of variation for temperature, precipitation, and relative
humidity. We will also perform the PCA at three different scales: the site 
scale (average climate across 8 years), the annual scale (average climate
within a single year at a site), and the month scale (average climate within
a single month at a site).







Here is a quick look at the site-scale weather PCA, where PC1 explains
65% of the total variation and PC2 explains an additional 27%:

![](F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/site-climate-PCA-1.png)<!-- -->

```
## Importance of components:
##                          PC1    PC2     PC3    PC4      PC5      PC6
## Eigenvalue            3.9152 1.6258 0.24984 0.1440 0.041644 0.023486
## Proportion Explained  0.6525 0.2710 0.04164 0.0240 0.006941 0.003914
## Cumulative Proportion 0.6525 0.9235 0.96514 0.9891 0.996086 1.000000
```




Here is a breakdown of PC1 across the sites and the loadings of the different
climate variables:

![](F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

|variable                |    PC1|
|:-----------------------|------:|
|site_CV_monthly_temp    |  0.993|
|site_avg_monthly_RH     |  0.785|
|site_CV_monthly_precip  |  0.602|
|site_avg_monthly_precip | -0.669|
|site_CV_monthly_RH      | -0.933|
|site_avg_monthly_temp   | -0.973|

Because this site-scale PCA seems to place relative humidity and precipitation on
opposite sides of the PC1 axis, so I'd like to visualize the relationship of 
these two variables with temperature. From this, we can see that at sites with
the highest lowest average temperatures, average humidity and precipitation are 
less linked than at the sites with more middling average temperatures:

![](F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Here is the annual-scale PCA, where PC1 explains 48% of the variation and 
PC2 explains an additional 22%. 

![](F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/year-climate-PCA-1.png)<!-- -->

```
## Importance of components:
##                          PC1    PC2    PC3    PC4     PC5     PC6
## Eigenvalue            2.8604 1.3081 0.7811 0.6102 0.36203 0.07814
## Proportion Explained  0.4767 0.2180 0.1302 0.1017 0.06034 0.01302
## Cumulative Proportion 0.4767 0.6948 0.8249 0.9266 0.98698 1.00000
```

The monthly climate PCA (not visualized) further reduces the explanatory power of the PC axes
with PC1 explaining only 30% of the climate variation and PC2 explaining an
additional 24%:


```
## Importance of components:
##                          PC1    PC2    PC3    PC4    PC5     PC6
## Eigenvalue            1.8199 1.4637 1.0022 0.9744 0.5334 0.20644
## Proportion Explained  0.3033 0.2439 0.1670 0.1624 0.0889 0.03441
## Cumulative Proportion 0.3033 0.5473 0.7143 0.8767 0.9656 1.00000
```



## Resistance/Tolerance PCA

Next, we will try to reduce the resistance and tolerance genetic expression 
variables into PCA axes, as we did with weather. In this case, we will fit
separate PCAs for resistance (4 variables) and tolerance (2 variables) and will
use the first PC axis as the index.



The resistance PC1 explains 77% of the variation among the (log-transformed) 
resistance variables, and is well correlated with those variables 
(only one shown):

![](F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/resistance_PCA-1.png)<!-- -->

```
##                         PC1   PC2   PC3   PC4
## Eigenvalue            4.996 1.248 0.190 0.047
## Proportion Explained  0.771 0.193 0.029 0.007
## Cumulative Proportion 0.771 0.964 0.993 1.000
```

The tolerance PC1 explains 97% of the variation in the two (log-transformed) 
tolerance variables:

![](F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/tolerance_PCA-1.png)<!-- -->

```
##                         PC1   PC2
## Eigenvalue            5.728 0.193
## Proportion Explained  0.967 0.033
## Cumulative Proportion 0.967 1.000
```



## SEMs:

In this section, we will look at SEMs that involve resistance and tolerance. 
Specifically, we are interested in resistance and tolerance as *latent* 
variables for tick attachment and *Borrelia* infection. 

### Tick attachment

First, we'll build a model whose ultimate response variable is tick attachment.
These models will include resistance and tolerance as predictors of tick 
attachment as well as factors that might predict resistance and tolerance. We
will also try to incorporate mouse behavior (e.g., capture time) into these
models.



#### model 1

The first model tries to replicate the model that Alli built, but adding an
additional year of data. This model is not identical to the one Alli fit:
rather than fitting the sub-models (lmer and glmer) to the *individual* 
scale (by summarizing variables across recaptures), we use fit to the individual
*observation* scale (including individual ID as a random effect). This prevents
the need to summarize and maximizes the available information (and power).



The results from this model indicate that there are some important paths missing
from the model (the effect of sexual maturity on capture timing and the 
correlation between tolerance and resistance). 

The results are somewhat interesting:

* The probability (results on logit-link scale) of having a tick attached is associated with site 
level climate (P=0.003), capture time (P=0.4), and there is a significant
interaction between sex and sexual maturity (P=0.4). Notably the main effects
for sex and sexual maturity are not significant.

* Capture time is affected by both sex and weight of the individual

* Resistance is differs by year, sex, and weight

* Tolerance differs only by year



```
## 
## Structural Equation Model of fsem1 
## 
## Call:
##   cap_prop_night ~ sex_male + weight
##   ticks_attatched ~ siteClimatePC1 + cap_prop_night + sex_male * sex_mature
##   res_PC1 ~ siteClimatePC1 * year + sex_mature + weight + sex_male
##   tol_PC1 ~ weight + year + sex_male * cap_prop_night + ticks_attatched * sex_mature
## 
##     AIC
##  3373.749
## 
## ---
## Tests of directed separation:
## 
##                          Independ.Claim Test.Type        DF Crit.Value P.Value    
##          ticks_attatched ~ weight + ...      coef 2251.0000    -1.6031  0.1089    
##   cap_prop_night ~ siteClimatePC1 + ...      coef    5.9655     0.9522  0.3671    
##          tol_PC1 ~ siteClimatePC1 + ...      coef    5.8088     0.3878  0.5571    
##       cap_prop_night ~ sex_mature + ...      coef 2604.6789     0.7693  0.3805    
##             cap_prop_night ~ year + ...      coef 1239.0196     3.9640  0.0467   *
##            ticks_attatched ~ year + ...      coef 2304.0000    -1.6118  0.1070    
##          res_PC1 ~ cap_prop_night + ...      coef  494.6890     1.4236  0.2334    
##         res_PC1 ~ ticks_attatched + ...      coef  195.0386     1.6811  0.1963    
##                 tol_PC1 ~ res_PC1 + ...      coef  391.7463    20.1552  0.0000 ***
## 
## --
## Global goodness-of-fit:
## 
## Chi-Squared = 152.663 with P-value = 0 and on 11 degrees of freedom
## Fisher's C = 49.454 with P-value = 0 and on 18 degrees of freedom
## 
## ---
## Coefficients:
## 
##          Response                  Predictor Estimate Std.Error        DF Crit.Value P.Value Std.Estimate    
##    cap_prop_night                   sex_male   0.0372    0.0105  984.0693    12.5681  0.0004       0.0716 ***
##    cap_prop_night                     weight   0.0064    0.0011 1861.5165    35.0404  0.0000       0.1143 ***
##   ticks_attatched             siteClimatePC1  -1.0012    0.3348 2304.0000    -2.9900  0.0028      -0.4104  **
##   ticks_attatched             cap_prop_night  -0.4329    0.2126 2304.0000    -2.0365  0.0417      -0.0499   *
##   ticks_attatched                   sex_male   0.2832    0.2042 2304.0000     1.3867  0.1655       0.0628    
##   ticks_attatched                 sex_mature   0.0143    0.1861 2304.0000     0.0769  0.9387       0.0030    
##   ticks_attatched        sex_male:sex_mature   0.5033    0.2434 2304.0000     2.0677  0.0387       0.0966   *
##           res_PC1             siteClimatePC1 -68.7177   38.5405  521.5760     3.1339  0.0773    -200.8454    
##           res_PC1                       year  -0.0590    0.0181  512.2496    10.3354  0.0014      -0.5980  **
##           res_PC1                 sex_mature  -0.0150    0.0290  506.2120     0.2607  0.6098      -0.0226    
##           res_PC1                     weight  -0.0092    0.0029  517.1024     9.7168  0.0019      -0.1355  **
##           res_PC1                   sex_male  -0.0581    0.0265  473.0112     4.6146  0.0322      -0.0919   *
##           res_PC1        siteClimatePC1:year   0.0339    0.0191  521.5717     3.1264  0.0776     200.1919    
##           tol_PC1                     weight  -0.0002    0.0034  395.6164     0.0040  0.9495      -0.0032    
##           tol_PC1                       year  -0.1235    0.0204  390.2400    35.2996  0.0000      -1.2636 ***
##           tol_PC1                   sex_male  -0.0308    0.0541  381.5276     0.3138  0.5757      -0.0492    
##           tol_PC1             cap_prop_night  -0.0862    0.0804  351.1393     1.0890  0.2974      -0.0716    
##           tol_PC1            ticks_attatched   0.0971    0.0472  204.8696     3.7699  0.0536       0.1482    
##           tol_PC1                 sex_mature   0.0411    0.0415  364.4865     0.9366  0.3338       0.0623    
##           tol_PC1    sex_male:cap_prop_night   0.0817    0.1108  383.7265     0.5275  0.4681       0.1285    
##           tol_PC1 ticks_attatched:sex_mature   0.0056    0.0603  339.2850     0.0082  0.9279       0.0087    
## 
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
## 
## ---
## Individual R-squared:
## 
##          Response      method Marginal Conditional
##    cap_prop_night        none     0.02        0.15
##   ticks_attatched theoretical     0.17        0.43
##           res_PC1        none     0.08        0.86
##           tol_PC1        none     0.11        0.81
```

Below is a visualization of the sex-maturity interaction:

![](F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

Next, we update the model to include the suggested missing components. 
Non-significant paths have not yet been removed.


```
## 
## Structural Equation Model of fsem1.corr 
## 
## Call:
##   cap_prop_night ~ sex_male + weight + year
##   ticks_attatched ~ siteClimatePC1 + cap_prop_night + sex_male * sex_mature
##   res_PC1 ~ siteClimatePC1 * year + sex_mature + weight + sex_male
##   tol_PC1 ~ weight + year + sex_male + cap_prop_night + ticks_attatched + sex_mature + res_PC1 + sex_male:cap_prop_night + ticks_attatched:sex_mature
## 
##     AIC
##  3365.344
## 
## ---
## Tests of directed separation:
## 
##                          Independ.Claim Test.Type        DF Crit.Value P.Value 
##          ticks_attatched ~ weight + ...      coef 2251.0000    -1.6031  0.1089 
##            ticks_attatched ~ year + ...      coef 2304.0000    -1.6118  0.1070 
##   cap_prop_night ~ siteClimatePC1 + ...      coef    5.9598     0.9157  0.3758 
##          tol_PC1 ~ siteClimatePC1 + ...      coef    5.9548     0.0216  0.8880 
##       cap_prop_night ~ sex_mature + ...      coef 2637.0750     0.3475  0.5556 
##          res_PC1 ~ cap_prop_night + ...      coef  494.6896     1.4236  0.2334 
##         res_PC1 ~ ticks_attatched + ...      coef  195.0386     1.6811  0.1963 
## 
## --
## Global goodness-of-fit:
## 
## Chi-Squared = 132.433 with P-value = 0 and on 8 degrees of freedom
## Fisher's C = 18.441 with P-value = 0.187 and on 14 degrees of freedom
## 
## ---
## Coefficients:
## 
##          Response                  Predictor Estimate Std.Error        DF Crit.Value P.Value Std.Estimate    
##    cap_prop_night                   sex_male   0.0363    0.0105  980.8610    11.9407  0.0006       0.0699 ***
##    cap_prop_night                     weight   0.0063    0.0011 1869.4823    33.9652  0.0000       0.1126 ***
##    cap_prop_night                       year  -0.0129    0.0065 1239.0194     3.9640  0.0467      -0.1590   *
##   ticks_attatched             siteClimatePC1  -1.0012    0.3348 2304.0000    -2.9900  0.0028      -0.4104  **
##   ticks_attatched             cap_prop_night  -0.4329    0.2126 2304.0000    -2.0365  0.0417      -0.0499   *
##   ticks_attatched                   sex_male   0.2832    0.2042 2304.0000     1.3867  0.1655       0.0628    
##   ticks_attatched                 sex_mature   0.0143    0.1861 2304.0000     0.0769  0.9387       0.0030    
##   ticks_attatched        sex_male:sex_mature   0.5033    0.2434 2304.0000     2.0677  0.0387       0.0966   *
##           res_PC1             siteClimatePC1 -68.7177   38.5405  521.5760     3.1339  0.0773    -200.8454    
##           res_PC1                       year  -0.0590    0.0181  512.2496    10.3354  0.0014      -0.5980  **
##           res_PC1                 sex_mature  -0.0150    0.0290  506.2120     0.2607  0.6098      -0.0226    
##           res_PC1                     weight  -0.0092    0.0029  517.1024     9.7168  0.0019      -0.1355  **
##           res_PC1                   sex_male  -0.0581    0.0265  473.0112     4.6146  0.0322      -0.0919   *
##           res_PC1        siteClimatePC1:year   0.0339    0.0191  521.5717     3.1264  0.0776     200.1919    
##           tol_PC1                     weight   0.0027    0.0034  394.3824     0.6403  0.4241       0.0405    
##           tol_PC1                       year  -0.1060    0.0201  373.9837    26.4183  0.0000      -1.0845 ***
##           tol_PC1                   sex_male   0.0021    0.0534  393.4754     0.0016  0.9684       0.0034    
##           tol_PC1             cap_prop_night  -0.0576    0.0801  392.8636     0.5134  0.4741      -0.0479    
##           tol_PC1            ticks_attatched   0.0906    0.0458  149.2522     3.2970  0.0714       0.1383    
##           tol_PC1                 sex_mature   0.0538    0.0408  386.8151     1.6806  0.1956       0.0816    
##           tol_PC1                    res_PC1   0.2360    0.0519  391.3996    20.0601  0.0000       0.2384 ***
##           tol_PC1    sex_male:cap_prop_night   0.0489    0.1094  391.7449     0.1992  0.6556       0.0769    
##           tol_PC1 ticks_attatched:sex_mature  -0.0056    0.0591  333.6815     0.0085  0.9267      -0.0087    
## 
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
## 
## ---
## Individual R-squared:
## 
##          Response      method Marginal Conditional
##    cap_prop_night        none     0.02        0.16
##   ticks_attatched theoretical     0.17        0.43
##           res_PC1        none     0.08        0.86
##           tol_PC1        none     0.14        0.85
```

Here's a visualization of the model (significant paths only):

<img src="F:/projects/NEON-Peromyscus/infection-modeling/graphical-models/resistance-tolerance_model1.drawio.png" width="361" />

There are a few problems with this model, from my perspective:

* There are some missing links that seem obvious (i.e., the effect of climate
on behavior).

* Year is treated as a numeric variable, which expects a constant trend through
time. This assumption is not reasonable. Better approaches would be to either 
1) include year as a random effect (note the lack of an interaction with year)
or 2) conduct a group comparison, whereby the model is parameter differently
for each year and testing which paths differ among years.

* The resistance and tolerance variables have a directed relationship. It is 
more appropriate to specify that these variables are adirectionally correlated.
I tried fitting the model with this specification multiple times and it would
not successfully fit.

* There are no significant links between ticks and resistance or tolerance, 
rather it is a model of the common factors that affect ticks, resistance, and 
tolerance. This is not completely uninteresting, but it does seem like there 
should be a direct link. 



#### model 2

[**UNDER CONSTRUCTION**]




```
## 
## Structural Equation Model of res_tol_sem 
## 
## Call:
##   cap_prop_night ~ siteClimatePC1 + sex_male + weight
##   ticks_attatched ~ siteClimatePC1 + cap_prop_night + sex_male * sex_mature
##   res_PC1 ~ siteClimatePC1 + sex_mature + weight + sex_male
##   tol_PC1 ~ res_PC1 + weight + sex_male * cap_prop_night + ticks_attatched * sex_mature
## 
##     AIC
##  812.565
## 
## ---
## Tests of directed separation:
## 
##                      Independ.Claim Test.Type       DF Crit.Value P.Value 
##      tol_PC1 ~ siteClimatePC1 + ...      coef   6.5003     0.0081  0.9312 
##      ticks_attatched ~ weight + ...      coef 342.0000    -0.2914  0.7707 
##   cap_prop_night ~ sex_mature + ...      coef 313.7839     1.4353  0.2318 
##      res_PC1 ~ cap_prop_night + ...      coef 330.1381     0.0053  0.9421 
##     res_PC1 ~ ticks_attatched + ...      coef 163.7735     0.4789  0.4899 
## 
## --
## Global goodness-of-fit:
## 
## Chi-Squared = NA with P-value = NA and on 6 degrees of freedom
## Fisher's C = 5.134 with P-value = 0.882 and on 10 degrees of freedom
## 
## ---
## Coefficients:
## 
##          Response                  Predictor Estimate Std.Error       DF Crit.Value P.Value Std.Estimate    
##    cap_prop_night             siteClimatePC1  -0.0440    0.0264   8.1900     2.5720  0.1466      -0.1230    
##    cap_prop_night                   sex_male   0.0423    0.0277 271.3484     2.1412  0.1445       0.0797    
##    cap_prop_night                     weight   0.0061    0.0030 302.2910     3.8565  0.0505       0.1078    
##   ticks_attatched             siteClimatePC1  -0.8826    0.3945 342.0000    -2.2374  0.0253      -0.3205   *
##   ticks_attatched             cap_prop_night  -0.8429    0.4634 342.0000    -1.8188  0.0689      -0.1094    
##   ticks_attatched                   sex_male   0.5210    0.4336 342.0000     1.2015  0.2295       0.1274    
##   ticks_attatched                 sex_mature   0.0703    0.4289 342.0000     0.1640  0.8697       0.0170    
##   ticks_attatched        sex_male:sex_mature   0.0722    0.5497 342.0000     0.1313  0.8955       0.0156    
##           res_PC1             siteClimatePC1  -0.0865    0.0459   6.2812     3.5267  0.1073      -0.2147    
##           res_PC1                 sex_mature  -0.0251    0.0325 304.8591     0.5555  0.4567      -0.0413    
##           res_PC1                     weight  -0.0109    0.0033 319.5091    10.4247  0.0014      -0.1721  **
##           res_PC1                   sex_male  -0.0857    0.0292 231.5254     7.7690  0.0058      -0.1432  **
##           tol_PC1                    res_PC1   0.1984    0.0560 302.7135    12.0625  0.0006       0.1838 ***
##           tol_PC1                     weight  -0.0002    0.0035 326.8091     0.0019  0.9653      -0.0023    
##           tol_PC1                   sex_male  -0.0186    0.0559 324.6305     0.1089  0.7416      -0.0287    
##           tol_PC1             cap_prop_night  -0.0511    0.0826 325.8398     0.3795  0.5383      -0.0420    
##           tol_PC1            ticks_attatched   0.0811    0.0487 281.7075     2.5599  0.1107       0.1253    
##           tol_PC1                 sex_mature   0.0804    0.0446 327.7695     3.1966  0.0747       0.1228    
##           tol_PC1    sex_male:cap_prop_night   0.0791    0.1134 325.1935     0.4840  0.4871       0.0693    
##           tol_PC1 ticks_attatched:sex_mature  -0.0038    0.0615 323.7548     0.0036  0.9521      -0.0051    
## 
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
## 
## ---
## Individual R-squared:
## 
##          Response      method Marginal Conditional
##    cap_prop_night        none     0.03        0.87
##   ticks_attatched theoretical     0.11        0.28
##           res_PC1        none     0.10        0.93
##           tol_PC1        none     0.05        0.65
```


|Response        |Predictor                  | Estimate| Std.Error|       DF| Crit.Value| P.Value| Std.Estimate|    |
|:---------------|:--------------------------|--------:|---------:|--------:|----------:|-------:|------------:|:---|
|cap_prop_night  |siteClimatePC1             |  -0.0440|    0.0264|   8.1900|     2.5720|  0.1466|      -0.1230|    |
|cap_prop_night  |sex_male                   |   0.0423|    0.0277| 271.3484|     2.1412|  0.1445|       0.0797|    |
|cap_prop_night  |weight                     |   0.0061|    0.0030| 302.2910|     3.8565|  0.0505|       0.1078|    |
|ticks_attatched |siteClimatePC1             |  -0.8826|    0.3945| 342.0000|    -2.2374|  0.0253|      -0.3205|*   |
|ticks_attatched |cap_prop_night             |  -0.8429|    0.4634| 342.0000|    -1.8188|  0.0689|      -0.1094|    |
|ticks_attatched |sex_male                   |   0.5210|    0.4336| 342.0000|     1.2015|  0.2295|       0.1274|    |
|ticks_attatched |sex_mature                 |   0.0703|    0.4289| 342.0000|     0.1640|  0.8697|       0.0170|    |
|ticks_attatched |sex_male:sex_mature        |   0.0722|    0.5497| 342.0000|     0.1313|  0.8955|       0.0156|    |
|res_PC1         |siteClimatePC1             |  -0.0865|    0.0459|   6.2812|     3.5267|  0.1073|      -0.2147|    |
|res_PC1         |sex_mature                 |  -0.0251|    0.0325| 304.8591|     0.5555|  0.4567|      -0.0413|    |
|res_PC1         |weight                     |  -0.0109|    0.0033| 319.5091|    10.4247|  0.0014|      -0.1721|**  |
|res_PC1         |sex_male                   |  -0.0857|    0.0292| 231.5254|     7.7690|  0.0058|      -0.1432|**  |
|tol_PC1         |res_PC1                    |   0.1984|    0.0560| 302.7135|    12.0625|  0.0006|       0.1838|*** |
|tol_PC1         |weight                     |  -0.0002|    0.0035| 326.8091|     0.0019|  0.9653|      -0.0023|    |
|tol_PC1         |sex_male                   |  -0.0186|    0.0559| 324.6305|     0.1089|  0.7416|      -0.0287|    |
|tol_PC1         |cap_prop_night             |  -0.0511|    0.0826| 325.8398|     0.3795|  0.5383|      -0.0420|    |
|tol_PC1         |ticks_attatched            |   0.0811|    0.0487| 281.7075|     2.5599|  0.1107|       0.1253|    |
|tol_PC1         |sex_mature                 |   0.0804|    0.0446| 327.7695|     3.1966|  0.0747|       0.1228|    |
|tol_PC1         |sex_male:cap_prop_night    |   0.0791|    0.1134| 325.1935|     0.4840|  0.4871|       0.0693|    |
|tol_PC1         |ticks_attatched:sex_mature |  -0.0038|    0.0615| 323.7548|     0.0036|  0.9521|      -0.0051|    |

```
## 
## Structural Equation Model of alt_psem 
## 
## Call:
##   cap_prop_night ~ res_PC1 + tol_PC1 + siteClimatePC1 + sex_male + sex_mature + weight
##   ticks_attatched ~ res_PC1 + tol_PC1 + siteClimatePC1 + sex_mature + sex_male
##   res_PC1 ~ siteClimatePC1 + sex_mature + weight + sex_male
##   tol_PC1 ~ weight + sex_mature + sex_male
##   res_PC1 ~~ tol_PC1
## 
##     AIC
##  813.893
## 
## ---
## Tests of directed separation:
## 
##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
##           tol_PC1 ~ siteClimatePC1 + ...      coef   6.3875     0.4811  0.5124 
##           ticks_attatched ~ weight + ...      coef 342.0000    -0.4946  0.6209 
##   ticks_attatched ~ cap_prop_night + ...      coef 342.0000    -1.6211  0.1050 
## 
## --
## Global goodness-of-fit:
## 
## Chi-Squared = NA with P-value = NA and on 3 degrees of freedom
## Fisher's C = 6.798 with P-value = 0.34 and on 6 degrees of freedom
## 
## ---
## Coefficients:
## 
##          Response      Predictor Estimate Std.Error       DF Crit.Value P.Value Std.Estimate   
##    cap_prop_night        res_PC1   0.0019    0.0532 316.1646     0.0012  0.9723       0.0022   
##    cap_prop_night        tol_PC1  -0.0228    0.0481 147.8033     0.1884  0.6649      -0.0277   
##    cap_prop_night siteClimatePC1  -0.0418    0.0272   8.0995     2.1959  0.1762      -0.1169   
##    cap_prop_night       sex_male   0.0512    0.0291 292.8686     2.8620  0.0918       0.0963   
##    cap_prop_night     sex_mature   0.0406    0.0319 314.1238     1.4978  0.2219       0.0754   
##    cap_prop_night         weight   0.0045    0.0033 326.5263     1.8108  0.1794       0.0801   
##   ticks_attatched        res_PC1  -0.0916    0.4447 342.0000    -0.2059  0.8368      -0.0131   
##   ticks_attatched        tol_PC1   1.3746    0.4314 342.0000     3.1861  0.0014       0.2125 **
##   ticks_attatched siteClimatePC1  -0.8744     0.458 342.0000    -1.9094  0.0562      -0.3108   
##   ticks_attatched     sex_mature  -0.0330     0.256 342.0000    -0.1288  0.8975      -0.0078   
##   ticks_attatched       sex_male   0.5068    0.2515 342.0000     2.0152  0.0439       0.1213  *
##           res_PC1 siteClimatePC1  -0.0865    0.0459   6.2812     3.5267  0.1073      -0.2147   
##           res_PC1     sex_mature  -0.0251    0.0325 304.8591     0.5555  0.4567      -0.0413   
##           res_PC1         weight  -0.0109    0.0033 319.5091    10.4247  0.0014      -0.1721 **
##           res_PC1       sex_male  -0.0857    0.0292 231.5254     7.7690  0.0058      -0.1432 **
##           tol_PC1         weight  -0.0024    0.0035 331.3738     0.4610  0.4976      -0.0354   
##           tol_PC1     sex_mature   0.0797    0.0347 332.1111     5.1199  0.0243       0.1217  *
##           tol_PC1       sex_male   0.0059    0.0314 315.3005     0.0338  0.8543       0.0092   
##         ~~res_PC1      ~~tol_PC1   0.1473         - 342.0000     2.7417  0.0032       0.1473 **
## 
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
## 
## ---
## Individual R-squared:
## 
##          Response      method Marginal Conditional
##    cap_prop_night        none     0.03        0.86
##   ticks_attatched theoretical     0.12        0.33
##           res_PC1        none     0.10        0.93
##           tol_PC1        none     0.01        0.83
```


|Response        |Predictor      | Estimate|Std.Error |     DF| Crit.Value| P.Value| Std.Estimate|   |
|:---------------|:--------------|--------:|:---------|------:|----------:|-------:|------------:|:--|
|cap_prop_night  |res_PC1        |     0.00|0.0532    | 316.16|       0.00|    0.97|         0.00|   |
|cap_prop_night  |tol_PC1        |    -0.02|0.0481    | 147.80|       0.19|    0.66|        -0.03|   |
|cap_prop_night  |siteClimatePC1 |    -0.04|0.0272    |   8.10|       2.20|    0.18|        -0.12|   |
|cap_prop_night  |sex_male       |     0.05|0.0291    | 292.87|       2.86|    0.09|         0.10|   |
|cap_prop_night  |sex_mature     |     0.04|0.0319    | 314.12|       1.50|    0.22|         0.08|   |
|cap_prop_night  |weight         |     0.00|0.0033    | 326.53|       1.81|    0.18|         0.08|   |
|ticks_attatched |res_PC1        |    -0.09|0.4447    | 342.00|      -0.21|    0.84|        -0.01|   |
|ticks_attatched |tol_PC1        |     1.37|0.4314    | 342.00|       3.19|    0.00|         0.21|** |
|ticks_attatched |siteClimatePC1 |    -0.87|0.458     | 342.00|      -1.91|    0.06|        -0.31|   |
|ticks_attatched |sex_mature     |    -0.03|0.256     | 342.00|      -0.13|    0.90|        -0.01|   |
|ticks_attatched |sex_male       |     0.51|0.2515    | 342.00|       2.02|    0.04|         0.12|*  |
|res_PC1         |siteClimatePC1 |    -0.09|0.0459    |   6.28|       3.53|    0.11|        -0.21|   |
|res_PC1         |sex_mature     |    -0.03|0.0325    | 304.86|       0.56|    0.46|        -0.04|   |
|res_PC1         |weight         |    -0.01|0.0033    | 319.51|      10.42|    0.00|        -0.17|** |
|res_PC1         |sex_male       |    -0.09|0.0292    | 231.53|       7.77|    0.01|        -0.14|** |
|tol_PC1         |weight         |     0.00|0.0035    | 331.37|       0.46|    0.50|        -0.04|   |
|tol_PC1         |sex_mature     |     0.08|0.0347    | 332.11|       5.12|    0.02|         0.12|*  |
|tol_PC1         |sex_male       |     0.01|0.0314    | 315.30|       0.03|    0.85|         0.01|   |
|~~res_PC1       |~~tol_PC1      |     0.15|-         | 342.00|       2.74|    0.00|         0.15|** |

```
##                           Independ.Claim Test.Type       DF Crit.Value P.Value 
## 1         tol_PC1 ~ siteClimatePC1 + ...      coef   6.3875     0.4811  0.5124 
## 2         ticks_attatched ~ weight + ...      coef 342.0000    -0.4946  0.6209 
## 3 ticks_attatched ~ cap_prop_night + ...      coef 342.0000    -1.6211  0.1050
```

## Question: What coverage do we have for recaptures within a capture event?

In total there are nearly 8000 individual *Peromyscus* individuals that were
captured multiple times within a trapping event:


```
## [1] 22092
```

![](F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

```
## [1] 7948
```

And of those individuals that were recaptured, over 1000 were also captured (and
recaptured) in traps with ibuttons, allowing for estimation of capture time:

![](F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

```
## [1] 1085
```

Below is a table containing the counts of *Peromyscus* recaptures, for which 
we have capture time estimates and multiple captures within a session, that 
breaks down whether or not an individual was recaptured with the same tick 
status (`changed_status`) as it had the first time it was captured 
(`initial_status`) for each tick life stage (`tickStage`). Note that 
`tickStage == "any"` refers to any tick regardless of life stage. Overall, 
relatively few individuals were recaptured with a different tick status
than their first capture:


Table: (\#tab:unnamed-chunk-24)Recaptured Peromyscus tick attachment status counts, by tick lifestage

|tickStage |initial_status |changed_status |   n|
|:---------|:--------------|:--------------|---:|
|larva     |FALSE          |FALSE          | 458|
|larva     |FALSE          |TRUE           |  94|
|larva     |TRUE           |FALSE          | 256|
|larva     |TRUE           |TRUE           |  89|
|nymph     |FALSE          |FALSE          | 798|
|nymph     |FALSE          |TRUE           |  35|
|nymph     |TRUE           |FALSE          |  20|
|nymph     |TRUE           |TRUE           |  43|
|adult     |FALSE          |FALSE          | 894|
|adult     |FALSE          |TRUE           |   2|
|any       |FALSE          |FALSE          | 420|
|any       |FALSE          |TRUE           |  89|
|any       |TRUE           |FALSE          | 294|
|any       |TRUE           |TRUE           |  94|

![](F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

There does appear to be be a trend whereby individuals that are recaptured
later tend to have a higher probability of being observed with a tick:

![](F:/projects/NEON-Peromyscus/docs/resistence-tolerance_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

But that relationship is not significant 
(accounting for year, site, event, and individual as random effects). The only
factor that strongly predicts whether a recapture has a tick is if the 
initial capture had a tick:


``` r
library(lmerTest)  

tick_time_fm <- glmer(
  attachStatus ~ cap_time_change * initial_status + 
    (1|year) + (1|siteID) + (1|eventID) + (1|tagID),
  data = cap_tick_tab %>% 
    filter(cap_num > 1, tickStage == "any") %>% 
    mutate(
      attachStatus = as.numeric(attachStatus),
      initial_status = as.numeric(initial_status)
    )
)  

# fixed effects
summary(tick_time_fm)$coefficients
```

```
##                                   Estimate Std. Error    t value
## (Intercept)                     0.23379777 0.05661918  4.1293034
## cap_time_change                -0.06106975 0.04911302 -1.2434533
## initial_status                  0.41365525 0.02981975 13.8718541
## cap_time_change:initial_status  0.07067464 0.07943176  0.8897528
```

``` r
# random effects
VarCorr(tick_time_fm)
```

```
##  Groups   Name        Std.Dev.
##  tagID    (Intercept) 0.088320
##  eventID  (Intercept) 0.128300
##  siteID   (Intercept) 0.130992
##  year     (Intercept) 0.034498
##  Residual             0.351196
```

``` r
# Type II ANOVA
car::Anova(tick_time_fm)
```

```
## Analysis of Deviance Table (Type II Wald chisquare tests)
## 
## Response: attachStatus
##                                   Chisq Df Pr(>Chisq)    
## cap_time_change                  0.7849  1     0.3756    
## initial_status                 191.9700  1     <2e-16 ***
## cap_time_change:initial_status   0.7917  1     0.3736    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```
And if we only look at those that started without ticks, the effect is still
nonsignificant:


``` r
upd <- update(
  tick_time_fm, . ~ . - initial_status - cap_time_change:initial_status,
  data = cap_tick_tab %>% 
    filter(cap_num > 1, tickStage == "any", !initial_status) %>% 
    mutate(
      attachStatus = as.numeric(attachStatus),
      initial_status = as.numeric(initial_status)
    ) 
  )

car::Anova(upd)
```

```
## Analysis of Deviance Table (Type II Wald chisquare tests)
## 
## Response: attachStatus
##                  Chisq Df Pr(>Chisq)
## cap_time_change 1.9019  1     0.1679
```

**Is tick status (and the change thereof) associated with change in relative
capture timing?**
