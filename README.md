# timesaveR

<!-- badges: start -->
[![R-CMD-check](https://github.com/LukasWallrich/timesaveR/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/LukasWallrich/timesaveR/actions)
<!-- badges: end -->


This package aims to speed up research projects in social psychology (and related fields). For that, it primarily includes some functions that help lay the groundwork and others that facilitate the reporting of results.

Among others, the package can help with the following:

- **Creating scales**, including reverse-coding and reporting their internal consistency,
- **Creating correlation tables** in APA style, including descriptive statistics and confidence intervals, and an option to use survey weights, multiple imputation or full-information maximum-likelihood estimation
- **Creating regression tables** comparing standardised and non-standardised regression coefficients, and comparing the F-change between two nested models
- **Format numbers for statistical reporting**, including rounding with trailing zeros, as *p*-values, as confidence intervals



## Why use this package?

There are many packages that support data analysis and reporting. For instance, the `psych` package offers functions to create scales, while the `modelsummary` package offers options to create customisable tables in a wide variety of output format. They power many of the functions offered here 'under the hood.'

`apa` and `papaja` are two packages that directly support the reporting of results in APA style - they can complement this package well. However, none of the existing offered quite what we needed. This package

- takes an end-to-end view of the data analysis process, streamlining time-consuming steps at various stages
- offers analysis templates that make it easy to get started, particularly for R novices,
- prioritises publication-readiness and good reporting practices over customisability in creating tables and charts
- integrates with the `tidyverse` by supporting tidy evaluation and returning tibbles where possible 

# Installation

You can install timesaveR from GitHub with the command below. If you do not have the `remotes`-package installed, run `install.packages("remote")` first.

```r
remotes::install_github('lukaswallrich/timesaveR')
```

# Get started

There are many functions in the package, and we will create vignettes detailing various use cases. However, the following can give you an initial flavor. The examples use data from the European Social Survey Wave 7 (2014). Here, I ignore survey weights. However, the package offers similar functions for analysing weighted survey data, which are explained in the [survey data vignette](doc/survey_functions.html).

## Load the package

(I also load the `tidyverse` since that is the recommended usage - of course, there are base R alternatives for most of the steps.)

```r
library(tidyverse)
library(timesaverR)

```

## Create scales

Let's create scales for health behaviours and depressive symptoms, each including some reverse coding.

```r
scales <- list(
  depression = c("fltdpr", "flteeff", "slprl", "wrhpp", "fltlnl", 
                 "enjlf", "fltsd", "cldgng"),
  healthy_eating = c("etfruit", "eatveg")
  )
  
scales_reverse <- list(
  depression = c("wrhpp", "enjlf"),
  healthy_eating = c("etfruit", "eatveg")
)

scales <- make_scales(ess_health, items = scales, reversed = scales_reverse)

#Check descriptives, including reliability
scales$descriptives

#Add scale scores to dataset
ess_health <- cbind(ess_health, scales$scores)
```
## Report correlations and descriptive statistics

Next, we are often interested in descriptive statistics, variable distributions and correlations.

```r
ess_health %>% select(agea, health, depression, healthy_eating) %>% cor_matrix() %>% apa_cor_table()

#It is often helpful to rename variables in this step
#Use get_rename_tribbles to get most of this code
var_renames <- tibble::tribble(
    ~old,     ~new,     
    "agea",   "Age",  
    "health",  "Poor health",
    "depression",  "Depression",
    "healthy_eating",  "Healthy eating",
)

#A rename tibble or vector automatically only selects the variables included into it
ess_health %>% cor_matrix(var_names = var_renames) %>% apa_cor_table()

#Often, it is also interesting to include variable distributions
ess_health %>% cor_matrix(var_names = var_renames) %>% apa_cor_table(add_distributions = TRUE, data = ess_health)


```



## Describe categorical variables and their relation with an outcome

Often, we are also interested in how the means of an outcome variable differ between different groups. It can be fiddly to get these tables and the pairwise significance tests done, but this function does it in a breeze.

```r
# Start with this in the console - that gets you 90% of the tribbles below.
# get_rename_tribbles(ess_health, gndr, cntry)

var_renames <- tibble::tribble(
    ~old,     ~new,     
    "gndr",   "Gender",  
    "cntry",  "Country"
)

level_renames <- tibble::tribble(
    ~var,     ~level_old, ~level_new, 
    "gndr",   "1",        "male",       
    "gndr",   "2",        "female",       
    "cntry",  "DE",       "Germany",      
    "cntry",  "FR",       "France",      
    "cntry",  "GB",       "UK"
)

cat_var_table(ess_health, health, gndr, cntry, var_names = var_renames, 
              level_names = level_renames)
```

## Report regression models with standardized coefficients

In psychology, it is often expected that regression models are reported with both unstandardised (B) and standardized (beta) coefficients. This can be fiddly and lead to redundant information. The functions below easily run a model with standardised variables and create a publication-ready table.

```r
ess_health$gndr <- factor(ess_health$gndr)

#Standard lm model
mod1 <- lm(depression ~ agea + gndr + health + cntry, ess_health)

#Model with standardised coefficients

mod2 <- lm_std(depression ~ agea + gndr + health + cntry, ess_health)

report_lm_with_std(mod1, mod2)

#Often the coefficients should be renamed - get_coef_rename_tribble(mod1) 
#is the starting point. In that, markdown formatting can be used.

coef_names <- tibble::tribble(
  ~old,           ~new,           
   "(Intercept)",  "*(Intercept)*", 
   "agea",         "Age",        
   "gndr2",        "Gender *(female)*",       
   "health",       "Poor health",      
   "cntryFR",      "France *(vs DE)*",     
   "cntryGB",      "UK *(vs DE)*"
)

report_lm_with_std(mod1, mod2, coef_renames = coef_names)

#You can also easily display multiple nested models side-by-side and get the
#F-change significance test. For that, all models need to be fit on the same
#dataset, so that I will drop all missing data.

mod1 <- lm(depression ~ agea + gndr + health + cntry, tidyr::drop_na(ess_health))
mod2 <- lm_std(depression ~ agea + gndr + health + cntry, tidyr::drop_na(ess_health))


mod3 <- lm(depression ~ agea * gndr + eisced + health + cntry, tidyr::drop_na(ess_health))
mod4 <- lm_std(depression ~ agea * gndr + eisced + health + cntry, tidyr::drop_na(ess_health))

coef_names <- tibble::tribble(
  ~old,           ~new,           
   "(Intercept)",  "*(Intercept)*", 
   "agea",         "Age",        
   "gndr2",        "Gender *(female)*",       
   "health",       "Poor health",      
   "cntryFR",      "France *(vs DE)*",     
   "cntryGB",      "UK *(vs DE)*",
   "eisced",       "Education",
   "agea:gndr2",   "Age x Female",
)

report_lm_with_std(mod = list(mod1, mod3), mod_std = list(mod2, mod4),
                   coef_renames = coef_names, R2_change = TRUE)
```

# Related/alternative packages

- (`modelsummary`)[https://vincentarelbundock.github.io/modelsummary/] allows you to create highly customisable tables with data summaries or the output of statistical models that can be saved in a wide range of formats.
- (`apa`)[https://cran.r-project.org/web/packages/apa/apa.pdf] mostly offers functions that turn the output of statistical tests (e.g., t-tests) into text, in line with APA guidelines.
- (`papaja`)[https://github.com/crsh/papaja] offers the opportunity to create full APA-style journal manuscripts in R. It's `apa_table` function is a generic alternative to the table functions in this package, which support many more types of models, but includes fewer details. 