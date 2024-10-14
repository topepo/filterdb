
<!-- README.md is generated from README.Rmd. Please edit that file -->

# filterdb

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/filterdb)](https://CRAN.R-project.org/package=filterdb)
[![Codecov test
coverage](https://codecov.io/gh/topepo/filterdb/branch/main/graph/badge.svg)](https://app.codecov.io/gh/topepo/filterdb?branch=main)
[![R-CMD-check](https://github.com/topepo/filterdb/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/topepo/filterdb/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of filterdb is to …

## Installation

You can install the development version of filterdb like so:

``` r
require(pak)
pak::pak("topepo/filterdb")
```

## Example

``` r
library(filterdb)
library(dplyr)
library(modeldata)

data(ames)
ames$Sale_Price <- log10(ames$Sale_Price)
```

``` r
ames_scores <-
  importance_metrics(
    ames %>% select(-Sale_Price),
    y = ames %>% select(Sale_Price),
    methods = c("corr_rank", "imp_rf", "max_diff")
  )
ames_scores
#> Univariate importance scores
#> • Rank Correlation Filter: 33 predictors
#> 
#> • Random Forest Variable Importance: 73 predictors
#> 
#> • Maximum Group Difference: 40 predictors
```

``` r
results <- tidy(ames_scores)
results
#> # A tibble: 73 × 4
#>    term            corr_rank     imp_rf   max_diff
#>    <chr>          <score_vc> <score_vc> <score_vc>
#>  1 Lot_Frontage       0.2276    0.75115        Inf
#>  2 Lot_Area           0.4292    1.64612        Inf
#>  3 Year_Built         0.6808    5.15427        Inf
#>  4 Year_Remod_Add     0.6015    3.29389        Inf
#>  5 Mas_Vnr_Area       0.4348    1.33136        Inf
#>  6 BsmtFin_SF_1       0.1152    0.47158        Inf
#>  7 BsmtFin_SF_2       0.0339    0.00751        Inf
#>  8 Bsmt_Unf_SF        0.1641    0.33071        Inf
#>  9 Total_Bsmt_SF      0.6066    4.33000        Inf
#> 10 First_Flr_SF       0.5815    3.98487        Inf
#> # ℹ 63 more rows
```

``` r
head(results$corr_rank)
#> [1] 0.228 0.429 0.681 0.601 0.435 0.115
#> Direction: "maximize_abs"
#> Imputed value: 1
```

``` r
results %>% 
  filter(corr_rank < 1/3 & max_diff >= 0.4) %>% 
  slice_max(imp_rf, n = 5)
#> # A tibble: 5 × 4
#>   term           corr_rank     imp_rf   max_diff
#>   <chr>         <score_vc> <score_vc> <score_vc>
#> 1 Latitude           0.295      1.676        Inf
#> 2 Second_Flr_SF      0.247      1.513        Inf
#> 3 Lot_Frontage       0.228      0.751        Inf
#> 4 Bedroom_AbvGr      0.197      0.495        Inf
#> 5 BsmtFin_SF_1       0.115      0.472        Inf
```

## Code of Conduct

Please note that the filterdb project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
