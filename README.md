
<!-- README.md is generated from README.Rmd. Please edit that file -->

# learntidymodels

<!-- badges: start -->

[![R build
status](https://github.com/tidymodels/learntidymodels/workflows/R-CMD-check/badge.svg)](https://github.com/tidymodels/learntidymodels/actions)
[![Codecov test
coverage](https://codecov.io/gh/tidymodels/learntidymodels/branch/master/graph/badge.svg)](https://codecov.io/gh/tidymodels/learntidymodels?branch=master)
[![R-CMD-check](https://github.com/tidymodels/learntidymodels/workflows/R-CMD-check/badge.svg)](https://github.com/tidymodels/learntidymodels/actions)
<!-- badges: end -->

Learn [tidymodels](https://www.tidymodels.org/) in the browser or
locally in your RStudio IDE with interactive
[learnr](https://rstudio.github.io/learnr/) primers!

## Installation

For now, you can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tidymodels/learntidymodels")
```

## How to run the tutorials

You can easily start any tutorial with:

``` r
learnr::run_tutorial("tutorial-of-choice", package = "learntidymodels")
```

For example:

``` r
learnr::run_tutorial("pca-recipes", package = "learntidymodels")
```

## List of available tutorials

| Tutorial      | Description                                                                                     |
|:--------------|:------------------------------------------------------------------------------------------------|
| `pca_recipes` | Learn how to conduct dimensionality reduction algorithms using recipes package from tidymodels. |

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
