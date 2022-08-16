
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wellr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/wellr)](https://CRAN.R-project.org/package=wellr)
[![R-CMD-check](https://github.com/rforbiochemists/wellr/workflows/R-CMD-check/badge.svg)](https://github.com/rforbiochemists/wellr/actions)
[![Codecov test
coverage](https://codecov.io/gh/rforbiochemists/wellr/branch/master/graph/badge.svg)](https://app.codecov.io/gh/rforbiochemists/wellr?branch=master)
[![R-CMD-check](https://github.com/rforbiochemists/wellr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rforbiochemists/wellr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`{wellr}` provides a consitent and reliable interface for dealing with
plate-based data and calculations. It provides functions for indexing
through microtitre plates, converting between well IDs (“C05”) and their
respective rows, columns and index. This is a utilitiy package, intended
for use in other packages that deal with plate-based data.

## Installation

<!-- You can install the released version of wellr from [CRAN](https://CRAN.R-project.org) with: -->

You can install the development version from github as below:

``` r
#install.packages("remotes")
remotes::install_github("rforbiochemists/wellr")
```

## Basic Examples

``` r
library(wellr)

well_to_colnum("G8")
#> [1] 8
well_to_rownum("G8")
#> [1] 7
well_to_index("H1")
#> [1] 85
well_to_index("H1", colwise = TRUE)
#> [1] 8
well_from_index(37)
#> [1] "D01"
well_join(3, 8)
#> [1] "C08"
well_join("E", 10)
#> [1] "E10"
```
