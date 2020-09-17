
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mal

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/mcguinlu/mal/workflows/R-CMD-check/badge.svg)](https://github.com/mcguinlu/mal/actions)
<!-- badges: end -->

The goal of mal is to …

## Installation

You can install the released version of mal from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("mal")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mcguinlu/mal")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(mal)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist    
#>  Min.   : 4.0   Min.   :  2  
#>  1st Qu.:12.0   1st Qu.: 26  
#>  Median :15.0   Median : 36  
#>  Mean   :15.4   Mean   : 43  
#>  3rd Qu.:19.0   3rd Qu.: 56  
#>  Max.   :25.0   Max.   :120
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
