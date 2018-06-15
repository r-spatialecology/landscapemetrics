
[![Travis build
status](https://travis-ci.org/marcosci/landscapemetrics.svg?branch=master)](https://travis-ci.org/marcosci/landscapemetrics)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/marcosci/landscapemetrics?branch=master&svg=true)](https://ci.appveyor.com/project/marcosci/landscapemetrics)
[![Coverage
status](https://codecov.io/gh/marcosci/landscapemetrics/branch/master/graph/badge.svg)](https://codecov.io/github/marcosci/landscapemetrics?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# landscapemetrics

The goal of landscapemetrics is to …

## Installation

You can install landscapemetrics from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("marcosci/landscapemetrics")
```

## Example

``` r
library(raster)
n_classes <- length(unique(landscapemetrics::landscape))
lsm_calculate(landscape, classes_max = n_classes)
#> # A tibble: 148 x 6
#>    layer level class    id metric    value
#>    <int> <chr> <int> <dbl> <chr>     <dbl>
#>  1     1 patch     1     1 area   0.0001  
#>  2     1 patch     1     2 area   0.0005  
#>  3     1 patch     1     3 area   0.0148  
#>  4     1 patch     1     4 area   0.0001  
#>  5     1 patch     1     5 area   0.0001  
#>  6     1 patch     1     6 area   0.0014  
#>  7     1 patch     1     7 area   0.000300
#>  8     1 patch     1     8 area   0.0005  
#>  9     1 patch     1     9 area   0.0001  
#> 10     1 patch     2    10 area   0.0035  
#> # ... with 138 more rows
```

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md).
