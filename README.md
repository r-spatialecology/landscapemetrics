
[![Travis build
status](https://travis-ci.org/marcosci/landscapemetrics.svg?branch=master)](https://travis-ci.org/marcosci/landscapemetrics)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/marcosci/landscapemetrics?branch=master&svg=true)](https://ci.appveyor.com/project/marcosci/landscapemetrics)
[![Coverage
status](https://codecov.io/gh/marcosci/landscapemetrics/branch/master/graph/badge.svg)](https://codecov.io/github/marcosci/landscapemetrics?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# landscapemetrics

**landscapemetrics** is an R package for calculating landscape metrics
for categorical landscape patterns in a tidy workflow. It offers most of
the metrics that are available in the standalone software FRAGSTATS
(McGarigal, SA Cushman & Ene, 2012) but also allows for implementation
of future (meaningful) categorical landscape metrics.

This package supports `raster` spatial objects and takes `RasterLayer`,
`RasterStacks`, `RasterBricks` or lists of `RasterLayer` as input
arguments.

Every function can be used in a piped workflow, as it always takes the
data as the first argument and returns a tibble of the same dimension.

## Installation

You can install **landscapemetrics** from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("marcosci/landscapemetrics")
```

## Using landscapemetrics

The functions in **landscapemetrics** starts with `lsm_` and next are
named based on a combination of abbreviations describing the scale
(patch - `p`, class - `c` or landscape - `l`) and metric they calculate:

    # landscapemetrics
    lsm_"level"_"metric" example:
    
    # Patch level
    ## lsm_p_"metric" example:
    lsm_p_enn()
    
    # Class level
    ## lsm_c_"metric" example:
    lsm_c_enn()
    
    # Landscape level
    ## lsm_p_"metric" example:
    lsm_l_enn()

…and returns a tibble with the same columns:

| layer | level     | class | id | metric           | value |
| ----- | --------- | ----- | -- | ---------------- | ----- |
| 1     | patch     | 1     | 1  | landscape metric | x     |
| 1     | class     | 1     | NA | landscape metric | x     |
| 1     | landscape | NA    | NA | landscape metric | x     |

### Using metric functions

Every function follows the same implementation design, so the usage is
quite straight forward:

``` r
library(landscapemetrics)
library(tidyverse)

# Landscape raster
landscape
#> class       : RasterLayer 
#> dimensions  : 30, 30, 900  (nrow, ncol, ncell)
#> resolution  : 1, 1  (x, y)
#> extent      : 0, 30, 0, 30  (xmin, xmax, ymin, ymax)
#> coord. ref. : NA 
#> data source : in memory
#> names       : clumps 
#> values      : 1, 3  (min, max)

# Calculate for example the Euclidean Nearest-Neighbor Distance on patch level
landscape %>% 
  lsm_p_enn()
#> # A tibble: 27 x 6
#>    layer level class    id metric                                    value
#>    <int> <chr> <int> <int> <chr>                                     <dbl>
#>  1     1 patch     1     1 euclidean nearest neighbor distance dist…  7   
#>  2     1 patch     1     2 euclidean nearest neighbor distance dist…  4   
#>  3     1 patch     1     3 euclidean nearest neighbor distance dist…  2.83
#>  4     1 patch     1     4 euclidean nearest neighbor distance dist…  2   
#>  5     1 patch     1     5 euclidean nearest neighbor distance dist…  2   
#>  6     1 patch     1     6 euclidean nearest neighbor distance dist…  2.83
#>  7     1 patch     1     7 euclidean nearest neighbor distance dist…  4.12
#>  8     1 patch     1     8 euclidean nearest neighbor distance dist…  4.12
#>  9     1 patch     1     9 euclidean nearest neighbor distance dist…  4.24
#> 10     1 patch     2    10 euclidean nearest neighbor distance dist…  4.47
#> # ... with 17 more rows
```

## Contributing

Bug reports, suggestions for new metrics, and especially code
contributions are welcome. Please see
[CONTRIBUTING.md](https://github.com/marcosci/landscapemetrics/blob/master/CONTRIBUTING.md).
Maintainers and contributors must follow this repository’s [code of
conduct](CODE_OF_CONDUCT.md).
