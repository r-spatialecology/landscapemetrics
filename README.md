
<!-- README.md is generated from README.Rmd. Please edit that file -->

# landscapemetrics <img src="man/figures/logo.png" align="right" width="150" />

[![Travis build
status](https://travis-ci.org/r-spatialecology/landscapemetrics.svg?branch=master)](https://travis-ci.org/r-spatialecology/landscapemetrics)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/r-spatialecology/landscapemetrics?branch=master&svg=true)](https://ci.appveyor.com/project/r-spatialecology/landscapemetrics)
[![Coverage
status](https://codecov.io/gh/r-spatialecology/landscapemetrics/branch/master/graph/badge.svg)](https://codecov.io/github/r-spatialecology/landscapemetrics?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN
status](https://www.r-pkg.org/badges/version/landscapemetrics)](https://cran.r-project.org/package=landscapemetrics)
[![](http://cranlogs.r-pkg.org/badges/grand-total/landscapemetrics)](http://cran.rstudio.com/web/packages/landscapemetrics/index.html)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

## Overview

**landscapemetrics** is an R package for calculating landscape metrics
for categorical landscape patterns in a tidy workflow. The package can
be used as a drop-in replacement for FRAGSTATS (McGarigal *et al.*
2012), as it offers a reproducible workflow for landscape analysis in a
single environment. It also allows for calculations of four theoretical
metrics of landscape complexity: an overall spatio-thematic complexity,
a thematic complexity, a configurational complexity, and a disambiguator
of pattern types having the same overall complexity (Nowosad and
Stepinski 2018).

**landscapemetrics** supports `raster` spatial objects and takes
`RasterLayer`, `RasterStacks`, `RasterBricks` or lists of `RasterLayer`
as input arguments. Every function can be used in a piped workflow, as
it always takes the data as the first argument and returns a tibble.

## Installation

There are several ways to install **landscapemetrics**:

``` r
# Get the stable version from CRAN
install.packages("landscapemetrics")

# Alternatively, you can install the development version from Github
# install.packages("devtools")
devtools::install_github("r-spatialecology/landscapemetrics", ref = "stable")
```

## Using landscapemetrics

The resolution of a raster cell has to be in **meters**, as the package
converts units internally and returns results in either meters, square
meters or hectares. Before using **landscapemetrics**, be sure to check
your raster (see `check_raster()`).

All functions in **landscapemetrics** start with `lsm_` (for
landscapemetrics). The second part of the name specifies the level
(patch - `p`, class - `c` or landscape - `l`). The last part of the
function name is the abbreviation of the corresponding metric (e.g.
`enn`for the euclidean nearest-neighbor distance):

    # general structure
    lsm_"level"_"metric"
    
    # Patch level
    ## lsm_p_"metric"
    lsm_p_enn()
    
    # Class level
    ## lsm_c_"metric"
    lsm_c_enn()
    
    # Landscape level
    ## lsm_p_"metric"
    lsm_l_enn()

All functions return an identical structured tibble:

<center>

<p style="text-align:center;">

| layer | level     | class | id | metric           | value |
| ----- | --------- | ----- | -- | ---------------- | ----- |
| 1     | patch     | 1     | 1  | landscape metric | x     |
| 1     | class     | 1     | NA | landscape metric | x     |
| 1     | landscape | NA    | NA | landscape metric | x     |

</p>

</center>

### Using metric functions

Every function follows the same implementation design, so the usage is
quite straightforward:

``` r
library(landscapemetrics)
library(dplyr)

# landscape raster
landscape
```

    ## class       : RasterLayer 
    ## dimensions  : 30, 30, 900  (nrow, ncol, ncell)
    ## resolution  : 1, 1  (x, y)
    ## extent      : 0, 30, 0, 30  (xmin, xmax, ymin, ymax)
    ## coord. ref. : NA 
    ## data source : in memory
    ## names       : clumps 
    ## values      : 1, 3  (min, max)

``` r
# calculate for example the Euclidean nearest-neighbor distance on patch level
lsm_p_enn(landscape)
```

    ## # A tibble: 27 x 6
    ##    layer level class    id metric value
    ##    <int> <chr> <int> <int> <chr>  <dbl>
    ##  1     1 patch     1     1 enn     7   
    ##  2     1 patch     1     2 enn     2.83
    ##  3     1 patch     1     3 enn     4   
    ##  4     1 patch     1     4 enn     2.83
    ##  5     1 patch     1     5 enn     4.24
    ##  6     1 patch     1     6 enn     4.12
    ##  7     1 patch     1     7 enn     2   
    ##  8     1 patch     1     8 enn     2   
    ##  9     1 patch     1     9 enn     4.12
    ## 10     1 patch     2    10 enn     4.47
    ## # ... with 17 more rows

``` r
# calculate the total area and total class edge length
bind_rows(
    lsm_l_ta(landscape), 
    lsm_c_te(landscape)
)
```

    ## # A tibble: 4 x 6
    ##   layer level     class    id metric  value
    ##   <int> <chr>     <int> <int> <chr>   <dbl>
    ## 1     1 landscape    NA    NA ta       0.09
    ## 2     1 class         1    NA te     180   
    ## 3     1 class         2    NA te     227   
    ## 4     1 class         3    NA te     321

There is also a wrapper around every metric in the package to quickly
calculate a bunch of metrics:

``` r
# calculate all metrics on patch level
calculate_lsm(landscape, level = "patch")
```

    ## # A tibble: 324 x 6
    ##    layer level class    id metric  value
    ##    <chr> <chr> <int> <int> <chr>   <dbl>
    ##  1 1     patch     1     1 area   0.0001
    ##  2 1     patch     1     2 area   0.0148
    ##  3 1     patch     1     3 area   0.0005
    ##  4 1     patch     1     4 area   0.0014
    ##  5 1     patch     1     5 area   0.0001
    ##  6 1     patch     1     6 area   0.0005
    ##  7 1     patch     1     7 area   0.0001
    ##  8 1     patch     1     8 area   0.0001
    ##  9 1     patch     1     9 area   0.0003
    ## 10 1     patch     2    10 area   0.0035
    ## # ... with 314 more rows

### Utility functions

**landscapemetrics** further provides several visualization functions,
e.g. show all labeld patches or the core area of all patches. All
visualization functions start with the prefix `show_` (e.g.
`show_cores()`).

Important building blocks of the package are exported to help facilitate
analysis or the development of new metrics. They all start with the
prefix `get_`. All of them are implemented with Rcpp and have either
memory or performance advantages compared to raster functions.

For more details, see the [utility function
vignette](https://r-spatialecology.github.io/landscapemetrics/articles/articles/utility.html).

## Contributing

One of the major motivations behind **landscapemetrics** is the idea to
provide an open-source code collection of landscape metrics. This
includes, besides bug reports, especially the idea to include new
metrics and functions. Therefore, in case you want to suggest new
metrics or functions and in the best case even contribute code, we
warmly welcome to do so\! For more information see
[CONTRIBUTING](CONTRIBUTING.md).

Maintainers and contributors must follow this repository’s [CODE OF
CONDUCT](CODE_OF_CONDUCT.md).

### References

  - McGarigal, K., Cushman, S.A., and Ene E. 2012. FRAGSTATS v4: Spatial
    Pattern Analysis Program for Categorical and Continuous Maps.
    Computer software program produced by the authors at the University
    of Massachusetts, Amherst. Available at the following website:
    <http://www.umass.edu/landeco/research/fragstats/fragstats.html>
  - Nowosad J., TF Stepinski. 2018. Information-theoretical approach to
    measure landscape complexity. <https://doi.org/10.1101/383281>
