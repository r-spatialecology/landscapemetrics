# landscapemetrics 0.3.2
* Bugfixes
    * Bugfix in `check_landscape()` if NA values were present
    * Bugfix in `pad_raster()` that pad_raster_cells > 1 did not work
* Improvements
    * Removing all functions from `dplyr`
    * Better structure for helping functions of `check_landscape()`
    * Better handling of RasterStacks for `check_landscape()`
    * `pad_raster()` is now typestable and always returns a list
    * `pad_raster()` can now return also a RasterLayer (and not just a matrix as before)

# landscapemetrics 0.3.1
* Bugfixes
    * fixing bug in `sample_lsm()` that occured when metrics where selected using `what` argument
    * Bugfix in `lsm_p_core()` if only one patch is present
    * Bugfix in `lsm_p_circle()` if only one cell is present in class
    * Bugfix in `lsm_p_hyrate()` if only one cell is present in class
    * Bugfix in `get_adjacencies()` that checks if `neighbourhood` was specified correctly did not work properly
* Improvements
    * `sample_lsm()` now returns a tibble including an extra column if `return_raster = TRUE` (and not a nested tibble as before)
    * Better explanation on how to specify metrics for `list_lsm()`, `calculate_lsm()` and ``sample_lsm()`
* Renaming
    * Renamed argument `return_plots` to `return_raster` in `sample_lsm()`

# landscapemetrics 0.3
* New functions
    * New function `check_landscape` to make sure your landscapes are feasible for landscapemetrics
    * New function `raster_to_points()` to get also NA cells (not possible with `raster::rasterToPoints`)
    * New function `get_boundaries()` to get only boundary cells
    * New function `get_unique_values()` that shows all uniques labels in a class
    * New function `list_lsm()` function to print available metrics
    * New function `show_lsm()` function to vizualize patch level metrics
* Bugfixes
    * Bugfix in `lsm_l_rpr`: Typo in internal function, used landscapemetrics::landscape instead of user input
    * Bugfix in all `show_()` * functions that na.value color is identical
* Improvements
    * Most `get_`-functions can now take matrix as input and also return a matrix
    * `calculate_lsm()` now uses `list_lsm()`. This allows more options to specify metrics to calculate
    * Updated structure of `lsm_abbreviations_names`
    * `show_()`-functions don't throw warnings
    * "global" facet for all `show_()` functions
    * `extract_lsm()` now uses `list_lsm()`. This allows more options to specify metrics to calculate
    * `rcpp_get_coocurrence_matrix()` can now handle large rasters and is faster
    * `lsm_p_circle()` and `get_circumscribingcircle()` now consideres different x- & y-resolutions
    * Generally, a better use of Rcpp to decrease computational time and memory demand
* Renaming
    * Renamed "new metric" group to "complexity metric" group
    * `what` arguments of all `show_()`-functions now are named `class` for consistency (so all `what` arguments deal with metrics)
    * `what` arguments of `get_patches()` is now named `class` for consistency (so all `what` arguments deal with metrics)

# landscapemetrics 0.2
* Unified naming scheme for all auxiliary functions:
    * `calculate_metrics` is now `calculate_lsm`
* Implemented `show_cores`, a function to plot the core area of patches
* `show_patches` now also shows labelled class facets (option `what`)
* All plot functions have the same theme
* Implemented `sample_lsm`, a function to sample metrics around buffered points
* Implemented `extract_lsm`, a function to extract landscape metrics for spatial coordinates
* Removed all functions from the `purrr` package and replaced them by `lapply`
* Removed all pipes
* `calculate_lsm` has the option `progress`
* `consider_boundary` is available for all core metrics
* The `edge_depth` can be specified for all core metrics

# landscapemetrics 0.1.1
* Replaced isFALSE() with !isTRUE() to be compatibile to R (> 3.1)
* Bugfix: lsm_p_core() and lsm_p_ncore() now takes landscape boundary into account
* Added namespace prefix std::fmod() in get_adjacency.cpp

# landscapemetrics 0.1.0
* First submission to CRAN
