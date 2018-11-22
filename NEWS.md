# landscapemetrics 0.3
- `check_landscape` function to make sure your landscapes are feasible for landscapemetrics
- Bugfix in `lsm_l_rpr`: Typo in internal function, used landscapemetrics::landscape instead of user input
- Renamed "new metric" group to "complexity metric" group
- Updated structure of `lsm_abbreviations_names`
- `list_lsm()` function to print available metrics
- `show_lsm()` function to vizualize patch level metrics
- Bugfix in all show_() - functions that na.value color is identical
- `calculate_lsm()` now uses `list_lsm()`. This allows more options to specify metrics to calculate
- `what` arguments of all `show_()`-functions now are named `class` for consistency (so all `what` arguments deal with metrics)
- `show_()`-functions don't throw warnings
- "global" facet for all `show_()` functions
- `what` arguments of `get_patches()` is now named `class` for consistency (so all `what` arguments deal with metrics)
- `extract_lsm()` now uses `list_lsm()`. This allows more options to specify metrics to calculate
- `rcpp_get_coocurrence_matrix()` can now handle large rasters and is faster
- `get_patches()` can now also return a matrix
- All functions using `get_patches()` are more memory efficient
- `pad_raster()` now takes and returns a matrix
- New function `get_unique_values()` that shows all uniques labels in a class

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
