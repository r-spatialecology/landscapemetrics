# landscapemetrics 0.2

* Implemented `show_cores`, a function to plot the core area of patches
* Implemented `sampling_metrics`, a function to sample metrics around buffered points
* `show_patches` now also shows labelled class facets (option `what`)
* Removed all functions from the purrr package and replaced them by `lapply`
* Removed all pipes

# landscapemetrics 0.1.1

* Replaced isFALSE() with !isTRUE() to be compatibile to R (> 3.1)
* Bugfix: lsm_p_core() and lsm_p_ncore() now takes landscape boundary into account
* Added namespace prefix std::fmod() in get_adjacency.cpp

# landscapemetrics 0.1.0

* First submission to CRAN
