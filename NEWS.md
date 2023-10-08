# landscapemetrics 2.1.0
* Improvements
    * Many performance improvements. Most visible are in
    `calculate_lsm` (all metrics: more than 5 times faster with 45% less memory 
    allocation for `augusta_nlcd`; larger increases were found for smaller data)
    and `window_lsm` (a single metric: more than 6 times faster for `augusta_nlcd`; 
    larger increases were found for smaller data)
    * Some performance improvements are related to the new "extras" mechanism, in which several objects 
    are precalculated in `calculate_lsm`
    * Creates an internal `extras_df` object that lists which extras are needed by
    each metric
    * Replaces the use of `tibble::tibble()` with `tibble::new_tibble(list())` in most functions.
    This change is partially responsible for improvements of the `window_lsm` speed
    * Replaces `raster_to_points` with `get_points` in several places. 
    The `get_points` function is based on the column and row numbers multiplied by
    the resolution, not actual coordinates.
* New functions
    * Adds a few internal helper functions and documents them, including `prepare_extras`,
    `get_area_patches`, `get_class_patches`, `get_complexity`, `get_enn_patch`, 
    `get_points`, and `get_perimeter_patch`
* Various
    * Fixes several typos and improves documentation in many places
    * Uses object references in most rcpp functions

# landscapemetrics 2.0.0
* Improvements
    * `terra` and `sf` instead of `raster` and `sp` as underlying frameworks
    * Adjust all `show_*` functions to avoid `ggplot2` warning
    * Update color palette of `show_correlation`
* New functions
    * `points_as_mat()` helper function
* Bugfixes
    * There was a bug introduced previously in the calculation of SHEI
    * `extract_lsm` returned an no-needed warning message
    * Minor bug in shape index fixed
    * Minor bug in clumpy index fixed
* Various 
    * Updated FRAGSTATS reference (thanks to Oto Kaláb @kalab-oto)
    * Update FRAGSTATS tests

# landscapemetrics 1.5.6
* Bugfixes
    * SHEI now equals 0 when only one patch is present
* Various
    * Removes `rgeos` dependency
    
# landscapemetrics 1.5.5
* Improvements
    * `get_patches` returns a unique patch id for all classes
    * Adds adds the ability to use a circular window in `window_lsm()`
* Bugfixes
    * SHEI now equals to 0 when only one patch present 
* Various
    * Fixes typo in Maintainer name

# landscapemetrics 1.5.4
* Improvements
    * Improved memory performance related to perimeter and edge calculations

# landscapemetrics 1.5.3
* Improvements
    * Improved terra objects (SpatRaster) support for almost all functions.
    * Bugfix in `get_boundaries()` for matrix input and `return_raster = TRUE`
    * Better naming of list outputs
* Bugfixes
    * Adding `rel_mut_inf` to `list_lsm()`
    * Fix changes due to new `ggplot2` version

# landscapemetrics 1.5.2
* Bugfixes
    * Bug in `lsm_l_ai` if class with only one cell exists
* Various
    * Renamed master to main branch
 
# landscapemetrics 1.5.1
* Improvements
    * Use GitHub actions
* Bugfixes
    * Bug in `lsm_c_lsi`, `lsm_c_nlsi`, `lsm_l_lsi` not using cell surfaces

# landscapemetrics 1.5.0
* Improvements
    * Improved internal understanding of input classes
    * Adds terra objects (SpatRaster) support
* New functions
    * `lsm_l_relmutinf` to calculate relative mutual information
    
# landscapemetrics 1.4.6
* Improvements
    * Improved RAM performance if `to_disk = TRUE` for `spatialize_lsm` and `matrix_to_raster`

# landscapemetrics 1.4.5
* Bugfixes
    * Fix of patch id in `get_circumscribingcircle()`
* Improvements
    * `lsm_p_gyrate` has an argument to force the cell centroid to be within patch
    * `get_nearestneighbour()` can now return ID of neighbouring patch
    * `get_boundaries()` allows now to specify edge depth
    * `get_boundaries()` can return the patch id for edge cells
* New functions
    * `get_centroid()` returns the coordinates of each patch centroid
    
# landscapemetrics 1.4.4
* Improvements
    * Set labels = FALSE as default for all plotting functions (messy for larger raster)
    * Add argument to `sample_lsm()` that adds NA if class is not present in sample plot

# landscapemetrics 1.4.3
* Improvements
    * Improved algorithm to calculate circumscribing circle. This refers to both speed and accuracy.
    * The circumscribing circle is now also calculated for patches with only one cell
* Bugfixes
    * Bugfix for all functions that produce warnings

# landscapemetrics 1.4.2
* Bugfixes
    * Bug in total edge calculation
* Improvements
    * Adds a support for sf polygons in sample_lsm

# landscapemetrics 1.4.1
* Bugfixes
    * Bug in the connected-component labelling algorithm for non-rectangular landscapes

# landscapemetrics 1.4
* Improvements
    * Improved connected-component labelling algorithm. **Note: The algorithm labels the patches in a different order and therefore may use different patch IDs compared to previous versions.** 
    
# landscapemetrics 1.3.2
* Bugfixes
    * Make sure all CRAN checks run

# landscapemetrics 1.3.1
* Bugfixes
    * Return NA if all cells are NA
* Improvements
    * Cleaner handling of warnings

# landscapemetrics 1.3
* Bugfixes
    * Remove NAs on patch level for shape index

# landscapemetrics 1.2.2
* Bugfixes
    * Bug in `lsm_p_circle()` when whole landscape contains only one patch
    * Bug in `extract_lsm()` that directions argument was not passed on
* New functions
    * `unpad_raster` to remove padding around raster
* Improvements
    * New argument in `get_boundaries` to consider landscape boundary

# landscapemetrics 1.2.1
* Bugfixes
    * Allow sf points for sampling metrics
    * Make sure x-y coordinates are used in `sample_lsm()`
    * Size argument is not needed for polygons in `sample_lsm()`
    * Make sure `sample_lsm()` can use SpatialPolygonsDataFrame
* Improvements
    * Update citation

# landscapemetrics 1.2
* New functions
    * `calculate_correlation()` returns a tibble with all correlations between metrics
    * `scale_sample()` allows to sample landscape metrics in buffer with increasing size
    * `scale_window()` allows calculate selected metrics in moving windows over the provided landscape.
* Improvements
    * `show_correlation()` can take result from `calculate_correlation()`
    * `sample_lsm()` returns a warning if `percentage_inside` < 90%
    * `sample_lsm()` and `extract_lsm()` can now be used with sample_ids (rather than just 1...n)

# landscapemetrics 1.1
* Bugfixes
    * Bugfix in `lsm_c_ai()` if only one class and NA values were present
    * Bugfix in `show_correlation()` that first col was lost
    * Bugfix in `sample_lsm()` and `extract_lsm()` to forward arguments to
    `calculate_lsm()`
    * size argument in `sample_lsm()` is now comparable between squares and circles
    * Bugfix in `window_lsm()` that some arguments were not passed on (resolution and points)
    * Bugfix in `extract_lsm()` and `window_lsm()` that allowed metric subset was wrong
* Improvements
    * The values can be added as geom_text in `show_correlation()`
    * `list_lsm()` allows to return all BUT the selected metrics
    * `sample_lsm()` can now use SpatialPolygons to sample metrics
    * `sample_lsm()` can now handle SpatialLines to sample metrics
    * `sample_lsm()` automatically detects provided data type
    * `extract_lsm()` can now handle SpatialLines to extract metrics
    * Updates the way of calculating `lsm_l_ent()` and thus `lsm_l_condent()` and `lsm_l_mutinf()`.
    * `calculate_lsm()`/`extract_lsm()`/`sample_lsm()`/`spatialize_lsm()`/`window_lsm()` can print progress
    * replaced `cat()` with `message()`
    * `calculate_lsm()` returns an error message if selected metrics do not exist
    * `construct_buffer()` can now return a matrix with coords instead of polygons
    * `calculate_lsm()` checks the input data before calculating metrics
* Renaming
    * `get_lsm()` is now called `spatialize_lsm()`

# landscapemetrics 1.0
* New functions
    * New function `moving_window()` to calculate metrics within a moving window
    * New function `get_lsm()` to raster in which each cell has patch metric value
* Bugfixes
    * Bugfix in `check_landscape()` if NA values were present
    * Bugfix in `pad_raster()` that pad_raster_cells > 1 did not work
    * Bugfix in `calculate_lsm()` calculating class area where resolution was lost
* Improvements
    * Removing all functions from `dplyr`
    * Better structure for helping functions of `check_landscape()`
    * Better handling of RasterStacks for `check_landscape()`
    * `pad_raster()` is now typestable and always returns a list
    * `pad_raster()` can now return also a RasterLayer (and not just a matrix as)

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
