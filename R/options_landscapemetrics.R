#' options_landscapemetrics
#'
#' @description Sets global options for landscapemetrics
#'
#' @param to_disk Logical argument, if FALSE results of get_patches are hold
#' in memory. If true, get_patches writes temporary files and hence, does not hold everything in memory.
#' Can be set with a global option, e.g. `option(to_disk = TRUE)`. See Details.
#'
#' @details
#'
#' Landscape metrics rely on the delineation of patches. Hence, `get_patches` is
#' heavily used in **landscapemetrics**. As raster can be quite big, the fact that
#' `get_patches` creates a copy of the raster for each class in a landscape becomes
#' a burden for computer memory. Hence, the argument *to_disk* allows to
#' store the results of the connected labeling algorithm on disk. Furthermore,
#' this option can be set globally, so that every function that internally uses
#' `get_patches` can make use of that.
#'
#' @return Global option to be used internally in the package
#'
#' @aliases options_landscapemetrics
#' @rdname options_landscapemetrics
#'
#' @export

options_landscapemetrics <- function(to_disk = NULL) {

    if(!is.null(to_disk)){
        options(to_disk = to_disk)
    }

}
