#' proj_info
#'
#' @description Projection info
#'
#' @param landscape RasterLayer
#'
#' @details
#' Check projection of RasterLayer
#'
#' @return raster
#'
#' @examples
#' proj_info(landscape)
#' proj_info(augusta_nlcd)
#' proj_info(podlasie_ccilc)
#'
#' @aliases proj_info
#' @rdname proj_info
#'
#' @keywords internal
#'
#' @export
proj_info <- function(landscape) {

    # get projection of raster
    landscape_proj <- raster::projection(landscape)

    # there is a projection
    if (!is.na(landscape_proj)) {

        # long-lat projection
        if(raster::isLonLat(landscape)) {

            tibble::tibble(crs = "geographic", units = "degrees")
        }

        # projected projection
        else {

            # get units
            proj_units <- strsplit(sub(".*units=", "", landscape_proj), " ",
                                   fixed = TRUE)[[1]][[1]]

            tibble::tibble(crs = "projected", units = proj_units)
        }
    }

    # no projection present
    else {
        tibble::tibble(crs = NA, units = NA)
    }
}
