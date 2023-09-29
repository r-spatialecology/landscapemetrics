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
#'
#' augusta_nlcd <- terra::rast(landscapemetrics::augusta_nlcd)
#' proj_info(augusta_nlcd)
#' podlasie_ccilc <- terra::rast(landscapemetrics::podlasie_ccilc)
#' proj_info(podlasie_ccilc)
#' landscape <- terra::rast(landscapemetrics::landscape)
#' proj_info(landscape)
#'
#' @aliases proj_info
#' @rdname proj_info
#'
#' @keywords internal
#'
#' @export
proj_info <- function(landscape) {

    # get projection of raster
    landscape_proj <- terra::crs(landscape, proj = TRUE)

    # there is a projection
    if (landscape_proj != "") {

        # long-lat projection
        if (terra::is.lonlat(landscape)) {

            tibble::new_tibble(list(crs = "geographic", units = "degrees"))

        # projected projection
        } else {

            # get units
            proj_units <- strsplit(sub(".*units=", "", landscape_proj), " ",
                                   fixed = TRUE)[[1]][[1]]

            tibble::new_tibble(list(crs = "projected", units = proj_units))
        }

    # no projection present
    } else {
        tibble::new_tibble(list(crs = NA, units = NA))
    }
}
