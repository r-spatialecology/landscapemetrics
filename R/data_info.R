#' data_info
#'
#' @description Data info
#'
#' @param landscape RasterLayer
#'
#' @details
#' Check class of raster values and number of classes
#'
#' @return raster
#'
#' @examples
#' augusta_nlcd <- terra::rast(augusta_nlcd)
#' data_info(augusta_nlcd)
#' podlasie_ccilc <- terra::rast(podlasie_ccilc)
#' data_info(podlasie_ccilc)
#' landscape <- terra::rast(landscape)
#' data_info(landscape)
#'
#' @aliases data_info
#' @rdname data_info
#'
#' @keywords internal
#'
#' @export
data_info <- function(landscape){

    # get raster values
    landscape_values <- unique(terra::values(landscape, mat = FALSE))

    # remove NA values (mess up with test if integer values)
    landscape_values <- landscape_values[!is.na(landscape_values)]

    # check if integer value
    class <- ifelse(test = all(is.na(landscape_values)),
                    yes = NA,
                    no = ifelse(
                        test = all(landscape_values %% 1 == 0),
                        yes = "integer",
                        no = "non-integer"))

    tibble::tibble(class = class, n_classes = length(landscape_values))
}
