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
#' data_info(terra::unwrap(landscape))
#' data_info(terra::unwrap(augusta_nlcd))
#' data_info(terra::unwrap(podlasie_ccilc))
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
