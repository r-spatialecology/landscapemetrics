#' data_info
#'
#' @description Adding padding to raster
#'
#' @param landscape RasterLayer
#'
#' @details
#' Check class of raster values and number of classes
#'
#' @return raster
#'
#' @examples
#' data_info(landscape)
#' data_info(augusta_nlcd)
#' data_info(podlasie_ccilc)
#'
#' @aliases data_info
#' @rdname data_info
#'
#' @keywords internal
#'
#' @export
data_info <- function(landscape){

    # get raster values
    landscape_values <- unique(raster::values(landscape))

    # check if all values are NA
    if(all(is.na(landscape_values))) {
        stop("All raster values NA.", call. = FALSE)
    }

    # remove NA values (mess up with test if integer values)
    landscape_values <- landscape_values[!is.na(landscape_values)]

    # check if integer value
    class <- dplyr::if_else(condition = all(landscape_values %% 1 == 0),
                            true = "integer",
                            false = "non-integer")

    tibble::tibble(class = class,
                   n_classes = length(landscape_values))
}
