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
#' augusta_nlcd <- terra::rast(landscapemetrics::augusta_nlcd)
#' data_info(augusta_nlcd)
#' podlasie_ccilc <- terra::rast(landscapemetrics::podlasie_ccilc)
#' data_info(podlasie_ccilc)
#' landscape <- terra::rast(landscapemetrics::landscape)
#' data_info(landscape)
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

    tibble::new_tibble(list(class = class, n_classes = length(landscape_values)))
}
