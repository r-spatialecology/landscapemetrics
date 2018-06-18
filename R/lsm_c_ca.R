#' Total class area (class level)
#'
#' @description Total area of class (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Total class area equals the sum of the area of all patches of class i. Total class area is
#' an absolute measure, making comparisons among landscapes with different
#' total areas difficult. It is a measure of landscape composition.
#' \deqn{CA = sum(area[patch_i])}
#' \subsection{Units}{Hectares}
#' \subsection{Range}{CA > 0}
#' \subsection{Behaviour}{CA increases without limit as the amount of the class increases.
#' CA = TA if only one class is present}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_ca(landscape)
#'
#' @aliases lsm_c_ca
#' @rdname lsm_c_ca
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_c_ca <- function(landscape) UseMethod("lsm_c_ca")

#' @name lsm_c_ca
#' @export
lsm_c_ca.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ca_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ca
#' @export
lsm_c_ca.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ca_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ca
#' @export
lsm_c_ca.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ca_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ca
#' @export
lsm_c_ca.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_ca_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_ca_calc <- function(landscape) {
    total_area <- landscape %>%
        lsm_p_area_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = total_area$class,
        id = as.integer(NA),
        metric = "total area",
        value = total_area$value
    )
}
