#' Percentage of landscape (class level)
#'
#' @description Percentage of landscape of class (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Percentage of landscape equals the sum of the patch area of class i divided
#' by the total area. In other words, PLAND is the percentage of the landscape
#' belonging to class i. It is a measure of compostion and because of the relative
#' character directly comparable among landscapes with different total areas
#' \deqn{PLAND = (sum(area[patch_i]) / total area) * 100}
#' \subsection{Units}{Percentage}
#' \subsection{Range}{0 < PLAND <= 100 \cr PLAND approaches PLAND = 0 when the class area
#'  is decreasing. PLAND = 100 when only one class is present}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_pland(landscape)
#'
#' @aliases lsm_c_pland
#' @rdname lsm_c_pland
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_pland <- function(landscape) UseMethod("lsm_c_pland")

#' @name lsm_c_pland
#' @export
lsm_c_pland.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_pland_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_pland
#' @export
lsm_c_pland.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_pland_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_pland
#' @export
lsm_c_pland.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_pland_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_pland
#' @export
lsm_c_pland.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_pland_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_pland_calc <- function(landscape){

    pland <- landscape %>%
        lsm_p_area() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value)) %>%
        dplyr::mutate(value = value / sum(value) * 100)

    tibble::tibble(
        level = "class",
        class = pland$class,
        id = as.integer(NA),
        metric = "percentage of landscape",
        value = pland$value
    )
}

