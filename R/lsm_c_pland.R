#' Percentage of landscape (class level)
#'
#' @description Percentage of landscape (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
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

