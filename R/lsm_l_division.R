#' Division index (class level)
#'
#' @description Division index (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return Value >= 1
#'
#' @examples
#' lsm_l_division(landscape)
#'
#' @aliases lsm_l_division
#' @rdname lsm_l_division
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_division <- function(landscape) UseMethod("lsm_l_division")

#' @name lsm_l_division
#' @export
lsm_l_division.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_division_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_division
#' @export
lsm_l_division.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_division_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_division
#' @export
lsm_l_division.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_division_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_division
#' @export
lsm_l_division.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_division_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_division_calc <- function(landscape) {

    total_area <- lsm_l_ta(landscape)

    division <- landscape %>%
        lsm_p_area() %>%
        dplyr::mutate(value = (value / total_area$value) ^ 2) %>%
        dplyr::summarise(value = sum(value)) %>%
        dplyr::mutate(value = 1 - value)

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "landscape division index",
        value = division$value
    )
}
