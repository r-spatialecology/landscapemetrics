#' Simpson's evenness index (landscape level)
#'
#' @description Simpson's evenness index (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return Value >= 1
#'
#' @examples
#' lsm_l_siei(landscape)
#'
#' @aliases lsm_l_siei
#' @rdname lsm_l_siei
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_siei <- function(landscape) UseMethod("lsm_l_siei")

#' @name lsm_l_siei
#' @export
lsm_l_siei.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_siei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_siei
#' @export
lsm_l_siei.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_siei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_siei
#' @export
lsm_l_siei.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_siei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_siei
#' @export
lsm_l_siei.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_siei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_siei_calc <- function(landscape) {

    sidi <- lsm_l_sidi(landscape)
    pr <- lsm_l_pr(landscape)

    siei <- sidi$value / (1 - 1 / pr$value)

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "Simpson's evenness index",
        value = siei
    )
}
