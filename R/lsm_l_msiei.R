#' Modified Simpson's evenness index (landscape level)
#'
#' @description Modified Simpson's evenness index (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return Value >= 1
#'
#' @examples
#' lsm_l_msiei(landscape)
#'
#' @aliases lsm_l_msiei
#' @rdname lsm_l_msiei
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_msiei <- function(landscape) UseMethod("lsm_l_msiei")

#' @name lsm_l_msiei
#' @export
lsm_l_msiei.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_msiei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_msiei
#' @export
lsm_l_msiei.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_msiei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_msiei
#' @export
lsm_l_msiei.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_msiei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_msiei
#' @export
lsm_l_msiei.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_msiei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_msiei_calc <- function(landscape) {

    msidi <- lsm_l_msidi(landscape)
    pr <- lsm_l_pr(landscape)

    msiei <- msidi$value / log(pr$value)

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "modified Simpson's evenness index",
        value = msiei
    )
}
