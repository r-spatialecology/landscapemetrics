#' Simpson's diversity index (landscape level)
#'
#' @description Simpson's diversity index (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return Value >= 1
#'
#' @examples
#' lsm_l_sidi(landscape)
#'
#' @aliases lsm_l_sidi
#' @rdname lsm_l_sidi
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_sidi <- function(landscape) UseMethod("lsm_l_sidi")

#' @name lsm_l_sidi
#' @export
lsm_l_sidi.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_sidi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_sidi
#' @export
lsm_l_sidi.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_sidi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_sidi
#' @export
lsm_l_sidi.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_sidi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_sidi
#' @export
lsm_l_sidi.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_sidi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_sidi_calc <- function(landscape) {

    sidi <- landscape %>%
        lsm_c_pland() %>%
        dplyr::mutate(value = (value / 100) ^ 2) %>%
        dplyr::summarise(value = 1 - sum(value))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "Simpson's diversity index",
        value = sidi$value
    )
}
