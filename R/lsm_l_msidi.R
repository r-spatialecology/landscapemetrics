#' Modified Simpson's diversity index (landscape level)
#'
#' @description Modified Simpson's diversity index (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return Value >= 1
#'
#' @examples
#' lsm_l_msidi(landscape)
#'
#' @aliases lsm_l_msidi
#' @rdname lsm_l_msidi
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_msidi <- function(landscape) UseMethod("lsm_l_msidi")

#' @name lsm_l_msidi
#' @export
lsm_l_msidi.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_msidi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_msidi
#' @export
lsm_l_msidi.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_msidi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_msidi
#' @export
lsm_l_msidi.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_msidi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_msidi
#' @export
lsm_l_msidi.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_msidi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_msidi_calc <- function(landscape) {

    msidi <- landscape %>%
        lsm_c_pland() %>%
        dplyr::mutate(value = (value / 100) ^ 2) %>%
        dplyr::pull(value) %>%
        sum() %>%
        log() %>%
        magrittr::multiply_by(-1)

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "modified Simpson's diversity index",
        value = msidi
    )
}
