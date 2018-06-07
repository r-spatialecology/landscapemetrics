#' Landscape shape index (landscape level)
#'
#' @description Landscape shape index (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return Value >= 1
#'
#' @examples
#' lsm_l_lsi(landscape)
#'
#' @aliases lsm_l_lsi
#' @rdname lsm_l_lsi
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_lsi <- function(landscape) UseMethod("lsm_l_lsi")

#' @name lsm_l_lsi
#' @export
lsm_l_lsi.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_lsi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_lsi
#' @export
lsm_l_lsi.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_lsi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_lsi
#' @export
lsm_l_lsi.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_lsi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_lsi
#' @export
lsm_l_lsi.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_lsi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_lsi_calc <- function(landscape) {

    edges <- lsm_l_te(landscape) # Needs to include also edge to background
    area <- lsm_l_ta(landscape)

    lsi <- (0.25 * edges$value) / sqrt(area$value)

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "landscape shape index",
        value = lsi
    )
}
