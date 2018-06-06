#' Disjunct core area density (class level)
#'
#' @description Disjunct core area density (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_l_dcad(landscape)
#'
#' @aliases lsm_l_dcad
#' @rdname lsm_l_dcad
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_dcad <- function(landscape) UseMethod("lsm_l_dcad")

#' @name lsm_l_dcad
#' @export
lsm_l_dcad.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_dcad_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_dcad
#' @export
lsm_l_dcad.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_dcad_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_dcad
#' @export
lsm_l_dcad.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_dcad_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_dcad
#' @export
lsm_l_dcad.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_dcad_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_dcad_calc <- function(landscape){

    total_area <- lsm_l_ta(landscape)

    dcad <- lsm_l_ndca(landscape) %>%
        dplyr::mutate(value = value / total_area$value  * 100  * 10000) # Correct unit?

    tibble::tibble(
        level = "class",
        class = dcad$class,
        id = as.integer(NA),
        metric = "core area index",
        value = dcad$value
    )
}
