#' Disjunct core area density (class level)
#'
#' @description Disjunct core area density (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_c_dcad(landscape)
#'
#' @aliases lsm_c_dcad
#' @rdname lsm_c_dcad
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_dcad <- function(landscape) UseMethod("lsm_c_dcad")

#' @name lsm_c_dcad
#' @export
lsm_c_dcad.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_dcad_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_dcad
#' @export
lsm_c_dcad.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_dcad_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_dcad
#' @export
lsm_c_dcad.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_dcad_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_dcad
#' @export
lsm_c_dcad.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_dcad_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_dcad_calc <- function(landscape){

    total_area <- lsm_l_ta(landscape)

    dcad <- lsm_c_ncore(landscape) %>%
        dplyr::mutate(value = value / total_area$value  * 100  * 10000) # Correct unit?

    tibble::tibble(
        level = "class",
        class = dcad$class,
        id = as.integer(NA),
        metric = "disjunct core area density",
        value = dcad$value
    )
}
