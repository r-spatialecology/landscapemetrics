#'  Area of core areas  (patch level)
#'
#' @description Area of corea area
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_p_core(landscape)
#' lsm_p_core(landscape_stack)
#'
#' @aliases lsm_p_core
#' @rdname lsm_p_core
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_p_core <- function(landscape) UseMethod("lsm_p_core")

#' @name lsm_p_core
#' @export
lsm_p_core.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_core_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_core
#' @export
lsm_p_core.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_core_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_core
#' @export
lsm_p_core.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_core_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_core
#' @export
lsm_p_core.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_p_core_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_core_calc <- function(landscape){

    lsm_p_ncore(landscape) %>%
        dplyr::mutate(metric = "core area",
                      value = value * prod(raster::res(landscape))) %>%
        dplyr::select(-layer)
}
