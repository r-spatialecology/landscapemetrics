#'  Total area of core areas (landscape level)
#'
#' @description Area of corea area (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_l_core(landscape)
#'
#' @aliases lsm_l_core
#' @rdname lsm_l_core
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_core <- function(landscape) UseMethod("lsm_l_core")


#' @name lsm_l_core
#' @export
lsm_l_core.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_core_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_core
#' @export
lsm_l_core.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_core_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_core
#' @export
lsm_l_core.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_core_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_core
#' @export
lsm_l_core.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_core_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_core_calc <- function(landscape) {

    total_core_area <- landscape %>%
        lsm_p_core() %>%
        dplyr::summarise(value = sum(value))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "total core area",
        value = total_core_area$value
    )
}
