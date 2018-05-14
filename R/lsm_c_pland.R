#' Percentage of landscape (class level)
#'
#' @description Percentage of landscape (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_c_pland(landscape)
#'
#' @aliases lsm_c_pland
#' @rdname lsm_c_pland
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_pland <- function(landscape) UseMethod("lsm_c_pland")

#' @name lsm_c_pland
#' @export
lsm_c_pland.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_pland_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_pland
#' @export
lsm_c_pland.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_pland_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_pland
#' @export
lsm_c_pland.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_pland_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_pland
#' @export
lsm_c_pland.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_pland_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_pland_calc <- function(landscape){
    area <- raster::ncell(landscape) * prod(raster::res(landscape))

    landscape %>%
        raster::values() %>%
        table() %>%
        purrr::map2_dfr(.x = ., .y = 1:length(.), .f = function(x, y) {
            tibble::tibble(
                level = "class",
                id = y,
                metric = "percentage of landscape",
                value = x * prod(raster::res(landscape)) / area * 100
            )
        })
}

