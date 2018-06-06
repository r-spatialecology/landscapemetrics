#'  Core area percentage of landscape (class level)
#'
#' @description Core area of class divided by landscape area (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_c_cpland(landscape)
#'
#' @aliases lsm_c_cpland
#' @rdname lsm_c_cpland
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_cpland <- function(landscape, directions) UseMethod("lsm_c_cpland")

#' @name lsm_c_cpland
#' @export
lsm_c_cpland.RasterLayer <- function(landscape, directions = 4) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cpland_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_cpland
#' @export
lsm_c_cpland.RasterStack <- function(landscape, directions = 4) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cpland_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_cpland
#' @export
lsm_c_cpland.RasterBrick <- function(landscape, directions = 4) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cpland_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_cpland
#' @export
lsm_c_cpland.list <- function(landscape, directions = 4) {
    purrr::map_dfr(landscape, lsm_c_cpland_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_cpland_calc <- function(landscape, directions){

    total_area <- lsm_l_ta(landscape)

    cpland <- landscape %>%
        lsm_c_tca(directions = directions) %>%
        dplyr::mutate(value = value / total_area$value * 100)

    tibble::tibble(
        level = "class",
        class = cpland$class,
        id = as.integer(NA),
        metric = "core area percentage of landscape",
        value = cpland$value
    )
}
