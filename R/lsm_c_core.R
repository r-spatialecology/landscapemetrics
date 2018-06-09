#'  Total area of core areas  (class level)
#'
#' @description Area of corea area (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions ...
#'
#' @return tibble
#'
#' @examples
#' lsm_c_core(landscape)
#'
#' @aliases lsm_c_core
#' @rdname lsm_c_core
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_core <- function(landscape, directions) UseMethod("lsm_c_core")

#' @name lsm_c_core
#' @export
lsm_c_core.RasterLayer <- function(landscape, directions = 4) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_core
#' @export
lsm_c_core.RasterStack <- function(landscape, directions = 4) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_core
#' @export
lsm_c_core.RasterBrick <- function(landscape, directions = 4) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_core
#' @export
lsm_c_core.list <- function(landscape, directions = 4) {
    purrr::map_dfr(landscape, lsm_c_core_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_core_calc <- function(landscape, directions){
    core_area <- landscape %>%
        lsm_p_core(directions = directions) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value))

    tibble::tibble(
        level = "class",
        class = core_area$class,
        id = as.integer(NA),
        metric = "total core area",
        value = core_area$value
    )
}
