#'  Total area of core areas  (class level)
#'
#' @description Area of corea area
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_c_tca(landscape)
#'
#' @aliases lsm_c_tca
#' @rdname lsm_c_tca
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_tca <- function(landscape, directions) UseMethod("lsm_c_tca")

#' @name lsm_c_tca
#' @export
lsm_c_tca.RasterLayer <- function(landscape, directions = 4) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_tca_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_tca
#' @export
lsm_c_tca.RasterStack <- function(landscape, directions = 4) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_tca_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_tca
#' @export
lsm_c_tca.RasterBrick <- function(landscape, directions = 4) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_tca_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_tca
#' @export
lsm_c_tca.list <- function(landscape, directions = 4) {
    purrr::map_dfr(landscape, lsm_c_tca_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_tca_calc <- function(landscape, directions){
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
