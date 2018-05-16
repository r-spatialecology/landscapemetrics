#'  Perimeter area ratio  (patch level)
#'
#' @description Ratio of patch perimeter and area
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_p_para(landscape)
#' lsm_p_para(landscape_stack)
#'
#' @aliases lsm_p_para
#' @rdname lsm_p_para
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_p_para <- function(landscape) UseMethod("lsm_p_para")

#' @name lsm_p_para
#' @export
lsm_p_para.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_para_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_para
#' @export
lsm_p_para.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_para_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_para
#' @export
lsm_p_para.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_para_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_para
#' @export
lsm_p_para.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_p_para_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_para_calc <- function(landscape){

    perimeter <- lsm_p_perim(landscape)
    area <- lsm_p_area(landscape)

    tibble::tibble(
        level = "patch",
        class = perimeter$class,
        id = perimeter$id,
        metric = "perimeter-area-ratio",
        value = perimeter$value / area$value
    )
}
