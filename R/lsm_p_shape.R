#'  Shape index  (patch level)
#'
#' @description Perimeter divided by squareroot of area
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_p_shape(landscape)
#' lsm_p_shape(landscape_stack)
#'
#' @aliases lsm_p_shape
#' @rdname lsm_p_shape
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_p_shape <- function(landscape) UseMethod("lsm_p_shape")

#' @name lsm_p_shape
#' @export
lsm_p_shape.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_shape_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_shape
#' @export
lsm_p_shape.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_shape_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_shape
#' @export
lsm_p_shape.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_shape_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_shape
#' @export
lsm_p_shape.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_p_shape_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_shape_calc <- function(landscape){

    perimeter <- lsm_p_perim(landscape)
    area <- lsm_p_area(landscape)

    shape <- 0.25 * perimeter$value / sqrt(area$value)

    tibble::tibble(
        level = "patch",
        class = perimeter$class,
        id = perimeter$id,
        metric = "shape index",
        value = shape
    )
}
