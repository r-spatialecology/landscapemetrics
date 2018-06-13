#' Shape index  (patch level)
#'
#' @description Shape index of patch (patch level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' The shape index equals a quater of the patch perimeter divided by the square root
#' of the patch area
#' \deqn{SHAPE = 0.25 * perimeter[patch] / sqrt(area[patch])}
#' \subsection{Units}{None}
#' \subsection{Range}{SHAPE >= 1}
#' \subsection{Behaviour}{SHAPE = 1 for a square and
#' increases as the patch becomes more irregular}
#'
#' @return tibble
#'
#' @examples
#' lsm_p_shape(landscape)
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

    area <- lsm_p_area(landscape) %>%
        dplyr::mutate(value = value * 10000)

    shape <- 0.25 * perimeter$value / sqrt(area$value)

    tibble::tibble(
        level = "patch",
        class = perimeter$class,
        id = perimeter$id,
        metric = "shape index",
        value = shape
    )
}
