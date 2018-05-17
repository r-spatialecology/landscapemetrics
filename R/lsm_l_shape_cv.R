#'  Shape index  (class level)
#'
#' @description Perimeter divided by squareroot of area
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_l_shape_cv(landscape)
#' lsm_l_shape_cv(landscape_stack)
#'
#' @aliases lsm_l_shape_cv
#' @rdname lsm_l_shape_cv
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_shape_cv <- function(landscape) UseMethod("lsm_l_shape_cv")

#' @name lsm_l_shape_cv
#' @export
lsm_l_shape_cv.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_shape_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_shape_cv
#' @export
lsm_l_shape_cv.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_shape_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_shape_cv
#' @export
lsm_l_shape_cv.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_shape_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_shape_cv
#' @export
lsm_l_shape_cv.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_shape_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_shape_cv_calc <- function(landscape){

    shape_cv <- landscape %>%
        lsm_p_shape() %>%
        dplyr::summarise(value = raster::cv(value))

    tibble::tibble(
        level = "patch",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "shape index (cv)",
        value = shape_cv$value
    )
}
