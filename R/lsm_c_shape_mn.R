#' Shape index distribution  (class level)
#'
#' @description Mean of shape index (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals the mean shape index of class i. SHAPE equals a quater
#' of the patch perimeter divided by the square root of the patch area
#' \deqn{SHAPE_MN = mean(SHAPE[patch_i])}
#' \subsection{Units}{None}
#' \subsection{Range}{???}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_shape_mn(landscape)
#'
#' @aliases lsm_c_shape_mn
#' @rdname lsm_c_shape_mn
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_shape_mn <- function(landscape) UseMethod("lsm_c_shape_mn")

#' @name lsm_c_shape_mn
#' @export
lsm_c_shape_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_shape_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_shape_mn
#' @export
lsm_c_shape_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_shape_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_shape_mn
#' @export
lsm_c_shape_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_shape_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_shape_mn
#' @export
lsm_c_shape_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_shape_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_shape_mn_calc <- function(landscape){

    shape_mn <- landscape %>%
        lsm_p_shape_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = mean(value, na.rm = TRUE))

    tibble::tibble(
        level = "patch",
        class = as.integer(shape_mn$class),
        id = as.integer(NA),
        metric = "shape index (mean)",
        value = as.double(shape_mn$value)
    )
}
