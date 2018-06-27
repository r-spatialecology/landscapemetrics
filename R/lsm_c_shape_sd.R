#' Shape index distribution  (class level)
#'
#' @description Standard deviation of shape index (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details Equals the standard deviation of the shape index of class i. SHAPE equals a quater
#' of the patch perimeter divided by the square root of the patch area
#' \deqn{SHAPE_SD = sd(SHAPE[patch_i])}
#' \subsection{Units}{None}
#' \subsection{Range}{???}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_shape_sd(landscape)
#'
#' @aliases lsm_c_shape_sd
#' @rdname lsm_c_shape_sd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_shape_sd <- function(landscape) UseMethod("lsm_c_shape_sd")

#' @name lsm_c_shape_sd
#' @export
lsm_c_shape_sd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_shape_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_shape_sd
#' @export
lsm_c_shape_sd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_shape_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_shape_sd
#' @export
lsm_c_shape_sd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_shape_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_shape_sd
#' @export
lsm_c_shape_sd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_shape_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_shape_sd_calc <- function(landscape){

    shape_sd <- landscape %>%
        lsm_p_shape_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = stats::sd(value, na.rm = TRUE))

    tibble::tibble(
        level = "patch",
        class = as.integer(shape_sd$class),
        id = as.integer(NA),
        metric = "shape index (sd)",
        value = as.double(shape_sd$value)
    )
}
