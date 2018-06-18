#' Shape index distribution  (landscape level)
#'
#' @description Standard deviation of shape index (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals the standard deviation of the shape index of all patches in the landscape.
#' SHAPE equals a quater of the patch perimeter divided by the square root of the patch area
#' \deqn{SHAPE_SD = sd(SHAPE[patch])}
#' \subsection{Units}{None}
#' \subsection{Range}{???}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_shape_sd(landscape)
#'
#' @aliases lsm_l_shape_sd
#' @rdname lsm_l_shape_sd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_shape_sd <- function(landscape) UseMethod("lsm_l_shape_sd")

#' @name lsm_l_shape_sd
#' @export
lsm_l_shape_sd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_shape_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_shape_sd
#' @export
lsm_l_shape_sd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_shape_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_shape_sd
#' @export
lsm_l_shape_sd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_shape_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_shape_sd
#' @export
lsm_l_shape_sd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_shape_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_shape_sd_calc <- function(landscape){

    shape_sd <- landscape %>%
        lsm_p_shape_calc() %>%
        dplyr::summarise(value = stats::sd(value, na.rm = TRUE))

    tibble::tibble(
        level = "patch",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "shape index (sd)",
        value = shape_sd$value
    )
}
