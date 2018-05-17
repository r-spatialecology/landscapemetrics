#'  Perimeter area ratio  (patch level)
#'
#' @description Ratio of patch perimeter and area
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_c_frac_cv(landscape)
#' lsm_c_frac_cv(landscape_stack)
#'
#' @aliases lsm_c_frac_cv
#' @rdname lsm_c_frac_cv
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_frac_cv <- function(landscape) UseMethod("lsm_c_frac_cv")

#' @name lsm_c_frac_cv
#' @export
lsm_c_frac_cv.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_frac_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_frac_cv
#' @export
lsm_c_frac_cv.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_frac_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_frac_cv
#' @export
lsm_c_frac_cv.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_frac_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_frac_cv
#' @export
lsm_c_frac_cv.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_frac_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_frac_cv_calc <- function(landscape){

    frac_cv <- lsm_p_frac(landscape) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = raster::cv(value))

    tibble::tibble(
        level = "patch",
        class = frac_cv$class,
        id = as.integer(NA),
        metric = "fractal dimension index (cv)",
        value = frac_cv$value
    )
}
