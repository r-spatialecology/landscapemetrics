#'  Perimeter area ratio  (patch level)
#'
#' @description Ratio of patch perimeter and area
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_c_para_cv(landscape)
#'
#' @aliases lsm_c_para_cv
#' @rdname lsm_c_para_cv
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_para_cv <- function(landscape) UseMethod("lsm_c_para_cv")

#' @name lsm_c_para_cv
#' @export
lsm_c_para_cv.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_para_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_para_cv
#' @export
lsm_c_para_cv.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_para_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_para_cv
#' @export
lsm_c_para_cv.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_para_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_para_cv
#' @export
lsm_c_para_cv.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_para_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_para_cv_calc <- function(landscape){

    para_cv <- lsm_p_para(landscape) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = raster::cv(value))

    tibble::tibble(
        level = "class",
        class = para_cv$class,
        id = as.integer(NA),
        metric = "perimeter-area-ratio (cv)",
        value = para_cv$value
    )
}
