#' Patch area distribution (class level)
#'
#' @description Mean patch size (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_c_core_cv(landscape)
#'
#' @aliases lsm_c_core_cv
#' @rdname lsm_c_core_cv
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_core_cv <- function(landscape) UseMethod("lsm_c_core_cv")

#' @name lsm_c_core_cv
#' @export
lsm_c_core_cv.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_core_cv
#' @export
lsm_c_core_cv.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_core_cv
#' @export
lsm_c_core_cv.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_core_cv
#' @export
lsm_c_core_cv.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_core_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_core_cv_calc <- function(landscape){

    core_cv <- landscape %>%
        lsm_p_core() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = raster::cv(value))

    tibble::tibble(
        level = "class",
        class = core_cv$class,
        id = as.integer(NA),
        metric = "core area (cv)",
        value = core_cv$value
    )
}
