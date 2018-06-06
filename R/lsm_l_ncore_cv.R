#' Patch area distribution (class level)
#'
#' @description Mean patch size (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_l_ncore_cv(landscape)
#'
#' @aliases lsm_l_ncore_cv
#' @rdname lsm_l_ncore_cv
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_ncore_cv <- function(landscape) UseMethod("lsm_l_ncore_cv")

#' @name lsm_l_ncore_cv
#' @export
lsm_l_ncore_cv.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ncore_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_ncore_cv
#' @export
lsm_l_ncore_cv.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ncore_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_ncore_cv
#' @export
lsm_l_ncore_cv.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ncore_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_ncore_cv
#' @export
lsm_l_ncore_cv.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_ncore_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_ncore_cv_calc <- function(landscape){

    ncore_cv <- landscape %>%
        lsm_p_ncore() %>%
        dplyr::summarise(value = raster::cv(value))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "core area (cv)",
        value = ncore_cv$value
    )
}
