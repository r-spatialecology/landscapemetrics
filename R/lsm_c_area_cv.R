#' Patch area distribution (class level)
#'
#' @description Coefficient of variation (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_c_area_cv(landscape)
#'
#' @aliases lsm_c_area_cv
#' @rdname lsm_c_area_cv
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_area_cv <- function(landscape) UseMethod("lsm_c_area_cv")

#' @name lsm_c_area_cv
#' @export
lsm_c_area_cv.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_area_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_area_cv
#' @export
lsm_c_area_cv.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_area_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_area_cv
#' @export
lsm_c_area_cv.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_area_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_area_cv
#' @export
lsm_c_area_cv.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_area_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_area_cv_calc <- function(landscape){
    landscape %>%
        cclabel() %>%
        purrr::map2_dfr(.x = ., .y = seq_along(.), .f = function(x, y){

            area_cv <- raster::values(x) %>%
                table(useNA = "no") %>%
                magrittr::multiply_by(prod(raster::res(landscape))) %>%
                cv()

            tibble::tibble(
                level = "class",
                id = as.integer(y),
                metric = "patch area (cv)",
                value = area_cv
            )
        })
}
