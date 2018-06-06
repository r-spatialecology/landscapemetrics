#' Disjunct core area density (class level)
#'
#' @description Disjunct core area density (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_c_cai_cv(landscape)
#'
#' @aliases lsm_c_cai_cv
#' @rdname lsm_c_cai_cv
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_cai_cv <- function(landscape) UseMethod("lsm_c_cai_cv")

#' @name lsm_c_cai_cv
#' @export
lsm_c_cai_cv.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cai_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_cai_cv
#' @export
lsm_c_cai_cv.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cai_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_cai_cv
#' @export
lsm_c_cai_cv.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cai_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_cai_cv
#' @export
lsm_c_cai_cv.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_cai_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_cai_cv_calc <- function(landscape){
    ndca_sd <- landscape %>%
        lsm_p_cai() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = raster::cv(value))

    tibble::tibble(
        level = "patch",
        class = ndca_sd$class,
        id = as.integer(NA),
        metric = "core area index (cv)",
        value = ndca_sd$value
    )
}
