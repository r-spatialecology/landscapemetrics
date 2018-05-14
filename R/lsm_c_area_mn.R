#' Patch area distribution (class level)
#'
#' @description Mean patch size (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_c_area_mn(landscape)
#'
#' @aliases lsm_c_area_mn
#' @rdname lsm_c_area_mn
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_area_mn <- function(landscape) UseMethod("lsm_c_area_mn")

#' @name lsm_c_area_mn
#' @export
lsm_c_area_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_area_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_area_mn
#' @export
lsm_c_area_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_area_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_area_mn
#' @export
lsm_c_area_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_area_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_area_mn
#' @export
lsm_c_area_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_area_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_area_mn_calc <- function(landscape){
    landscape %>%
        cclabel() %>%
        purrr::map2_dfr(.x = ., .y = seq_along(.), .f = function(x, y){

            area_mean <- raster::values(x) %>%
                table(useNA = "no") %>%
                magrittr::multiply_by(prod(raster::res(landscape))) %>%
                mean()

            tibble::tibble(
                level = "class",
                id = as.integer(y),
                metric = "patch area (mean)",
                value = area_mean
            )
        })
}
