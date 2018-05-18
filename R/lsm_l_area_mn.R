#' Patch area distribution (landscape level)
#'
#' @description Mean patch size (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_l_area_mn(landscape)
#'
#' @aliases lsm_l_area_mn
#' @rdname lsm_l_area_mn
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_area_mn <- function(landscape) UseMethod("lsm_l_area_mn")

#' @name lsm_l_area_mn
#' @export
lsm_l_area_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_area_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_area_mn
#' @export
lsm_l_area_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_area_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_area_mn
#' @export
lsm_l_area_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_area_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_area_mn
#' @export
lsm_l_area_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_area_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

# Not working yet!
lsm_l_area_mn_calc <- function(landscape){
    area_mean <- landscape %>%
        lsm_p_area() %>%
        dplyr::summarise(value = mean(value))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "patch area (mean)",
        value = area_mean$value
    )
}


