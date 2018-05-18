#'  Perimeter area ratio  (patch level)
#'
#' @description Ratio of patch perimeter and area
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_l_para_mn(landscape)
#'
#' @aliases lsm_l_para_mn
#' @rdname lsm_l_para_mn
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_para_mn <- function(landscape) UseMethod("lsm_l_para_mn")

#' @name lsm_l_para_mn
#' @export
lsm_l_para_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_para_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_para_mn
#' @export
lsm_l_para_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_para_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_para_mn
#' @export
lsm_l_para_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_para_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_para_mn
#' @export
lsm_l_para_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_para_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_para_mn_calc <- function(landscape){

    para_mn <- lsm_p_para(landscape) %>%
        dplyr::summarise(value = mean(value))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "perimeter-area-ratio (mean)",
        value = para_mn$value
    )
}
