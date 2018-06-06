#' Disjunct core area density (class level)
#'
#' @description Disjunct core area density (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_c_ncore_mn(landscape)
#'
#' @aliases lsm_c_ncore_mn
#' @rdname lsm_c_ncore_mn
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_ncore_mn <- function(landscape) UseMethod("lsm_c_ncore_mn")

#' @name lsm_c_ncore_mn
#' @export
lsm_c_ncore_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ncore_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ncore_mn
#' @export
lsm_c_ncore_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ncore_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_ncore_mn
#' @export
lsm_c_ncore_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ncore_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_ncore_mn
#' @export
lsm_c_ncore_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_ncore_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_ncore_mn_calc <- function(landscape){
    ndca_mean <- landscape %>%
        lsm_p_ncore() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = mean(value))

    tibble::tibble(
        level = "patch",
        class = ndca_mean$class,
        id = as.integer(NA),
        metric = "number of disjunct core areas (mean)",
        value = ndca_mean$value
    )
}
