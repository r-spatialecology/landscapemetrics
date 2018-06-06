#' Disjunct core area density (class level)
#'
#' @description Disjunct core area density (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_l_ndca(landscape)
#'
#' @aliases lsm_l_ndca
#' @rdname lsm_l_ndca
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_ndca <- function(landscape) UseMethod("lsm_l_ndca")

#' @name lsm_l_ndca
#' @export
lsm_l_ndca.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ndca_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_ndca
#' @export
lsm_l_ndca.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ndca_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_ndca
#' @export
lsm_l_ndca.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ndca_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_ndca
#' @export
lsm_l_ndca.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_ndca_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_ndca_calc <- function(landscape){

    ndca <- landscape %>%
        lsm_c_ndca() %>%
        dplyr::summarise(value = sum(value))

    tibble::tibble(
        level = "landscape",
        class =  as.integer(NA),
        id = as.integer(NA),
        metric = "number of disjunct core areas",
        value = ndca$value
    )
}
