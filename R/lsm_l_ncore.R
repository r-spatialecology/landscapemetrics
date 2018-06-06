#' Number of core areas (landscape level)
#'
#' @description Number of disjunct core areas (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_l_ncore(landscape)
#'
#' @aliases lsm_l_ncore
#' @rdname lsm_l_ncore
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_ncore <- function(landscape) UseMethod("lsm_l_ncore")

#' @name lsm_l_ncore
#' @export
lsm_l_ncore.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ncore_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_ncore
#' @export
lsm_l_ncore.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ncore_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_ncore
#' @export
lsm_l_ncore.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ncore_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_ncore
#' @export
lsm_l_ncore.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_ncore_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_ncore_calc <- function(landscape){

    ncore <- landscape %>%
        lsm_p_ncore() %>%
        dplyr::summarise(value = sum(value))

    tibble::tibble(
        level = "landscape",
        class =  as.integer(NA),
        id = as.integer(NA),
        metric = "number of disjunct core areas",
        value = ncore$value
    )
}
