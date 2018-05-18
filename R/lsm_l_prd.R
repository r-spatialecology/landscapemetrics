#' Patch richness density
#'
#' @description Number of patch types (classes) in the landscape divided by the total
#' landscape area
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return Value >= 1
#'
#' @examples
#' lsm_l_prd(landscape)
#'
#' @aliases lsm_l_prd
#' @rdname lsm_l_prd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_prd <- function(landscape) UseMethod("lsm_l_prd")

#' @name lsm_l_prd
#' @export
lsm_l_prd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_prd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_prd
#' @export
lsm_l_prd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_prd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_prd
#' @export
lsm_l_prd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_prd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_prd
#' @export
lsm_l_prd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_prd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_prd_calc <- function(landscape) {

    total_area <- landscape %>%
        lsm_l_ta()

    patch_richness <- landscape %>%
        lsm_l_pr()

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "patch richness density",
        value = patch_richness$value / total_area$value * 100
    )
}
