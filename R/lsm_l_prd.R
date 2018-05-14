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
    total_area <- raster::ncell(landscape) * prod(raster::res(landscape))

    tibble::tibble(
        level = "landscape",
        id = as.numeric(NA),
        metric = "patch richness density",
        value = length(raster::unique(landscape)) / total_area * 100
    )

}
