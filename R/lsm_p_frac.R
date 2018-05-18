#'  Perimeter area ratio  (patch level)
#'
#' @description Ratio of patch perimeter and area
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_p_frac(landscape)
#'
#' @aliases lsm_p_frac
#' @rdname lsm_p_frac
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_p_frac <- function(landscape) UseMethod("lsm_p_frac")

#' @name lsm_p_frac
#' @export
lsm_p_frac.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_frac_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_frac
#' @export
lsm_p_frac.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_frac_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_frac
#' @export
lsm_p_frac.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_frac_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_frac
#' @export
lsm_p_frac.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_p_frac_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_frac_calc <- function(landscape){

    perimeter <- lsm_p_perim(landscape)
    area <- lsm_p_area(landscape)

    frac <- 2 * log (0.25 * perimeter$value) / log(area$value)
    frac[is.na(frac)] <- 1

    tibble::tibble(
        level = "patch",
        class = perimeter$class,
        id = perimeter$id,
        metric = "fractal dimension index",
        value = frac
    )
}
