#'  Perimeter area ratio  (patch level)
#'
#' @description Ratio of patch perimeter and area
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_c_frac_mn(landscape)
#' lsm_c_frac_mn(landscape_stack)
#'
#' @aliases lsm_c_frac_mn
#' @rdname lsm_c_frac_mn
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_frac_mn <- function(landscape) UseMethod("lsm_c_frac_mn")

#' @name lsm_c_frac_mn
#' @export
lsm_c_frac_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_frac_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_frac_mn
#' @export
lsm_c_frac_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_frac_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_frac_mn
#' @export
lsm_c_frac_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_frac_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_frac_mn
#' @export
lsm_c_frac_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_frac_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_frac_mn_calc <- function(landscape){

    frac_mean <- lsm_p_frac(landscape) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = mean(value))

    tibble::tibble(
        level = "patch",
        class = frac_mean$class,
        id = as.integer(NA),
        metric = "fractal dimension index (mean)",
        value = frac_mean$value
    )
}
