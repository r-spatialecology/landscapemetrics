#'  Perimeter area ratio  (patch level)
#'
#' @description Ratio of patch perimeter and area
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_l_frac_sd(landscape)
#' lsm_l_frac_sd(landscape_stack)
#'
#' @aliases lsm_l_frac_sd
#' @rdname lsm_l_frac_sd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_frac_sd <- function(landscape) UseMethod("lsm_l_frac_sd")

#' @name lsm_l_frac_sd
#' @export
lsm_l_frac_sd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_frac_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_frac_sd
#' @export
lsm_l_frac_sd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_frac_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_frac_sd
#' @export
lsm_l_frac_sd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_frac_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_frac_sd
#' @export
lsm_l_frac_sd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_frac_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_frac_sd_calc <- function(landscape){

    frac_sd <- lsm_p_frac(landscape) %>%
        dplyr::summarise(value = stats::sd(value))

    tibble::tibble(
        level = "patch",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "fractal dimension index (sd)",
        value = frac_sd$value
    )
}
