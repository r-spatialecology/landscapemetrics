#' Distribution number of core areas (landscape level)
#'
#' @description Standard deviation (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_l_ncore_sd(landscape)
#'
#' @aliases lsm_l_ncore_sd
#' @rdname lsm_l_ncore_sd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_ncore_sd <- function(landscape) UseMethod("lsm_l_ncore_sd")

#' @name lsm_l_ncore_sd
#' @export
lsm_l_ncore_sd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ncore_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_ncore_sd
#' @export
lsm_l_ncore_sd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ncore_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_ncore_sd
#' @export
lsm_l_ncore_sd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ncore_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_ncore_sd
#' @export
lsm_l_ncore_sd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_ncore_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_ncore_sd_calc <- function(landscape){

    ncore_sd <- landscape %>%
        lsm_p_ncore() %>%
        dplyr::summarise(value = stats::sd(value))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "number of core areas (sd)",
        value = ncore_sd$value
    )
}
