#' Distrubution number of core areas (class level)
#'
#' @description Standard deviation (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_c_ncore_sd(landscape)
#'
#' @aliases lsm_c_ncore_sd
#' @rdname lsm_c_ncore_sd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_ncore_sd <- function(landscape) UseMethod("lsm_c_ncore_sd")

#' @name lsm_c_ncore_sd
#' @export
lsm_c_ncore_sd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ncore_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ncore_sd
#' @export
lsm_c_ncore_sd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ncore_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_ncore_sd
#' @export
lsm_c_ncore_sd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ncore_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_ncore_sd
#' @export
lsm_c_ncore_sd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_ncore_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_ncore_sd_calc <- function(landscape){
    ncore_sd <- landscape %>%
        lsm_p_ncore() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sd(value))

    tibble::tibble(
        level = "class",
        class = ncore_sd$class,
        id = as.integer(NA),
        metric = "number of core areas (sd)",
        value = ncore_sd$value
    )
}
