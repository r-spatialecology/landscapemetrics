#' Patch density (class level)
#'
#' @description Patch density (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return Value >= 1
#'
#' @examples
#' lsm_c_pd(landscape)
#'
#' @aliases lsm_c_pd
#' @rdname lsm_c_pd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_c_pd <- function(landscape) UseMethod("lsm_c_pd")

#' @name lsm_c_pd
#' @export
lsm_c_pd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_pd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_pd
#' @export
lsm_c_pd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_pd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_pd
#' @export
lsm_c_pd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_pd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_pd
#' @export
lsm_c_pd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_pd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_pd_calc <- function(landscape) {

    total_area <- lsm_l_ta(landscape)

    patch_density <- landscape %>%
        lsm_c_np() %>%
        dplyr::mutate(value = (value / total_area$value) * 10000 * 100)

    tibble::tibble(
        level = "class",
        class = patch_density$class,
        id = as.integer(NA),
        metric = "patch density",
        value = patch_density$value
    )
}
