#' Largest patch index (landscape level)
#'
#' @description Area of the larges patch divided by the total area (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_l_lpi(landscape)
#'
#' @aliases lsm_l_lpi
#' @rdname lsm_l_lpi
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_lpi <- function(landscape) UseMethod("lsm_l_lpi")

#' @name lsm_l_lpi
#' @export
lsm_l_lpi.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_lpi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_lpi
#' @export
lsm_l_lpi.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_lpi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_lpi
#' @export
lsm_l_lpi.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_lpi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_lpi
#' @export
lsm_l_lpi.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_lpi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_lpi_calc <- function(landscape) {

    lpi <- landscape %>%
        lsm_p_area() %>%
        dplyr::mutate(total_area = sum(value),
                      lpi = value / total_area * 100) %>%
        dplyr::summarise(value = max(lpi))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "largest patch index",
        value = lpi$value
    )
}
