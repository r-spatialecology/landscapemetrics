#' Largest patch index (class level)
#'
#' @description Area of the larges patch divided by the total area (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_c_lpi(landscape)
#' lsm_c_lpi(landscape_stack)
#'
#' @aliases lsm_c_lpi
#' @rdname lsm_c_lpi
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_c_lpi <- function(landscape) UseMethod("lsm_c_lpi")

#' @name lsm_c_lpi
#' @export
lsm_c_lpi.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_lpi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_lpi
#' @export
lsm_c_lpi.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_lpi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_lpi
#' @export
lsm_c_lpi.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_lpi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_lpi
#' @export
lsm_c_lpi.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_lpi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_lpi_calc <- function(landscape) {

    lpi <- landscape %>%
        lsm_p_area() %>%
        dplyr::mutate(ta = sum(value),
                      value = value / ta * 100) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = max(value))

    tibble::tibble(
        level = "class",
        class = lpi$class,
        id = as.integer(NA),
        metric = "largest patch index",
        value = lpi$value
    )
}
