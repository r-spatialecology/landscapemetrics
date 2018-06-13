#' Largest patch index (landscape level)
#'
#' @description Largest patch index of the whole landscape (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' The largest patch index equals the area of the larges patch of the whole landscape
#' divided by the total area. It is a simple measure of dominance
#' \deqn{LPI = (max(area[patch]) / total area) * 100}
#' \subsection{Units}{Percentage}
#' \subsection{Range}{0 < LPI <= 100}
#' \subsection{Behaviour}{LPI approaches LPI = 0 when the largest patch of the corresponding class is becoming small
#' and equals LPI = 100 when only one class and patch is present}
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

    area_landscape <- lsm_l_ta(landscape)

    lpi <- landscape %>%
        lsm_p_area() %>%
        dplyr::mutate(lpi = value / area_landscape$value * 100) %>%
        dplyr::summarise(value = max(lpi))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "largest patch index",
        value = lpi$value
    )
}
