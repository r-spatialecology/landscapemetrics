#' Largest patch index (class level)
#'
#' @description Largest patch index of class i (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' The largest patch index equals the area of the larges patch of class i
#' divided by the total area. It is a simple measure of dominance
#' \deqn{LPI = (max(area[patch_i]) / total area) * 100}
#' \subsection{Units}{Percentage}
#' \subsection{Range}{0 < LPI <= 100}
#' \subsection{Behaviour}{LPI approaches LPI = 0 when the largest patch of the corresponding class is becoming small
#' and equals LPI = 100 when only one class and patch is present}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_lpi(landscape)
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

    total_area <- lsm_l_ta(landscape)

    lpi <- landscape %>%
        lsm_p_area() %>%
        dplyr::mutate(value = value / total_area$value * 100) %>%
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
