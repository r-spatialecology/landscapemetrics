#' Landscape division index (class level)
#'
#' @description Landscpae division index (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' The landscape division index equals 1 minus the sum of patch areas of
#' class i divided by the total area squared
#' \deqn{DIVISON = 1 - sum((area[patch_i] / total area) ^ 2)}
#' \subsection{Units}{Proportion (???)}
#' \subsection{Ranges}{0 <= Division < 1}
#' \subsection{Behaviour}{DIVISION = 1 when only one class and patch is present. Approaches
#' DIVISON = 1 when all patches of class i are single cells}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_division(landscape)
#'
#' @aliases lsm_c_division
#' @rdname lsm_c_division
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_c_division <- function(landscape) UseMethod("lsm_c_division")

#' @name lsm_c_division
#' @export
lsm_c_division.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_division_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_division
#' @export
lsm_c_division.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_division_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_division
#' @export
lsm_c_division.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_division_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_division
#' @export
lsm_c_division.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_division_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_division_calc <- function(landscape) {

    total_area <- lsm_l_ta_calc(landscape)

    division <- landscape %>%
        lsm_p_area_calc() %>%
        dplyr::mutate(value = (value / total_area$value) ^ 2) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
        dplyr::mutate(value = 1 - value)

    tibble::tibble(
        level = "class",
        class = division$class,
        id = as.integer(NA),
        metric = "landscape division index",
        value = division$value
    )
}
