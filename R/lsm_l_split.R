#' Splitting index (landscape level)
#'
#' @description Splitting index (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' The splitting index equals the squared total area divided by the sum of patch
#' area squared
#' \deqn{SPLIT = total area ^ 2 / sum(area[patch])}
#' \subsection{Units}{None}
#' \subsection{Range}{1 <= SPLIT <= Number of cells squared}
#' \subsection{Behaviour}{SPLIT = 1 when only one class and patch is present.
#' SPLIt increases as the number of patches increases}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_split(landscape)
#'
#' @aliases lsm_l_split
#' @rdname lsm_l_split
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_split <- function(landscape) UseMethod("lsm_l_split")

#' @name lsm_l_split
#' @export
lsm_l_split.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_split_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_split
#' @export
lsm_l_split.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_split_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_split
#' @export
lsm_l_split.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_split_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_split
#' @export
lsm_l_split.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_split_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_split_calc <- function(landscape) {

    area_landscape <- lsm_l_ta(landscape)

    split <- landscape %>%
        lsm_p_area() %>%
        dplyr::mutate(value = value ^ 2) %>%
        dplyr::summarise(value = sum(value)) %>%
        dplyr::mutate(value = (area_landscape$value ^ 2) / value)

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "splitting index",
        value = split$value
    )
}
