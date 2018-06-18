#' Splitting index (class level)
#'
#' @description Splitting index (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' The splitting index equals the squared total area divided by the sum of patch
#' area squared of class i
#' \deqn{SPLIT = total area ^ 2 / sum(area[patch_i])}
#' \subsection{Units}{None}
#' \subsection{Range}{1 <= SPLIT <= Number of cells squared}
#' \subsection{Behaviour}{SPLIT = 1 when only one class and patch is present.
#' SPLIt increases as the number of patches of class i increases}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_split(landscape)
#'
#' @aliases lsm_c_split
#' @rdname lsm_c_split
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_c_split <- function(landscape) UseMethod("lsm_c_split")

#' @name lsm_c_split
#' @export
lsm_c_split.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_split_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_split
#' @export
lsm_c_split.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_split_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_split
#' @export
lsm_c_split.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_split_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_split
#' @export
lsm_c_split.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_split_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_split_calc <- function(landscape) {

    area_landscape <- lsm_l_ta_calc(landscape)

    split <- landscape %>%
        lsm_p_area_calc() %>%
        dplyr::mutate(value = value ^ 2) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
        dplyr::mutate(value = (area_landscape$value ^ 2) / value)

    tibble::tibble(
        level = "class",
        class = split$class,
        id = as.integer(NA),
        metric = "splitting index",
        value = split$value
    )
}
