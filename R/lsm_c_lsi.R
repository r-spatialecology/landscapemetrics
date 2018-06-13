#' Landscape shape index (class level)
#'
#' @description Landscape shape index (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' The landscape shape index equals a quarter of the sum of all edges of class i
#' divided by the square root of the total area.
#' \deqn{LSI = (0.25 * sum(edges[patch_i])) / sqrt(total area)}
#' \subsection{Units}{none}
#' \subsection{Ranges}{LSI >= 1}
#' \subsection{Behaviour}{Equals LSI = 1 when only one class and patch is present and
#' increases when the length of edges increases, i.e. the patches of class i become more complex}
#'
#' @return Value >= 1
#'
#' @examples
#' lsm_c_lsi(landscape)
#'
#' @aliases lsm_c_lsi
#' @rdname lsm_c_lsi
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_c_lsi <- function(landscape) UseMethod("lsm_c_lsi")

#' @name lsm_c_lsi
#' @export
lsm_c_lsi.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_lsi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_lsi
#' @export
lsm_c_lsi.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_lsi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_lsi
#' @export
lsm_c_lsi.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_lsi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_lsi
#' @export
lsm_c_lsi.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_lsi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_lsi_calc <- function(landscape) {

    edges_class <- lsm_c_te(landscape) # Needs to include also edge to background
    area_landscape <- lsm_l_ta(landscape) %>%
        dplyr::mutate(value = value * 10000)

    lsi <- (0.25 * edges_class$value) / sqrt(area_landscape$value)

    tibble::tibble(
        level = "class",
        class = edges_class$class,
        id = as.integer(NA),
        metric = "landscape shape index",
        value = lsi
    )
}
