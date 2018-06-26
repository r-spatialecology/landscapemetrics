#' Landscape shape index (landscape level)
#'
#' @description Landscape shape index (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param count_boundary ???
#'
#' @details
#' The landscape shape index equals a quarter of the sum of all edges in the landscape
#' divided by the square root of the total area.
#' \deqn{LSI = (0.25 * sum(edges[patch])) / sqrt(total area)}
#' \subsection{Units}{none}
#' \subsection{Ranges}{LSI >= 1}
#' \subsection{Behaviour}{Equals LSI = 1 when only one class and patch is present and
#' increases when the length of edges increases, i.e. the patches of class i become more complex}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_lsi(landscape)
#'
#' @aliases lsm_l_lsi
#' @rdname lsm_l_lsi
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_lsi <- function(landscape) UseMethod("lsm_l_lsi")

#' @name lsm_l_lsi
#' @export
lsm_l_lsi.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_lsi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_lsi
#' @export
lsm_l_lsi.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_lsi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_lsi
#' @export
lsm_l_lsi.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_lsi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_lsi
#' @export
lsm_l_lsi.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_lsi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_lsi_calc <- function(landscape) {

    edges_landscape <- lsm_l_te(landscape, count_boundary = TRUE)

    area_landscape <- landscape %>%
        lsm_l_ta_calc() %>%
        dplyr::mutate(value = value * 10000)

    lsi <- (0.25 * edges_landscape$value) / sqrt(area_landscape$value)

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "landscape shape index",
        value = lsi
    )
}
