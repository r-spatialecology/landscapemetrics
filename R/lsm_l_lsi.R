#' Landscape shape index (landscape level)
#'
#' @description Landscape shape index (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' The landscape shape index equals a quarter of the sum of all edges in the landscape
#' divided by the square root of the total area.
#' \deqn{LSI = \frac{E}{\min E}}
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

    edge_landscape <- lsm_l_te_calc(landscape, count_boundary = T)

    area_landscape <- landscape %>%
        lsm_l_ta_calc() %>%
        dplyr::mutate(value = value * 10000)

    lsi <- area_landscape %>%
        dplyr::mutate(n = trunc(sqrt(value)),
                      m = value - n^ 2,
                      minp = dplyr::case_when(m == 0 ~ n * 4,
                                              n ^ 2 < value & value <= n * (1 + n) ~ 4 * n + 2,
                                              value > n * (1 + n) ~ 4 * n + 4),
                      value = edge_landscape$value / minp) %>%
        dplyr::select(-c(n, m, minp))

    tibble::tibble(
        level = "patch",
        class = as.integer(edge_landscape$class),
        id = as.integer(edge_landscape$id),
        metric = "shape index",
        value = as.double(lsi$value)
    )

}
