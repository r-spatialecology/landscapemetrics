#' Radius of Gyration Distribution (landcape level)
#'
#' @description Mean of Radius of Gyration (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals coeffiecent of variation of mean distance of each cell centroid in a patch to
#' the centroid of the whole patch (mean location of all cell centroids) of all patches
#' in the landscape
#' \deqn{GYRATE_MN = mean(GYRATE[patch])}
#' \subsection{Units}{Meters}
#' \subsection{Range}{GYRATE >= 0, without limit}
#' \subsection{Behaviour}{0 if single cell, maximum if patch occupies the entire landscape}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_gyrate_mn(landscape)
#'
#' @aliases lsm_l_gyrate_mn
#' @rdname lsm_l_gyrate_mn
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_gyrate_mn <- function(landscape) UseMethod("lsm_l_gyrate_mn")

#' @name lsm_l_gyrate_mn
#' @export
lsm_l_gyrate_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_gyrate_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_gyrate_mn
#' @export
lsm_l_gyrate_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_gyrate_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_gyrate_mn
#' @export
lsm_l_gyrate_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_gyrate_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_gyrate_mn
#' @export
lsm_l_gyrate_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_gyrate_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_gyrate_mn_calc <- function(landscape) {

    gyrate_mn <- landscape %>%
        lsm_p_gyrate_calc() %>%
        dplyr::summarize(value = stats::sd(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "related circumscribing circle (mean)",
        value = gyrate_mn$value
    )

}

