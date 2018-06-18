#' Radius of Gyration Distribution (class level)
#'
#' @description Mean of Radius of Gyration (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals the mean of mean distance of each cell centroid
#' in a patch to the centroid of the whole patch (mean location of all cell centroids) of class i
#' \deqn{GYRATE_MN = mean(GYRATE[patch_i]}
#' \subsection{Units}{Meters}
#' \subsection{Range}{GYRATE >= 0, without limit}
#' \subsection{Behaviour}{0 if single cell, maximum if patch occupies the entire landscape}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_gyrate_mn(landscape)
#'
#' @aliases lsm_c_gyrate_mn
#' @rdname lsm_c_gyrate_mn
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_gyrate_mn <- function(landscape) UseMethod("lsm_c_gyrate_mn")

#' @name lsm_c_gyrate_mn
#' @export
lsm_c_gyrate_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_gyrate_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_gyrate_mn
#' @export
lsm_c_gyrate_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_gyrate_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_gyrate_mn
#' @export
lsm_c_gyrate_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_gyrate_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_gyrate_mn
#' @export
lsm_c_gyrate_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_gyrate_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_gyrate_mn_calc <- function(landscape) {

    gyrate_mn  <- landscape %>%
        lsm_p_gyrate_calc() %>%
        dplyr::group_by(class)  %>%
        dplyr::summarize(value = mean(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = seq_len(nrow(gyrate_mn)),
        id = as.integer(NA),
        metric = "related circumscribing circle (mean)",
        value = gyrate_mn$value
    )

}

