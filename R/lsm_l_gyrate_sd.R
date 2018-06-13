#' Radius of Gyration Distribution (landcape level)
#'
#' @description Standard deviation of Radius of Gyration (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals coeffiecent of variation of mean distance of each cell centroid in a patch to
#' the centroid of the whole patch (mean location of all cell centroids) of all patches
#' in the landscape
#' \deqn{GYRATE_SD = sd(GYRATE[patch])}
#' \subsection{Units}{Meters}
#' \subsection{Range}{GYRATE >= 0, without limit}
#' \subsection{Behaviour}{0 if single cell, maximum if patch occupies the entire landscape}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_gyrate_sd(landscape)
#'
#' @aliases lsm_l_gyrate_sd
#' @rdname lsm_l_gyrate_sd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_gyrate_sd <- function(landscape) UseMethod("lsm_l_gyrate_sd")

#' @name lsm_l_gyrate_sd
#' @export
lsm_l_gyrate_sd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_gyrate_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_gyrate_sd
#' @export
lsm_l_gyrate_sd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_gyrate_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_gyrate_sd
#' @export
lsm_l_gyrate_sd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_gyrate_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_gyrate_sd
#' @export
lsm_l_gyrate_sd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_gyrate_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_gyrate_sd_calc <- function(landscape) {

    gyrate_sd  <- lsm_p_gyrate_calc(landscape) %>%
        dplyr::summarize(value = stats::sd(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "related circumscribing circle (mean)",
        value = gyrate_cv$value
    )

}

