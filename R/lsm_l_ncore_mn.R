#' Number of core areas distribution (landscape level)
#'
#' @description Mean of number of core areas (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals the mean of number of core area of all patches in the landscape.
#' A core area is a 'patch within the patch' without any edge cells. In other words,
#' the number of patches within the patch that only have neighbouring cells of the same type
#' \deqn{NCORE_MN = mean(NCORE[patch])}
#' \subsection{Units}{None}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_ncore_mn(landscape)
#'
#' @aliases lsm_l_ncore_mn
#' @rdname lsm_l_ncore_mn
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_ncore_mn <- function(landscape) UseMethod("lsm_l_ncore_mn")

#' @name lsm_l_ncore_mn
#' @export
lsm_l_ncore_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ncore_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_ncore_mn
#' @export
lsm_l_ncore_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ncore_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_ncore_mn
#' @export
lsm_l_ncore_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ncore_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_ncore_mn
#' @export
lsm_l_ncore_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_ncore_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_ncore_mn_calc <- function(landscape){

    ncore_mean <- landscape %>%
        lsm_p_ncore_calc() %>%
        dplyr::summarise(value = mean(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "number of core areas (mean)",
        value = ncore_mean$value
    )
}
