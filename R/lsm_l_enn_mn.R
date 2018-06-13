#' Euclidean Nearest Neighbor Distance Distribution (landscape level)
#'
#' @description Mean Euclidean Nearest Neighbor Distance (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals the mean of the eclidean nearest neighbor distance of all patches in the
#' landscape. ENN equals the distance the the nearest neigbouring patch of the same
#' patch type (shortest edge-to-edge distance)
#' \deqn{ENN_MN = mean(ENN[patch])}
#' \subsection{Units}{Meters}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_enn_mn(landscape)
#'
#' @aliases lsm_l_enn_mn
#' @rdname lsm_l_enn_mn
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_enn_mn <- function(landscape) UseMethod("lsm_l_enn_mn")

#' @name lsm_l_enn_mn
#' @export
lsm_l_enn_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_enn_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_enn_mn
#' @export
lsm_l_enn_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_enn_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_enn_mn
#' @export
lsm_l_enn_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_enn_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_enn_mn
#' @export
lsm_l_enn_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_enn_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_enn_mn_calc <- function(landscape) {

    enn_mn  <- lsm_p_enn_calc(landscape) %>%
        dplyr::summarize(value = mean(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "euclidean nearest neighbor distance distribution (mean)",
        value = enn_mn$value
    )

}
