#' Related Circumscribing Circle distribution (landscape level)
#'
#' @description Mean of related circumscribing circle (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals the mean of the related circumscribing circle of all patches
#' in the landscape. Equals the 1 - the patch area (m^2) divided by the area (m^2)
#' of the smallest circumscribing circle.
#' \deqn{CIRCLE_MN = mean(CIRCLE[patch])}
#' \subsection{Units}{None}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{????}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_circle_mn(landscape)
#'
#' @aliases lsm_l_circle_mn
#' @rdname lsm_l_circle_mn
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_circle_mn <- function(landscape) UseMethod("lsm_l_circle_mn")

#' @name lsm_l_circle_mn
#' @export
lsm_l_circle_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_circle_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_circle_mn
#' @export
lsm_l_circle_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_circle_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_circle_mn
#' @export
lsm_l_circle_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_circle_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_circle_mn
#' @export
lsm_l_circle_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_circle_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_circle_mn_calc <- function(landscape) {

    circle_mn <- landscape %>%
        lsm_p_circle_calc() %>%
        dplyr::summarize(value = mean(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "related circumscribing circle (mean)",
        value = circle_mn$value
    )

}

