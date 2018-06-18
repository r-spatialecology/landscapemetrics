#' Related Circumscribing Circle distribution (landscape level)
#'
#' @description Standard deviation of related circumscribing circle (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals the Standard deviation of the related circumscribing circle of all patches
#' in the landscape. Equals the 1 - the patch area (m^2) divided by the area (m^2)
#' of the smallest circumscribing circle.
#' \deqn{CIRCLE_SD = sd(CIRCLE[patch])}
#' \subsection{Units}{None}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{????}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_circle_sd(landscape)
#'
#' @aliases lsm_l_circle_sd
#' @rdname lsm_l_circle_sd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_circle_sd <- function(landscape) UseMethod("lsm_l_circle_sd")

#' @name lsm_l_circle_sd
#' @export
lsm_l_circle_sd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_circle_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_circle_sd
#' @export
lsm_l_circle_sd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_circle_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_circle_sd
#' @export
lsm_l_circle_sd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_circle_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_circle_sd
#' @export
lsm_l_circle_sd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_circle_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_circle_sd_calc <- function(landscape) {

    circle_mn <- landscape %>%
        lsm_p_circle_calc() %>%
        dplyr::summarize(value = stats::sd(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "related circumscribing circle (mean)",
        value = circle_mn$value
    )

}

