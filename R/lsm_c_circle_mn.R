#' Related Circumscribing Circle distribution (CIRCLE)
#'
#' @description Mean of related circumscribing circle (class)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals the mean of the related circumscribing circle of
#' class i. Equals the 1 - the patch area (m^2) divided by the area (m^2) of the smallest
#' circumscribing circle.
#' \deqn{CIRCLE_MN = mean(CIRCLE[patch_i])}
#' \subsection{Units}{None}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{????}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_circle_mn(landscape)
#'
#' @aliases lsm_c_circle_mn
#' @rdname lsm_c_circle_mn
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_circle_mn <- function(landscape) UseMethod("lsm_c_circle_mn")

#' @name lsm_c_circle_mn
#' @export
lsm_c_circle_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_circle_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_circle_mn
#' @export
lsm_c_circle_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_circle_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_circle_mn
#' @export
lsm_c_circle_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_circle_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_circle_mn
#' @export
lsm_c_circle_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_circle_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_circle_mn_calc <- function(landscape) {

    circle_mn  <- lsm_p_circle_calc(landscape) %>%
        dplyr::group_by(class)  %>%
        dplyr::summarize(value = mean(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = seq_len(nrow(circle_mn)),
        id = as.integer(NA),
        metric = "related circumscribing circle (mean)",
        value = circle_mn$value
    )

}

