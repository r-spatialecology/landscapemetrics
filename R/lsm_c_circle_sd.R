#' Related Circumscribing Circle (CIRCLE)
#'
#' @description Related Circumscribing Circle (class)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals the 1 - the patch area (m^2) divided by the area (m^2) of the smallest circumscribing circle.
#' \subsection{Units}{None}
#' \subsection{Range}{0 <= CIRCLE < 1}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_circle_sd(landscape)
#'
#' @aliases lsm_c_circle_sd
#' @rdname lsm_c_circle_sd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_circle_sd <- function(landscape) UseMethod("lsm_c_circle_sd")

#' @name lsm_c_circle_sd
#' @export
lsm_c_circle_sd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_circle_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_circle_sd
#' @export
lsm_c_circle_sd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_circle_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_circle_sd
#' @export
lsm_c_circle_sd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_circle_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_circle_sd
#' @export
lsm_c_circle_sd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_circle_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_circle_sd_calc <- function(landscape) {

    circle_mn  <- lsm_p_circle_calc(landscape) %>%
        dplyr::group_by(class)  %>%
        dplyr::summarize(value = stats::sd(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = seq_len(nrow(circle_mn)),
        id = as.integer(NA),
        metric = "related circumscribing circle (mean)",
        value = circle_mn$value
    )

}

