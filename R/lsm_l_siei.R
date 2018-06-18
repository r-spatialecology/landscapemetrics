#' Simpson's evenness index (landscape level)
#'
#' @description Simpson's evenness index (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Simpson's evenness index equals 1 minus the sum of squared class proportions
#' divided by 1 minus 1 divided by the number of classes
#' \deqn{SIEI = sum(proportion[patch_i] ^ 2) / 1 - (1 / number of classes)}
#' \subsection{Units}{None}
#' \subsection{Range}{0 < SIEI <= 1}
#' \subsection{Behaviour}{Simpson's evenness index approaches SIEI = 0 when only one patch and
#' class is present and approaches SIDI = 1 when the number of class types increases while the
#' proportions are equally distributed}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_siei(landscape)
#'
#' @aliases lsm_l_siei
#' @rdname lsm_l_siei
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_siei <- function(landscape) UseMethod("lsm_l_siei")

#' @name lsm_l_siei
#' @export
lsm_l_siei.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_siei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_siei
#' @export
lsm_l_siei.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_siei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_siei
#' @export
lsm_l_siei.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_siei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_siei
#' @export
lsm_l_siei.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_siei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_siei_calc <- function(landscape) {

    sidi <- lsm_l_sidi_calc(landscape)
    pr <- lsm_l_pr_calc(landscape)

    siei <- sidi$value / (1 - (1 / pr$value))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "Simpson's evenness index",
        value = siei
    )
}
