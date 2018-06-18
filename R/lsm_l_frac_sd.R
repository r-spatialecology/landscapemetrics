#' Fractal dimension index distribution (landscape level)
#'
#' @description Standard deviation fractal dimension index (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Standard deviation of the fractal dimension index of all patches in the landscape.
#' FRAC equals two times the natural logarithm of a quarter of the patch area divided by the
#' natural logarithm of the patch area
#' \deqn{FRAC_SD = sd(FRAC[patch])}
#' \subsection{Units}{None}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
#' @return tibble
#'
#' @examples
#' lsm_l_frac_sd(landscape)
#'
#' @aliases lsm_l_frac_sd
#' @rdname lsm_l_frac_sd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_frac_sd <- function(landscape) UseMethod("lsm_l_frac_sd")

#' @name lsm_l_frac_sd
#' @export
lsm_l_frac_sd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_frac_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_frac_sd
#' @export
lsm_l_frac_sd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_frac_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_frac_sd
#' @export
lsm_l_frac_sd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_frac_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_frac_sd
#' @export
lsm_l_frac_sd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_frac_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_frac_sd_calc <- function(landscape){

    frac_sd <- landscape %>%
        lsm_p_frac_calc() %>%
        dplyr::summarise(value = stats::sd(value, na.rm = TRUE))

    tibble::tibble(
        level = "patch",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "fractal dimension index (sd)",
        value = frac_sd$value
    )
}
