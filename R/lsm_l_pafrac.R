#'  Perimeter-area fractal dimension  (landscape level)
#'
#' @description 2 divided by the beta coeffient of the regression log(area) ~ log(perimeter)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_l_pafrac(landscape)
#' lsm_l_pafrac(landscape_stack)
#'
#' @aliases lsm_l_pafrac
#' @rdname lsm_l_pafrac
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_pafrac <- function(landscape) UseMethod("lsm_l_pafrac")

#' @name lsm_l_pafrac
#' @export
lsm_l_pafrac.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_pafrac_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_pafrac
#' @export
lsm_l_pafrac.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_pafrac_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_pafrac
#' @export
lsm_l_pafrac.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_pafrac_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_pafrac
#' @export
lsm_l_pafrac.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_pafrac_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_pafrac_calc <- function(landscape){

    area <- lsm_p_area(landscape)
    perimeter <- lsm_p_perim(landscape)

    regression_model <- stats::lm(log(area$value) ~ log(perimeter$value))

    pafrac = 2 / regression_model$coefficients[[2]]

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "perimeter-area fractal dimension",
        value = pafrac
    )
}
