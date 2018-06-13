#' Perimeter-Area Fractal Dimension  (landscape level)
#'
#' @description Perimeter-Area Fractal Dimension (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' The perimeter-area fractal dimension equals two divided by the slope of
#' the regression line of the natural logrithm of patch area against the
#' natural logrithm of the patch perimeter of all patches in the landscape.
#' The regression has equation ln(area) = beta * ln(perimeter) + intercept.
#' It is only meaningful if the relationship between the area and perimeter.
#' is linear on a logarithmic scale. If there are less than 10 patches,
#' the functions returns NA because of the small-sample issue
#' \deqn{PAFRAC = 2 / (beta(ln(area[patch]) ~ ln(perimeter[patch]))}
#' \subsection{Units}{None}
#' \subsection{Range}{1 <= PAFRAC <= 2}
#' \subsection{Behaviour}{If only a few patches are present the value
#' can exceed the range. Approaches PAFRAC = 1 for patches with simples shapes and
#' PAFRAC = 2 for irregular shapes}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_pafrac(landscape)
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

    np <- lsm_l_np(landscape)

    if(np$value < 10){
        pafrac = NA
        warning("PAFRAC = NA for NP < 10")
    }
    else{
        regression_model <- stats::lm(log(area$value) ~ log(perimeter$value))
        pafrac = 2 / regression_model$coefficients[[2]]
    }

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "perimeter-area fractal dimension",
        value = pafrac
    )
}
