#' Perimeter-Area Fractal Dimension  (class level)
#'
#' @description Perimeter-Area Fractal Dimension (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' The perimeter-area fractal dimension equals two divided by the slope of
#' the regression line of the natural logrithm of patch area against the
#' natural logrithm of the patch perimeter of class i. The regression has equation
#' ln(area) = beta * ln(perimeter) + intercept. It is only meaningful
#' if the relationship between the area and perimeter is linear on a logarithmic scale.
#' \deqn{PAFRAC = 2 / (beta(ln(area[patch_i]) ~ ln(perimeter[patch_i]))}
#' \subsection{Units}{None}
#' \subsection{Range}{1 <= PAFRAC <= 2 \cr If only a few patches are present the value
#' can exceed the range. Approaches PAFRAC = 1 for patches with simples shapes and
#' PAFRAC = 2 for irregular shapes}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_pafrac(landscape)
#'
#' @aliases lsm_c_pafrac
#' @rdname lsm_c_pafrac
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_pafrac <- function(landscape) UseMethod("lsm_c_pafrac")

#' @name lsm_c_pafrac
#' @export
lsm_c_pafrac.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_pafrac_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_pafrac
#' @export
lsm_c_pafrac.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_pafrac_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_pafrac
#' @export
lsm_c_pafrac.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_pafrac_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_pafrac
#' @export
lsm_c_pafrac.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_pafrac_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_pafrac_calc <- function(landscape){

    area <- lsm_p_area(landscape)
    perimeter <- lsm_p_perim(landscape)

    patch_richness <- lsm_l_pr(landscape)

    patch_richness$value %>%
        seq_len() %>%
        purrr::map_dfr(function(x) {

            area_class <- area %>%
                dplyr::filter(class == x)

            perimeter_class <- perimeter %>%
                dplyr::filter(class == x)

            regression_model_class <- stats::lm(log(area_class$value) ~ log(perimeter_class$value))

            pafrac = 2 / regression_model_class$coefficients[[2]]

            tibble::tibble(
                level = "class",
                class = x,
                id = as.integer(NA),
                metric = "perimeter-area fractal dimension",
                value = pafrac
            )
        })
}
