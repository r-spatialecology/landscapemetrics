#'  Fractal dimension index distribution (class level)
#'
#' @description Coeffiecent of variation fractal dimension index (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Coeffiecent of variation of the fractal dimension index of class i. FRAC equals
#' two times the natural logarithm of a quarter of the patch area divided by the
#' natural logarithm of the patch area
#' \deqn{FRAC_CV = cv(FRAC[patch_i])
#' \subsection{Units}{None}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_frac_cv(landscape)
#'
#' @aliases lsm_c_frac_cv
#' @rdname lsm_c_frac_cv
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_frac_cv <- function(landscape) UseMethod("lsm_c_frac_cv")

#' @name lsm_c_frac_cv
#' @export
lsm_c_frac_cv.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_frac_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_frac_cv
#' @export
lsm_c_frac_cv.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_frac_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_frac_cv
#' @export
lsm_c_frac_cv.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_frac_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_frac_cv
#' @export
lsm_c_frac_cv.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_frac_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_frac_cv_calc <- function(landscape){

    frac_cv <- lsm_p_frac(landscape) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = raster::cv(value))

    tibble::tibble(
        level = "patch",
        class = frac_cv$class,
        id = as.integer(NA),
        metric = "fractal dimension index (cv)",
        value = frac_cv$value
    )
}
