#' FRAC_CV (class level)
#'
#' @description Coeffiecent of variation fractal dimension index (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{FRAC_{CV} = cv(FRAC[patch_{ij}])}
#' where \eqn{FRAC[patch_{ij}]} equals the fractal dimension index of each patch.
#'
#' FRAC_CV is a 'Shape metric'. The metric summarises each class
#' as the coeffiecent of variation of the fractal dimension index of all patches
#' belonging to class i. The fractal dimenstion index is based on the patch perimeter and
#' the patch area and describes the patch complexity. The coeffiecent of variation is
#' scaled to the mean and comparable among different landscapes.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{FRAC_CV >= 0 }
#' \subsection{Behaviour}{Equals FRAC_CV = 0 if the fractal dimension index is identical
#' for all patches. Increases, without limit, as the variation of the fractal dimension
#' indices increases.}
#'
#' @seealso
#' \code{\link{lsm_p_frac}},
#' \code{\link{cv}}, \cre
#' \code{\link{lsm_c_frac_mn}},
#' \code{\link{lsm_c_frac_sd}}, \cr
#' \code{\link{lsm_l_frac_mn}},
#' \code{\link{lsm_l_frac_sd}},
#' \code{\link{lsm_l_frac_cv}}
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

    frac_cv <- landscape %>%
        lsm_p_frac_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = raster::cv(value, na.rm = TRUE))

    tibble::tibble(
        level = "patch",
        class = as.integer(frac_cv$class),
        id = as.integer(NA),
        metric = "fractal dimension index (cv)",
        value = as.double(frac_cv$value)
    )
}
