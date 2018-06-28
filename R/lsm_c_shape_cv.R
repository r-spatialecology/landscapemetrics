#' SHAPE_CV (class level)
#'
#' @description Covariance of variation shape index (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{SHAPE_{CV} = cv(SHAPE[patch_{ij}])}
#' where \eqn{SHAPE[patch_{ij}]} is the shape index of each patch.
#'
#' SHAPE_CV is a 'Shape metric'. Each class is summarised as the coeffiecent of variation
#' of each patch belonging to class i. SHAPE describes the ratio between the actual perimeter
#' of the patch and the hypothetical minimum perimeter of the patch. The minimum perimeter
#' equals the perimeter if the patch would be maximally compact.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{SHAPE_CV >= 0}
#' \subsection{Behaviour}{Equals SHAPE_CV = 0 if all patches have an identical shape index.
#' Increases, without limit, as the variation of the shape index increases.}
#'
#' @seealso
#' \code{\link{lsm_p_shape}},
#' \code{\link{cv}}, \cr
#' \code{\link{lsm_c_shape_mn}},
#' \code{\link{lsm_c_shape_sd}}, \cr
#' \code{\link{lsm_l_shape_mn}},
#' \code{\link{lsm_l_shape_sd}},
#' \code{\link{lsm_l_shape_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_shape_cv(landscape)
#'
#' @aliases lsm_c_shape_cv
#' @rdname lsm_c_shape_cv
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_shape_cv <- function(landscape) UseMethod("lsm_c_shape_cv")

#' @name lsm_c_shape_cv
#' @export
lsm_c_shape_cv.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_shape_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_shape_cv
#' @export
lsm_c_shape_cv.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_shape_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_shape_cv
#' @export
lsm_c_shape_cv.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_shape_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_shape_cv
#' @export
lsm_c_shape_cv.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_shape_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_shape_cv_calc <- function(landscape){

    shape_cv <- landscape %>%
        lsm_p_shape_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = raster::cv(value, na.rm = TRUE))

    tibble::tibble(
        level = "patch",
        class = as.integer(shape_cv$class),
        id = as.integer(NA),
        metric = "shape index (cv)",
        value = as.double(shape_cv$value)
    )
}
