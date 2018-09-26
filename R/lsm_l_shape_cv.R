#' SHAPE_CV (landscape level)
#'
#' @description Covariance of variation shape index (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{SHAPE_{CV} = cv(SHAPE[patch_{ij}])}
#' where \eqn{SHAPE[patch_{ij}]} is the shape index of each patch.
#'
#' SHAPE_CV is a 'Shape metric'. The landscape is summarised as the Coefficient of variation
#' of all patches in the landscape. SHAPE describes the ratio between the actual perimeter
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
#' \code{\link{lsm_c_shape_cv}},
#' \code{\link{lsm_l_shape_mn}},
#' \code{\link{lsm_l_shape_sd}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_shape_cv(landscape)
#'
#' @aliases lsm_l_shape_cv
#' @rdname lsm_l_shape_cv
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' Patton, D. R. 1975. A diversity index for quantifying habitat "edge".
#' Wildl. Soc.Bull. 3:171-173.
#'
#' @export
lsm_l_shape_cv <- function(landscape, directions) UseMethod("lsm_l_shape_cv")

#' @name lsm_l_shape_cv
#' @export
lsm_l_shape_cv.RasterLayer <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_shape_cv_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_shape_cv
#' @export
lsm_l_shape_cv.RasterStack <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_shape_cv_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_shape_cv
#' @export
lsm_l_shape_cv.RasterBrick <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_shape_cv_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_shape_cv
#' @export
lsm_l_shape_cv.stars <- function(landscape, directions = 8) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_shape_cv_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_shape_cv
#' @export
lsm_l_shape_cv.list <- function(landscape, directions = 8) {

    result <- lapply(X = landscape,
                     FUN = lsm_l_shape_cv_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

lsm_l_shape_cv_calc <- function(landscape, directions){

    shape_cv <- dplyr::summarise(lsm_p_shape_calc(landscape, directions = directions),
                                 value = raster::cv(value))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "shape_cv",
        value = as.double(shape_cv$value)
    )
}
