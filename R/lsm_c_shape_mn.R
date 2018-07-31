#' SHAPE_MN (class level)
#'
#' @description Mean shape index (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{SHAPE_{MN} = mean(SHAPE[patch_{ij}])}
#' where \eqn{SHAPE[patch_{ij}]} is the shape index of each patch.
#'
#' SHAPE_MN is a 'Shape metric'. Each class is summarised as the mean
#' of each patch belonging to class i. SHAPE describes the ratio between the actual perimeter
#' of the patch and the hypothetical minimum perimeter of the patch. The minimum perimeter
#' equals the perimeter if the patch would be maximally compact.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{SHAPE_SD >= 1}
#' \subsection{Behaviour}{Equals SHAPE_MN = 0 if all patches are squares.
#' Increases, without limit, as the shapes of patches become more complex.}
#'
#' @seealso
#' \code{\link{lsm_p_shape}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_shape_sd}},
#' \code{\link{lsm_c_shape_cv}}, \cr
#' \code{\link{lsm_l_shape_mn}},
#' \code{\link{lsm_l_shape_sd}},
#' \code{\link{lsm_l_shape_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_shape_mn(landscape)
#'
#' @aliases lsm_c_shape_mn
#' @rdname lsm_c_shape_mn
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_shape_mn <- function(landscape, directions) UseMethod("lsm_c_shape_mn")

#' @name lsm_c_shape_mn
#' @export
lsm_c_shape_mn.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_shape_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_shape_mn
#' @export
lsm_c_shape_mn.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_shape_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_shape_mn
#' @export
lsm_c_shape_mn.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_shape_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_shape_mn
#' @export
lsm_c_shape_mn.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_c_shape_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_shape_mn_calc <- function(landscape, directions){

    shape_mn <- landscape %>%
        lsm_p_shape_calc(directions = directions) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = mean(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = as.integer(shape_mn$class),
        id = as.integer(NA),
        metric = "shape_mn",
        value = as.double(shape_mn$value)
    )
}
