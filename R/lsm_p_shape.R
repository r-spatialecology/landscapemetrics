#' SHAPE (patch level)
#'
#' @description Shape index (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{SHAPE = \frac{p_{ij}} {\min p_{ij}}}
#' where \eqn{p_{ij}} is the perimeter in terms of cell surfaces and \eqn{\min p_{ij}}
#' is the minimum perimeter of the patch in terms of cell surfaces.
#'
#' SHAPE is a 'Shape metric'. It describes the ratio between the actual perimeter of
#' the patch and the hypothetical minimum perimeter of the patch. The minimum perimeter
#' equals the perimeter if the patch would be maximally compact.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{SHAPE >= 1}
#' \subsection{Behaviour}{Equals SHAPE = 1 for a squared patch and
#' increases, without limit, as the patch shape becomes more complex.}
#'
#' @seealso
#' \code{\link{lsm_p_perim}},
#' \code{\link{lsm_p_area}}, \cr
#' \code{\link{lsm_c_shape_mn}},
#' \code{\link{lsm_c_shape_sd}},
#' \code{\link{lsm_c_shape_cv}}, \cr
#' \code{\link{lsm_l_shape_mn}},
#' \code{\link{lsm_l_shape_sd}},
#' \code{\link{lsm_l_shape_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_p_shape(landscape)
#'
#' @aliases lsm_p_shape
#' @rdname lsm_p_shape
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_p_shape <- function(landscape) UseMethod("lsm_p_shape")

#' @name lsm_p_shape
#' @export
lsm_p_shape.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_shape_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_shape
#' @export
lsm_p_shape.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_shape_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_shape
#' @export
lsm_p_shape.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_shape_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_shape
#' @export
lsm_p_shape.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_p_shape_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_shape_calc <- function(landscape){

    perimeter_patch <- lsm_p_perim_calc(landscape)

    area_patch <- lsm_p_area_calc(landscape)

    shape_patch <- dplyr::mutate(area_patch,
                                 value = value * 10000,
                                 n = trunc(sqrt(value)),
                                 m = value - n^ 2,
                                 minp = dplyr::case_when(m == 0 ~ n * 4,
                                                         n ^ 2 < value & value <= n * (1 + n) ~ 4 * n + 2,
                                                         value > n * (1 + n) ~ 4 * n + 4),
                                 value = perimeter_patch$value / minp)

    tibble::tibble(
        level = "patch",
        class = as.integer(perimeter_patch$class),
        id = as.integer(perimeter_patch$id),
        metric = "shape",
        value = as.double(shape_patch$value)
        )


}
