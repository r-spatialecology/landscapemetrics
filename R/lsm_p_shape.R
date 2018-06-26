#' SHAPE (patch level)
#'
#' @description Shape index (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{SHAPE = \frac{p_{ij}}{\min p_{ij}}}
#' where \eqn{p_{ij}} is the perimeter in meters and \eqn{\min p_{ij}} is the minimum
#' perimeter of the patch,
#'
#' SHAPE is a 'Shape metric'. The index is based on the patch perimeter and the patch area
#' and describes the patch complexity. Because it it standarised, it is independent
#' of the patch size.
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
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
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

    area_patch <- landscape %>%
        lsm_p_area_calc() %>%
        dplyr::mutate(value = value * 10000)

    shape <- area_patch %>%
        dplyr::mutate(n = trunc(sqrt(value)),
                      m = value - n^ 2,
                      minp = dplyr::case_when(m == 0 ~ n * 4,
                                              n ^ 2 < value & value <= n * (1 + n) ~ 4 * n + 2,
                                              value > n * (1 + n) ~ 4 * n + 4),
                      value = perimeter_patch$value / minp) %>%
        dplyr::select(-c(n, m, minp))

    tibble::tibble(
        level = "patch",
        class = as.integer(perimeter_patch$class),
        id = as.integer(perimeter_patch$id),
        metric = "shape index",
        value = as.double(shape$value)
        )


}
