#' SHAPE (patch level)
#'
#' @description Shape index (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{SHAPE = \frac{0.25 * p_{ij}}{\sqrt{a_{ij}}}}
#' where \eqn{p_{ij}} is the perimeter in meters and \eqn{a_{ij}} is the area in
#' square meters.
#'
#' SHAPE is a 'Shape metric'. The index is based on the patch perimeter and the patch area
#' and describes the patch complexity. Because it it standarised, it is independent
#' of the patch size.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{SHAPE >= 1}
#' \subsection{Behaviour}{Approaches SHAPE = 1 for a square and
#' increases, without limit, as the patch shape becomes more complex.}
#'
#' @seealso \code{\link{lsm_p_perim}} and \code{\link{lsm_p_area}}
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

    perimeter <- lsm_p_perim_calc(landscape)

    area <- landscape %>%
        lsm_p_area_calc() %>%
        dplyr::mutate(value = value * 10000)

    shape <- 0.25 * perimeter$value / sqrt(area$value)

    tibble::tibble(
        level = "patch",
        class = perimeter$class,
        id = perimeter$id,
        metric = "shape index",
        value = shape
    )
}
