#' SHAPE_MN (class level)
#'
#' @description Mean shape index (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
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
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_shape_mn <- function(landscape) UseMethod("lsm_c_shape_mn")

#' @name lsm_c_shape_mn
#' @export
lsm_c_shape_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_shape_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_shape_mn
#' @export
lsm_c_shape_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_shape_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_shape_mn
#' @export
lsm_c_shape_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_shape_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_shape_mn
#' @export
lsm_c_shape_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_shape_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_shape_mn_calc <- function(landscape){

    shape_mn <- landscape %>%
        lsm_p_shape_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = mean(value, na.rm = TRUE))

    tibble::tibble(
        level = "patch",
        class = as.integer(shape_mn$class),
        id = as.integer(NA),
        metric = "shape index (mean)",
        value = as.double(shape_mn$value)
    )
}
