#' ENN_CV (patch level)
#'
#' @description Coeffiecent of variation of euclidean nearest-neighbor distance (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{ENN_{CV} = cv(ENN[patch_{ij}])}
#' where \eqn{ENN[patch_{ij}]} is the euclidean nearest-neighbor distance
#' of each patch.
#'
#' ENN_CV is an 'Aggregation metric'. It summarises each class as the coeffiecent
#' of variation of each patch belonging to class i. ENN measures the distance to the  nearest
#' neighbouring patch of the same class i. The distance is measured from edge-to-edge.
#' The range is limited by the cell resolution on the lower limit and the landscape extent
#' on the upper limit. The metric is a simple way to describe patch isolation. Because it is
#' scaled to the mean, it is easily comparable among different landscapes.
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{ENN_CV >= 0}
#' \subsection{Behaviour}{Equals ENN_CV = 0 if the euclidean nearest-neighbor distance is
#' identical for all patches. Increases, without limit, as the variation of ENN increases.}
#'
#' @seealso
#' \code{\link{lsm_p_enn}},
#' \corde{\link{cv}}, \cr
#' \code{\link{lsm_c_enn_mn}},
#' \code{\link{lsm_c_enn_sd}}, \cr
#' \code{\link{lsm_l_enn_mn}},
#' \code{\link{lsm_l_enn_sd}},
#' \code{\link{lsm_l_enn_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_enn_cv(landscape)
#'
#' @aliases lsm_c_enn_cv
#' @rdname lsm_c_enn_cv
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_enn_cv <- function(landscape) UseMethod("lsm_c_enn_cv")

#' @name lsm_c_enn_cv
#' @export
lsm_c_enn_cv.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_enn_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_enn_cv
#' @export
lsm_c_enn_cv.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_enn_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_enn_cv
#' @export
lsm_c_enn_cv.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_enn_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_enn_cv
#' @export
lsm_c_enn_cv.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_enn_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}


lsm_c_enn_cv_calc <- function(landscape) {

    enn_cv  <- landscape %>%
        lsm_p_enn_calc() %>%
        dplyr::group_by(class)  %>%
        dplyr::summarize(value = raster::cv(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = seq_len(nrow(enn_cv)),
        id = as.integer(NA),
        metric = "euclidean nearest neighbor distance distribution (cv)",
        value = enn_cv$value
    )

}
