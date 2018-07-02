#' ENN_MN (class level)
#'
#' @description Mean of euclidean nearest-neighbor distance (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{ENN_{MN} = mean(ENN[patch_{ij}])}
#' where \eqn{ENN[patch_{ij}]} is the euclidean nearest-neighbor distance
#' of each patch.
#'
#' ENN_MN is an 'Aggregation metric'. It summarises each class as the mean of each patch
#' belonging to class i. ENN measures the distance to the  nearest neighbouring patch
#' of the same class i. The distance is measured from edge-to-edge. The range is limited
#' by the cell resolution on the lower limit and the landscape extent on the upper limit.
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{ENN_MN > 0}
#' \subsection{Behaviour}{Approaches ENN_MN = 0 as the distance to the nearest neighbour
#' decreases, i.e. patches of the same class i are more aggregated. Increases, without limit,
#' as the distance between neighbouring patches of the same class i increases, i.e. patches are
#' more isolated.}
#'
#' @seealso
#' \code{\link{lsm_p_enn}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_enn_sd}},
#' \code{\link{lsm_c_enn_cv}}, \cr
#' \code{\link{lsm_l_enn_mn}},
#' \code{\link{lsm_l_enn_sd}},
#' \code{\link{lsm_l_enn_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_enn_mn(landscape)
#'
#' @aliases lsm_c_enn_mn
#' @rdname lsm_c_enn_mn
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_enn_mn <- function(landscape) UseMethod("lsm_c_enn_mn")

#' @name lsm_c_enn_mn
#' @export
lsm_c_enn_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_enn_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_enn_mn
#' @export
lsm_c_enn_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_enn_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_enn_mn
#' @export
lsm_c_enn_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_enn_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_enn_mn
#' @export
lsm_c_enn_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_enn_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}


lsm_c_enn_mn_calc <- function(landscape) {

    enn_mn  <- landscape %>%
        lsm_p_enn() %>%
        dplyr::group_by(class)  %>%
        dplyr::summarize(value = mean(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = as.integer(enn_mn$class),
        id = as.integer(NA),
        metric = "euclidean nearest neighbor distance distribution (mean)",
        value = as.double(enn_mn$value)
    )

}
