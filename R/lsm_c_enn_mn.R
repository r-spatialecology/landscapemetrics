#' ENN_MN (class level)
#'
#' @description Mean of euclidean nearest-neighbor distance (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#' @param verbose Print warning message if not sufficient patches are present
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
#' McGarigal, K., and McComb, W. C. (1995). Relationships between landscape
#' structure and breeding birds in the Oregon Coast Range.
#' Ecological monographs, 65(3), 235-260.
#'
#' @export
lsm_c_enn_mn <- function(landscape, directions, verbose) UseMethod("lsm_c_enn_mn")

#' @name lsm_c_enn_mn
#' @export
lsm_c_enn_mn.RasterLayer <- function(landscape, directions = 8, verbose = TRUE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_enn_mn_calc,
                     directions = directions,
                     verbose = verbose)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_enn_mn
#' @export
lsm_c_enn_mn.RasterStack <- function(landscape, directions = 8, verbose = TRUE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_enn_mn_calc,
                     directions = directions,
                     verbose = verbose)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_enn_mn
#' @export
lsm_c_enn_mn.RasterBrick <- function(landscape, directions = 8, verbose = TRUE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_enn_mn_calc,
                     directions = directions,
                     verbose = verbose)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_enn_mn
#' @export
lsm_c_enn_mn.stars <- function(landscape, directions = 8, verbose = TRUE) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_enn_mn_calc,
                     directions = directions,
                     verbose = verbose)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_enn_mn
#' @export
lsm_c_enn_mn.list <- function(landscape, directions = 8, verbose = TRUE) {

    result <- lapply(X = landscape,
                     FUN = lsm_c_enn_mn_calc,
                     directions = directions,
                     verbose = verbose)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}


lsm_c_enn_mn_calc <- function(landscape, directions, verbose,
                              extent = NULL, resolution = NULL, crs = NULL) {

    enn <- lsm_p_enn_calc(landscape,
                          directions = directions,
                          verbose = verbose,
                          extent = extent, resolution = resolution, crs = crs)

    enn_mn <-  dplyr::summarize(dplyr::group_by(enn, class),
                                value = mean(value))

    tibble::tibble(
        level = "class",
        class = as.integer(enn_mn$class),
        id = as.integer(NA),
        metric = "enn_mn",
        value = as.double(enn_mn$value)
    )

}
