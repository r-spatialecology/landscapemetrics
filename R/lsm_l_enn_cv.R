#' ENN_CV (landscape level)
#'
#' @description Coefficient of variation of euclidean nearest-neighbor distance (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#' @param verbose Print warning message if not sufficient patches are present
#'
#' @details
#' \deqn{ENN_{CV} = cv(ENN[patch_{ij}])}
#' where \eqn{ENN[patch_{ij}]} is the euclidean nearest-neighbor distance
#' of each patch.
#'
#' ENN_CV is an 'Aggregation metric'. It summarises the landscape as the Coefficient
#' of variation of all patches in the landscape. ENN measures the distance to the  nearest
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
#' \code{\link{cv}}, \cr
#' \code{\link{lsm_c_enn_mn}},
#' \code{\link{lsm_c_enn_sd}},
#' \code{\link{lsm_c_enn_cv}}, \cr
#' \code{\link{lsm_l_enn_mn}},
#' \code{\link{lsm_l_enn_sd}},
#'
#' @return tibble
#'
#' @examples
#' lsm_l_enn_cv(landscape)
#'
#' @aliases lsm_l_enn_cv
#' @rdname lsm_l_enn_cv
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
lsm_l_enn_cv <- function(landscape, directions, verbose) UseMethod("lsm_l_enn_cv")

#' @name lsm_l_enn_cv
#' @export
lsm_l_enn_cv.RasterLayer <- function(landscape, directions = 8, verbose = TRUE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_enn_cv_calc,
                     directions = directions,
                     verbose = verbose)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_enn_cv
#' @export
lsm_l_enn_cv.RasterStack <- function(landscape, directions = 8, verbose = TRUE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_enn_cv_calc,
                     directions = directions,
                     verbose = verbose)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_enn_cv
#' @export
lsm_l_enn_cv.RasterBrick <- function(landscape, directions = 8, verbose = TRUE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_enn_cv_calc,
                     directions = directions,
                     verbose = verbose)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_enn_cv
#' @export
lsm_l_enn_cv.stars <- function(landscape, directions = 8, verbose = TRUE) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_enn_cv_calc,
                     directions = directions,
                     verbose = verbose)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_enn_cv
#' @export
lsm_l_enn_cv.list <- function(landscape, directions = 8, verbose = TRUE) {

    result <- lapply(X = landscape,
                     FUN = lsm_l_enn_cv_calc,
                     directions = directions,
                     verbose = verbose)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_enn_cv_calc <- function(landscape, directions, verbose,
                              points = NULL) {

    enn_cv <- dplyr::summarize(lsm_p_enn_calc(landscape,
                                              directions = directions, verbose = verbose,
                                              points = points),
                               value = raster::cv(value))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "enn_cv",
        value = as.double(enn_cv$value)
    )
}
