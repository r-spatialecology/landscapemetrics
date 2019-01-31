#' CORE_MN (landscape level)
#'
#' @description Mean of core area (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param consider_boundary Logical if cells that only neighbour the landscape
#' boundary should be considered as core
#' @param edge_depth Distance (in cells) a cell has the be away from the patch
#' edge to be considered as core cell
#'
#' @details
#' \deqn{CORE_{MN} = mean(CORE[patch_{ij}])}
#' where \eqn{CORE[patch_{ij}]} is the core area in square meters of each patch.
#'
#' CORE_MN is a 'Core area metric' and equals the mean of core areas of all patches
#' in the landscape. The core area is defined as all cells that have no
#' neighbour with a different value than themselves (rook's case).
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{CORE_MN >= 0}
#' \subsection{Behaviour}{Equals CORE_MN = 0 if CORE = 0 for all patches. Increases,
#' without limit, as the core area indices increase.}
#'
#' @seealso
#' \code{\link{lsm_p_core}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_core_mn}},
#' \code{\link{lsm_c_core_sd}},
#' \code{\link{lsm_c_core_cv}}, \cr
#' \code{\link{lsm_l_core_sd}},
#' \code{\link{lsm_l_core_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_core_mn(landscape)
#'
#' @aliases lsm_l_core_mn
#' @rdname lsm_l_core_mn
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_core_mn <- function(landscape, directions, consider_boundary, edge_depth) UseMethod("lsm_l_core_mn")

#' @name lsm_l_core_mn
#' @export
lsm_l_core_mn.RasterLayer <- function(landscape,
                                      directions = 8,
                                      consider_boundary = FALSE,
                                      edge_depth = 1) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_core_mn_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_core_mn
#' @export
lsm_l_core_mn.RasterStack <- function(landscape,
                                      directions = 8,
                                      consider_boundary = FALSE,
                                      edge_depth = 1) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_core_mn_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_core_mn
#' @export
lsm_l_core_mn.RasterBrick <- function(landscape,
                                      directions = 8,
                                      consider_boundary = FALSE,
                                      edge_depth = 1) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_core_mn_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_core_mn
#' @export
lsm_l_core_mn.stars <- function(landscape,
                                directions = 8,
                                consider_boundary = FALSE,
                                edge_depth = 1) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_core_mn_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_core_mn
#' @export
lsm_l_core_mn.list <- function(landscape,
                               directions = 8,
                               consider_boundary = FALSE,
                               edge_depth = 1) {

    result <- lapply(X = landscape,
                     FUN = lsm_l_core_mn_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_core_mn_calc <- function(landscape, directions, consider_boundary, edge_depth, resolution = NULL){

    core_mean <- dplyr::summarise(lsm_p_core_calc(landscape,
                                                  directions = directions,
                                                  consider_boundary = consider_boundary,
                                                  edge_depth = edge_depth,
                                                  resolution = resolution),
                                  value = mean(value))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "core_mn",
        value = as.double(core_mean$value)
    )
}
