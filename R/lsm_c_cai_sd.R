#' CAI_SD (class level)
#'
#' @description Standard deviation of core area index (Core area metric)
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
#' \deqn{CAI_{SD} = sd(CAI[patch_{ij}]}
#' where \eqn{CAI[patch_{ij}]} is the core area index of each patch.
#'
#' CAI_SD is a 'Core area metric'. The metric summarises each class
#' as the standard deviation of the core area index of all patches belonging to class i.
#' The core area index is the percentage of core area in relation to patch area.
#' A cell is defined as core area if the cell has no neighbour with a different
#' value than itself (rook's case). The metric describes the differences among patches
#' of the same class i in the landscape.
#'
#' \subsection{Units}{Percent}
#' \subsection{Range}{CAI_SD >= 0}
#' \subsection{Behaviour}{Equals CAI_SD = 0 if the core area index is identical
#' for all patches. Increases, without limit, as the variation of core area
#' indices increases.}
#'
#' @seealso
#' \code{\link{lsm_p_cai}},
#' \code{\link{sd}}, \cr
#' \code{\link{lsm_c_cai_mn}},
#' \code{\link{lsm_c_cai_cv}}, \cr
#' \code{\link{lsm_l_cai_mn}},
#' \code{\link{lsm_l_cai_sd}},
#' \code{\link{lsm_l_cai_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_cai_sd(landscape)
#'
#' @aliases lsm_c_cai_sd
#' @rdname lsm_c_cai_sd
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_cai_sd <- function(landscape, directions, consider_boundary, edge_depth) UseMethod("lsm_c_cai_sd")

#' @name lsm_c_cai_sd
#' @export
lsm_c_cai_sd.RasterLayer <- function(landscape, directions = 8, consider_boundary = FALSE, edge_depth = 1) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_cai_sd_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_c_cai_sd
#' @export
lsm_c_cai_sd.RasterStack <- function(landscape, directions = 8, consider_boundary = FALSE, edge_depth = 1) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_cai_sd_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_c_cai_sd
#' @export
lsm_c_cai_sd.RasterBrick <- function(landscape, directions = 8, consider_boundary = FALSE, edge_depth = 1) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_cai_sd_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_c_cai_sd
#' @export
lsm_c_cai_sd.stars <- function(landscape, directions = 8, consider_boundary = FALSE, edge_depth = 1) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_cai_sd_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_c_cai_sd
#' @export
lsm_c_cai_sd.list <- function(landscape, directions = 8, consider_boundary = FALSE, edge_depth = 1) {

    result <- lapply(X = landscape,
                     FUN = lsm_c_cai_sd_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_cai_sd_calc <- function(landscape, directions, consider_boundary, edge_depth){

    # calculate core area index for each patch
    cai <- lsm_p_cai_calc(landscape,
                          directions = directions,
                          consider_boundary = consider_boundary,
                          edge_depth = edge_depth)

    # all values NA
    if (all(is.na(cai$value))) {
        return(tibble::tibble(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "cai_sd",
                              value = as.double(NA)))
    }

    # summarise for classes
    cai_sd <- stats::aggregate(x = cai[, 5], by = cai[, 2], FUN = stats::sd)

    return(tibble::tibble(level = "class",
                          class = as.integer(cai_sd$class),
                          id = as.integer(NA),
                          metric = "cai_sd",
                          value = as.double(cai_sd$value)))
}
