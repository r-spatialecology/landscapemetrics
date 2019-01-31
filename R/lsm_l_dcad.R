#' DCAD (landscape level)
#
#' @description Disjunct core area density (core area metric)
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
#' \deqn{DCAD = (\frac{\sum \limits_{i = 1}^{m} \sum \limits_{j = 1}^{n} n_{ij}^{core}} {A}) * 10000 * 100}
#' where \eqn{n_{ij}^{core}} is the number of disjunct core areas and \eqn{A}
#' is the total landscape area in square meters.
#'
#' DCAD is a 'Core area metric'. It equals the number of disjunct core areas per
#' 100 ha relative to the total area. A disjunct core area is a 'patch within
#' the patch' containing only core cells. A cell is defined as core area if the cell has no
#' neighbour with a different value than itself (rook's case). The metric is relative and
#' therefore comparable among landscapes with different total areas.
#'
#' \subsection{Units}{Number per 100 hectares}
#' \subsection{Range}{DCAD >= 0}
#' \subsection{Behaviour}{Equals DCAD = 0 when DCORE = 0, i.e. no patch contains
#' a disjunct core area. Increases, without limit, as disjunct core areas become more
#' present, i.e. patches becoming larger and less complex.}
#'
#' @seealso
#' \code{\link{lsm_c_ndca}},
#' \code{\link{lsm_l_ta}}, \cr
#' \code{\link{lsm_c_dcad}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_dcad(landscape)
#'
#' @aliases lsm_l_dcad
#' @rdname lsm_l_dcad
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_dcad <- function(landscape, directions, consider_boundary, edge_depth) UseMethod("lsm_l_dcad")

#' @name lsm_l_dcad
#' @export
lsm_l_dcad.RasterLayer <- function(landscape,
                                   directions = 8,
                                   consider_boundary = FALSE,
                                   edge_depth = 1) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_dcad_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_dcad
#' @export
lsm_l_dcad.RasterStack <- function(landscape,
                                   directions = 8,
                                   consider_boundary = FALSE,
                                   edge_depth = 1) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_dcad_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_dcad
#' @export
lsm_l_dcad.RasterBrick <- function(landscape,
                                   directions = 8,
                                   consider_boundary = FALSE,
                                   edge_depth = 1) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_dcad_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_dcad
#' @export
lsm_l_dcad.stars <- function(landscape,
                             directions = 8,
                             consider_boundary = FALSE,
                             edge_depth = 1) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_dcad_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_dcad
#' @export
lsm_l_dcad.list <- function(landscape,
                            directions = 8,
                            consider_boundary = FALSE,
                            edge_depth = 1) {

    result <- lapply(X = landscape,
                     FUN = lsm_l_dcad_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_dcad_calc <- function(landscape, directions, consider_boundary, edge_depth,
                            resolution = NULL, points = NULL){

    # get patch area
    patch_area <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution)

    # summarise to total area
    total_area <- dplyr::summarise(patch_area, value = sum(value))

    # get core areas for each patch
    ncore_patch <- lsm_p_ncore_calc(landscape,
                                    directions = directions,
                                    consider_boundary = consider_boundary,
                                    edge_depth = edge_depth,
                                    points = points)

    # summarise for total landscape
    dcad <- dplyr::mutate(dplyr::summarise(ncore_patch, value = sum(value)),
                          value = (value / total_area$value) * 100)

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "dcad",
        value = as.double(dcad$value)
    )
}
