#' CPLAND (class level)
#'
#' @description Core area percentage of landscape (Core area metric)
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
#' \deqn{CPLAND = (\frac{\sum \limits_{j = 1}^{n} a_{ij}^{core}} {A}) * 100}
#' where \eqn{a_{ij}^{core}} is the core area in square meters and \eqn{A}
#' is the total landscape area in square meters.
#'
#' CPLAND is a 'Core area metric'. It is the percentage of core area of class i in relation to
#' the total landscape area. A cell is defined as core area if the cell has
#' no neighbour with a different value than itself (rook's case). Because CPLAND is
#' a relative measure, it is comparable among landscapes with different total areas.
#'
#' \subsection{Units}{Percentage}
#' \subsection{Range}{0 <= CPLAND < 100}
#' \subsection{Behaviour}{Approaches CPLAND = 0 if CORE = 0 for all patches. Increases as
#' the amount of core area increases, i.e. patches become larger while being rather simple
#' in shape.}
#'
#' @seealso \code{\link{lsm_p_core}} and \code{\link{lsm_l_ta}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_cpland(landscape)
#'
#' @aliases lsm_c_cpland
#' @rdname lsm_c_cpland
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_cpland <- function(landscape, directions, consider_boundary, edge_depth) UseMethod("lsm_c_cpland")

#' @name lsm_c_cpland
#' @export
lsm_c_cpland.RasterLayer <- function(landscape, directions = 8, consider_boundary = FALSE, edge_depth = 1) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_cpland_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_c_cpland
#' @export
lsm_c_cpland.RasterStack <- function(landscape, directions = 8, consider_boundary = FALSE, edge_depth = 1) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_cpland_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_c_cpland
#' @export
lsm_c_cpland.RasterBrick <- function(landscape, directions = 8, consider_boundary = FALSE, edge_depth = 1) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_cpland_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_c_cpland
#' @export
lsm_c_cpland.stars <- function(landscape, directions = 8, consider_boundary = FALSE, edge_depth = 1) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_cpland_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_c_cpland
#' @export
lsm_c_cpland.list <- function(landscape, directions = 8, consider_boundary = FALSE, edge_depth = 1) {

    result <- lapply(X = landscape,
                     FUN = lsm_c_cpland_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_cpland_calc <- function(landscape, directions, consider_boundary, edge_depth, resolution = NULL){

    # conver to matrix
    if (!methods::is(landscape, "matrix")) {
        resolution <- raster::res(landscape)

        landscape <- raster::as.matrix(landscape)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "cpland",
                              value = as.double(NA)))
    }

    # calculate patch area
    area <- lsm_p_area_calc(landscape,
                            directions = directions,
                            resolution = resolution)

    # total landscape area
    area <- sum(area$value)

    # get core area for each patch
    core_area <- lsm_p_core_calc(landscape,
                                 directions = directions,
                                 consider_boundary = consider_boundary,
                                 edge_depth = edge_depth,
                                 resolution = resolution)

    # summarise core area for classes
    core_area <- stats::aggregate(x = core_area[, 5], by = core_area[, 2], FUN = sum)

    # relative core area of each class
    core_area$value <- core_area$value / area * 100

    return(tibble::tibble(level = "class",
                          class = as.integer(core_area$class),
                          id = as.integer(NA),
                          metric = "cpland",
                          value = as.double(core_area$value)))
}
