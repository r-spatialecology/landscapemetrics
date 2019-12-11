#' MESH (class level)
#'
#' @description Effective Mesh Size (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{MESH = \frac{\sum \limits_{j = 1}^{n} a_{ij} ^ 2} {A} * \frac{1} {10000}}
#' where \eqn{a_{ij}} is the patch area in square meters and \eqn{A} is the
#' total landscape area in square meters.
#'
#' The effective mesh size is an 'Aggregation metric'. Because each patch is squared
#' before the sums for each group i are calculated and the sum is standardized by the
#' total landscape area, MESH is a relative measure of patch structure. MESH is
#' perfectly, negatively correlated to \code{\link{lsm_c_division}}.
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{cell size / total area <= MESH <= total area}
#' \subsection{Behaviour}{Equals cellsize/total area if class covers only
#' one cell and equals total area if only one patch is present.}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{lsm_l_ta}}, \cr
#' \code{\link{lsm_l_mesh}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_mesh(landscape)
#'
#' @aliases lsm_c_mesh
#' @rdname lsm_c_mesh
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' Jaeger, J. A. 2000. Landscape division, splitting index, and effective mesh
#' size: new measures of landscape fragmentation.
#' Landscape ecology, 15(2), 115-130.
#'
#' @export
lsm_c_mesh <- function(landscape, directions) UseMethod("lsm_c_mesh")

#' @name lsm_c_mesh
#' @export
lsm_c_mesh.RasterLayer <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_mesh_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_c_mesh
#' @export
lsm_c_mesh.RasterStack <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_mesh_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_c_mesh
#' @export
lsm_c_mesh.RasterBrick <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_mesh_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_c_mesh
#' @export
lsm_c_mesh.stars <- function(landscape, directions = 8) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_mesh_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_c_mesh
#' @export
lsm_c_mesh.list <- function(landscape, directions = 8) {

    result <- lapply(X = landscape,
                     FUN = lsm_c_mesh_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_mesh_calc <- function(landscape, directions, resolution = NULL) {

    # get patch area
    patch_area <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution)

    # summarise to landscape area in sqm
    total_area <- sum(patch_area$value) * 10000

    # all values NA
    if (is.na(total_area)) {
        return(tibble::tibble(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "mesh",
                              value = as.double(NA)))
    }

    # calculate mesh for each patch
    patch_area$value <- (patch_area$value * 10000) ^ 2

    # summarise for each class
    mesh <- stats::aggregate(x = patch_area[, 5], by = patch_area[, 2], FUN = sum)

    # relative to total landscape area
    mesh$value <- (mesh$value / total_area) * (1 / 10000)

    return(tibble::tibble(level = "class",
                          class = as.integer(mesh$class),
                          id = as.integer(NA),
                          metric = "mesh",
                          value = as.double(mesh$value)))
}
