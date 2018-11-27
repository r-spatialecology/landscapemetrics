#' CORE (patch level)
#'
#' @description Core area (Core area metric)
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
#' \deqn{CORE = a_{ij}^{core}}
#' where \eqn{a_{ij}^{core}} is the core area in square meters
#'
#' CORE is a 'Core area metric' and equals the area within a patch that is not
#' on the edge of it. A cell is defined as core area if the cell has no
#' neighbour with a different value than itself (rook's case). It describes patch area
#' and shape simultaneously (more core area when the patch is large and the shape is
#' rather compact, i.e. a square).
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{CORE >= 0}
#' \subsection{Behaviour}{Increases, without limit, as the patch area increases
#' and the patch shape simplifies (more core area). CORE = 0 when every cell in
#' the patch is an edge.}
#'
#' @seealso
#' \code{\link{lsm_c_core_mn}},
#' \code{\link{lsm_c_core_sd}},
#' \code{\link{lsm_c_core_cv}},
#' \code{\link{lsm_c_tca}}, \cr
#' \code{\link{lsm_l_core_mn}},
#' \code{\link{lsm_l_core_sd}},
#' \code{\link{lsm_l_core_cv}},
#' \code{\link{lsm_l_tca}}
#'
#' @return tibble
#'
#' @importFrom stats na.omit
#'
#' @examples
#' lsm_p_core(landscape)
#'
#' @aliases lsm_p_core
#' @rdname lsm_p_core
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_p_core <- function(landscape, directions, consider_boundary, edge_depth) UseMethod("lsm_p_core")

#' @name lsm_p_core
#' @export
lsm_p_core.RasterLayer <- function(landscape, directions = 8,
                                   consider_boundary = FALSE, edge_depth = 1) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_core_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_p_core
#' @export
lsm_p_core.RasterStack <- function(landscape, directions = 8,
                                   consider_boundary = FALSE, edge_depth = 1) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_core_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_p_core
#' @export
lsm_p_core.RasterBrick <- function(landscape, directions = 8,
                                   consider_boundary = FALSE, edge_depth = 1) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_core_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_p_core
#' @export
lsm_p_core.stars <- function(landscape, directions = 8,
                             consider_boundary = FALSE, edge_depth = 1) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_core_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_p_core
#' @export
lsm_p_core.list <- function(landscape, directions = 8,
                            consider_boundary = FALSE, edge_depth = 1) {

    result <- lapply(X = landscape,
                     FUN = lsm_p_core_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

lsm_p_core_calc <- function(landscape, directions, consider_boundary, edge_depth) {

    # get unique classes
    classes <- get_unique_values(landscape)[[1]]

    # get resolution of raster
    resolution_xy <- raster::res(landscape)

    if(consider_boundary) {
        # create empty raster for matrix_to_raster()
        landscape_empty <- raster::raster(x = raster::extent(landscape) + (2 * resolution_xy),
                                          resolution = resolution_xy,
                                          crs = raster::crs(landscape))
    }

    core <- lapply(classes, function(patches_class) {

        # get connected patches
        landscape_labeled <- get_patches(landscape,
                                         class = patches_class,
                                         directions = directions)[[1]]

        # consider landscape boundary for core definition
        if(consider_boundary) {
            # add cells around raster to consider landscape boundary
            landscape_padded <- pad_raster(landscape_labeled,
                                           pad_raster_value = NA,
                                           pad_raster_cells = 1,
                                           global = FALSE)

            # convert to back raster
            landscape_labeled <- matrix_to_raster(matrix = landscape_padded,
                                                  landscape = landscape_empty,
                                                  landscape_empty = TRUE)
        }

        # label all edge cells
        class_edge <- raster::boundaries(landscape_labeled,
                                         directions = 4)

        # count number of edge cells in each patch (edge == 1)
        cells_edge_patch <- table(raster::values(landscape_labeled)[raster::values(class_edge) == 1])

        # loop if edge_depth > 1
        if(edge_depth > 1){

            # first edge depth already labels
            for(i in seq_len(edge_depth - 1)){

                # set all already edge to NA
                raster::values(class_edge)[raster::values(class_edge) == 1] <- NA

                # set current_edge + 1 to new edge
                class_edge <- raster::boundaries(class_edge,
                                                 directions = 4)

                # count number of edge cells in each patch (edge == 1) and add to already counted edge
                cells_edge_patch <- cells_edge_patch + table(raster::values(landscape_labeled)[raster::values(class_edge) == 1])
            }
        }

        # all cells of the patch
        cells_patch <- table(raster::values(landscape_labeled))

        # all cells minus edge cells equal core and convert to ha
        core_area <- (cells_patch - cells_edge_patch) * prod(resolution_xy) / 10000

        tibble::tibble(class = patches_class,
                       value = core_area)
    })

    core <- dplyr::bind_rows(core)

    tibble::tibble(
        level = "patch",
        class = as.integer(core$class),
        id = as.integer(seq_len(nrow(core))),
        metric = "core",
        value = as.double(core$value)
    )
}

