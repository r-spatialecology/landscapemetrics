#' CORE (patch level)
#'
#' @description Core area (Core area metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
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
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_p_core(landscape)
#'
#' @aliases lsm_p_core
#' @rdname lsm_p_core
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_p_core <- function(landscape, directions = 8,
                                   consider_boundary = FALSE, edge_depth = 1) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_p_core_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_p_core_calc <- function(landscape, directions, consider_boundary, edge_depth, resolution, extras = NULL) {

    if (missing(resolution)) resolution <- terra::res(landscape)

    if (is.null(extras)){
        metrics <- "lsm_p_core"
        landscape <- terra::as.matrix(landscape, wide = TRUE)
        extras <- prepare_extras_nonspatial(metrics, landscape = landscape,
                                            directions = directions, resolution = resolution)
    }
    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::new_tibble(list(level = rep("patch", nrow()),
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "core",
                              value = as.double(NA))))
    }

    # get common variables
    classes <- extras$classes
    class_patches <- extras$class_patches

    core <- do.call(rbind,
                    lapply(classes, function(patches_class) {

                        # get connected patches
                        landscape_labeled <- class_patches[[as.character(patches_class)]]

                        # label all edge cells
                        class_edge <- get_boundaries_calc(landscape_labeled,
                                                          edge_depth = edge_depth,
                                                          consider_boundary = consider_boundary,
                                                          as_NA = FALSE,
                                                          patch_id = FALSE)

                        # count number of edge cells in each patch (edge == 1)
                        cells_edge_patch <- table(landscape_labeled[class_edge == 1])

                        # all cells of the patch
                        cells_patch <- table(landscape_labeled)

                        # check if no cell is edge, i.e. only one patch is present
                        if (dim(cells_edge_patch) == 0) {
                            cells_edge_patch <- 0
                        }

                        # all cells minus edge cells equal core and convert to ha
                        core_area <- (cells_patch - cells_edge_patch) * prod(resolution) / 10000

                        tibble::new_tibble(list(class = rep(patches_class, length(core_area)),
                                                value = core_area))
                    })
    )

    tibble::new_tibble(list(
        level = rep("patch", nrow(core)),
        class = as.integer(core$class),
        id = as.integer(seq_len(nrow(core))),
        metric = rep("core", nrow(core)),
        value = as.double(core$value)
    ))
}

