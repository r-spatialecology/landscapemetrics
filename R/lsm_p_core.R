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
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
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
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         core <- lsm_p_core_calc(landscape_mat = landscape_mat,
                                                 directions = directions,
                                                 consider_boundary = consider_boundary,
                                                 edge_depth = edge_depth,
                                                 resolution = resolution)
                         lsm_patch_output(metric = "core",
                                          class = as.integer(names(core)),
                                          value = unname(core),
                                          id = seq_along(core))
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_p_core_calc <- function(landscape_mat, directions, consider_boundary, edge_depth, resolution,
                            classes = NULL, class_patches = NULL) {


    # all values NA
    if (all(is.na(landscape_mat))) {
        return(stats::setNames(as.double(NA), NA_character_))

    }

    # lazy dependency resolution
    if (is.null(classes) || is.null(class_patches)) {
        deps <- resolve_extras(
            landscape_mat = landscape_mat,
            directions = directions,
            required = c("classes", "class_patches")
        )
        classes <- deps$classes
        class_patches <- deps$class_patches
    }

    core <- do.call(c,
                    lapply(classes, function(patches_class) {

                        # get connected patches
                        landscape_labeled <- class_patches[[as.character(patches_class)]]

                        # get existing patch IDs (non-NA values)
                        patch_ids <- sort(unique(as.vector(landscape_labeled[!is.na(landscape_labeled)])))

                        # label all edge cells
                        class_edge <- get_boundaries_calc(landscape_labeled,
                                                          edge_depth = edge_depth,
                                                          consider_boundary = consider_boundary,
                                                          as_NA = FALSE,
                                                          patch_id = FALSE)

                        # count number of edge cells in each patch (edge == 1)
                        cells_edge_patch <- tabulate(landscape_labeled[class_edge == 1])

                        # all cells of the patch
                        cells_patch <- tabulate(landscape_labeled)

                        # only keep values for existing patches
                        cells_edge_patch <- cells_edge_patch[patch_ids]
                        cells_patch <- cells_patch[patch_ids]

                        # check if no cell is edge, i.e. only one patch is present
                        if (length(cells_edge_patch) == 0) {
                            cells_edge_patch <- 0
                        }

                        # all cells minus edge cells equal core and convert to ha
                        core_area <- (cells_patch - cells_edge_patch) * prod(resolution) / 10000

                        # return named vector: names are class IDs, values are core areas
                        stats::setNames(core_area, rep(as.character(patches_class), length(core_area)))
                    })
    )

    # return named vector (preserve names)
    stats::setNames(as.double(core), names(core))
}
