#' NCORE (patch level)
#'
#' @description Number of core areas (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, stars, or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param consider_boundary Logical if cells that only neighbour the landscape
#' boundary should be considered as core
#' @param edge_depth Distance (in cells) a cell has the be away from the patch
#' edge to be considered as core cell
#'
#' #' @details
#' \deqn{NCORE = n_{ij}^{core}}
#' where \eqn{n_{ij}^{core}} is the number of disjunct core areas.
#'
#' NCORE is a 'Core area metric'. A cell is defined as core if the cell has no
#' neighbour with a different value than itself (rook's case). The metric
#' counts the disjunct core areas, whereby a core area is a 'patch within the
#' patch' containing only core cells. It describes patch area and shape
#' simultaneously (more core area when the patch is large, however, the shape
#' must allow disjunct core areas). Thereby, a compact shape (e.g. a square)
#' will contain less disjunct core areas than a more irregular patch.
#
#' \subsection{Units}{None}
#' \subsection{Range}{NCORE >= 0}
#' \subsection{Behaviour}{NCORE = 0 when CORE = 0, i.e. every cell in patch is
#' edge. Increases, without limit, as core area increases and patch shape
#' allows disjunct core areas (i.e. patch shape becomes rather complex).}
#'
#' @seealso
#' \code{\link{lsm_c_dcore_mn}},
#' \code{\link{lsm_c_dcore_sd}},
#' \code{\link{lsm_c_dcore_cv}},
#' \code{\link{lsm_c_ndca}}, \cr
#' \code{\link{lsm_l_dcore_mn}},
#' \code{\link{lsm_l_dcore_sd}},
#' \code{\link{lsm_l_dcore_cv}},
#' \code{\link{lsm_l_ndca}}
#'
#' @return tibble
#'
#' @importFrom stats na.omit
#' @importFrom raster ncell
#'
#' @examples
#' lsm_p_ncore(landscape)
#'
#' @aliases lsm_p_ncore
#' @rdname lsm_p_ncore
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_p_ncore <- function(landscape,
                                    directions = 8,
                                    consider_boundary = FALSE,
                                    edge_depth = 1) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_p_ncore_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_p_ncore_calc <- function(landscape, directions, consider_boundary, edge_depth,
                             points = NULL){

    # conver to matrix
    if (!inherits(x = landscape, what = "matrix")) {

        # get coordinates and values of all cells
        points <- raster_to_points(landscape)[, 2:4]

        # convert to matrix
        landscape <- raster::as.matrix(landscape)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "patch",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "ncore",
                              value = as.double(NA)))
    }

    # get unique classes
    classes <- get_unique_values(landscape)[[1]]

    core_class <- do.call(rbind,
                          lapply(classes, function(patches_class) {

        # get connected patches
        landscape_labeled <- get_patches(landscape,
                                         class = patches_class,
                                         directions = directions,
                                         return_raster = FALSE)[[1]]

        # get unique patch id (must be 1 to number_patches)
        patches_id <- 1:max(landscape_labeled, na.rm = TRUE)

        # label all edge cells
        class_edge <- get_boundaries_calc(landscape_labeled,
                                          edge_depth = edge_depth,
                                          consider_boundary = consider_boundary,
                                          as_NA = FALSE,
                                          patch_id = FALSE)

        # set all edge and background to -999
        class_edge[class_edge == 1 | is.na(class_edge)] <- -999

        # no core area present
        if (max(class_edge, na.rm = TRUE) == -999) {
            result <- c(rep(0, length(patches_id)))
            names(result)  <- patches_id
        }

        else {

            # get all core patches
            patch_core <- get_patches(class_edge,
                                      class = 0,
                                      directions = directions,
                                      return_raster = FALSE)[[1]]

            # remove landscape boundary rows/cells
            if (!consider_boundary) {

                patch_core <- patch_core[-c(1, nrow(patch_core)),
                                         -c(1, ncol(patch_core))]

                landscape_labeled <- landscape_labeled[-c(1, nrow(landscape_labeled)),
                                                       -c(1, ncol(landscape_labeled))]
            }

            # transpose to get same direction of ID
            patch_core <- t(patch_core)
            landscape_labeled <- t(landscape_labeled)

            # get coordinates of current class
            points <- data.frame(x = points[which(!is.na(patch_core)), 1],
                                 y = points[which(!is.na(patch_core)), 2],
                                 z = points[which(!is.na(patch_core)), 3])

            points$core_id <- patch_core[!is.na(patch_core)]

            points$patch_id <- landscape_labeled[!is.na(patch_core)]

            n_core_area <- table(unique(points[, c(4, 5)])[, 2]) # sth breaking here

            # set up results same length as number of patches (in case patch has no core)
            result <- c(rep(0, length(patches_id)))
            names(result)  <- patches_id

            # add number of core patches if present for corresponding patch
            result[as.numeric(names(n_core_area))] <- n_core_area
        }

        tibble::tibble(
            class = patches_class,
            value = result
        )
        })
    )

    tibble::tibble(
        level = "patch",
        class = as.integer(core_class$class),
        id = as.integer(seq_len(nrow(core_class))),
        metric = "ncore",
        value = as.double(core_class$value)
    )
}
