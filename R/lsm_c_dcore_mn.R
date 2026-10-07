#' DCORE_MN (class level)
#'
#' @description Mean number of disjunct core areas (Core area metric)
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
#' \deqn{DCORE_{MN} = mean(NCORE[patch_{ij}])}
#' where \eqn{NCORE[patch_{ij}]} is the number of core areas.
#'
#' DCORE_MN is an 'Core area metric'. It summarises each class as the mean of all
#' patch areas belonging to class i. A cell is defined as core if the cell
#' has no neighbour with a different value than itself (rook's case). NCORE counts the disjunct
#' core areas, whereby a core area is a 'patch within the patch' containing only core cells.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{DCORE_MN > 0}
#' \subsection{Behaviour}{Equals DCORE_MN = 0 if NCORE = 0 for all patches. Increases,
#' without limit, as the number of disjunct core areas increases.}
#'
#' @seealso
#' \code{\link{lsm_p_ncore}},
#' \code{\link[base]{mean}}, \cr
#' \code{\link{lsm_c_dcore_sd}},
#' \code{\link{lsm_c_dcore_cv}}, \cr
#' \code{\link{lsm_l_dcore_mn}},
#' \code{\link{lsm_l_dcore_sd}},
#' \code{\link{lsm_l_dcore_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_dcore_mn(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_c_dcore_mn <- function(landscape, directions = 8, consider_boundary = FALSE, edge_depth = 1) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         dcore_mn <- lsm_c_dcore_mn_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             consider_boundary = consider_boundary,
                             edge_depth = edge_depth,
                             resolution = resolution
                         )

                         lsm_class_output(metric = "dcore_mn",
                                          class = as.integer(names(dcore_mn)),
                                          value = unname(dcore_mn))
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_dcore_mn_calc <- function(landscape_mat, directions, consider_boundary, edge_depth,
                                classes = NULL, class_patches = NULL, points = NULL, resolution = NULL) {

    # lazy dependency resolution
    if (is.null(classes) || is.null(class_patches) || is.null(points)) {
        deps <- resolve_extras(
            landscape_mat = landscape_mat,
            directions = directions,
            required = c("classes", "class_patches", "points"),
            resolution = resolution
        )
        classes <- deps$classes
        class_patches <- deps$class_patches
        points <- deps$points
    }

    dcore <- lsm_p_ncore_calc(
        landscape_mat = landscape_mat,
        directions = directions,
        consider_boundary = consider_boundary,
        edge_depth = edge_depth,
        resolution = NULL,
        classes = classes,
        class_patches = class_patches,
        points = points
    )

    if (all(is.na(unname(dcore)))) {
        return(stats::setNames(as.double(NA), NA_character_))
    }

    dcore_mn <- tapply(dcore, names(dcore), mean)

    # return named vector
    stats::setNames(as.double(dcore_mn), names(dcore_mn))
}
