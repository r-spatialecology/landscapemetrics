#' DCORE_MN (landscape level)
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
#' DCORE_MN is an 'Core area metric'. It summarises the landscape as the mean of all
#' patches in the landscape. A cell is defined as core if the cell has no neighbour with
#' a different value than itself (rook's case). NCORE counts the disjunct core areas,
#' whereby a core area is a 'patch within the patch' containing only core cells.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{DCORE_MN > 0}
#' \subsection{Behaviour}{Equals DCORE_MN = 0 if NCORE = 0 for all patches. Increases,
#' without limit, as the number of disjunct core areas increases.}
#'
#' @seealso
#' \code{\link{lsm_p_ncore}},
#' \code{\link[base]{mean}}, \cr
#' \code{\link{lsm_c_dcore_mn}},
#' \code{\link{lsm_c_dcore_sd}},
#' \code{\link{lsm_c_dcore_cv}}, \cr
#' \code{\link{lsm_l_dcore_sd}},
#' \code{\link{lsm_l_dcore_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_dcore_mn(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_l_dcore_mn <- function(landscape,
                                directions = 8,
                                consider_boundary = FALSE,
                                edge_depth = 1) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_dcore_mn_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_dcore_mn_calc <- function(landscape, directions, consider_boundary, edge_depth, resolution, extras = NULL){

    dcore_patch <- lsm_p_ncore_calc(landscape,
                                    directions = directions,
                                    consider_boundary = consider_boundary,
                                    edge_depth = edge_depth,
                                    resolution = resolution,
                                    extras = extras)

    # all values NA
    if (all(is.na(dcore_patch$value))) {
        return(tibble::new_tibble(list(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "dcore_mn",
                              value = as.double(NA))))
    }

    dcore_mn <- mean(dcore_patch$value)

    return(tibble::new_tibble(list(level = rep("landscape", length(dcore_mn)),
                 class = rep(as.integer(NA), length(dcore_mn)),
                 id = rep(as.integer(NA), length(dcore_mn)),
                 metric = rep("dcore_mn", length(dcore_mn)),
                 value = as.double(dcore_mn))))
}
