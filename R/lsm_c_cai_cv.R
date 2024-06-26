#' CAI_CV (class level)
#'
#' @description Coefficient of variation of core area index (Core area metric)
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
#' \deqn{CAI_{CV} = cv(CAI[patch_{ij}]}
#' where \eqn{CAI[patch_{ij}]} is the core area index of each patch.
#'
#' CAI_CV is a 'Core area metric'. The metric summarises each class
#' as the Coefficient of variation of the core area index of all patches
#' belonging to class i. The core area index is the percentage of core area
#' in relation to patch area. A cell is defined as core area if the cell has
#' no neighbour with a different value than itself (rook's case). The metric
#' describes the differences among patches of the same class i in
#' the landscape.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Percent}
#' \subsection{Range}{CAI_CV >= 0}
#' \subsection{Behaviour}{Equals CAI_CV = 0 if the core area index is identical
#' for all patches. Increases, without limit, as the variation of the core area
#' indices increases.}
#'
#' @seealso
#' \code{\link{lsm_p_cai}}, \cr
#' \code{\link{lsm_c_cai_mn}},
#' \code{\link{lsm_c_cai_sd}}, \cr
#' \code{\link{lsm_l_cai_mn}},
#' \code{\link{lsm_l_cai_sd}},
#' \code{\link{lsm_l_cai_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_cai_cv(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_c_cai_cv <- function(landscape, directions = 8, consider_boundary = FALSE, edge_depth = 1) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_cai_cv_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_cai_cv_calc <- function(landscape, directions, consider_boundary, edge_depth, resolution, extras = NULL){

    # calculate core area index for each patch
    cai <- lsm_p_cai_calc(landscape,
                          directions = directions,
                          consider_boundary = consider_boundary,
                          edge_depth = edge_depth,
                          resolution = resolution,
                          extras = extras)

    # all values NA
    if (all(is.na(cai$value))) {
        return(tibble::new_tibble(list(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "cai_cv",
                              value = as.double(NA))))
    }

    # summarise for classes
    cai_cv <- stats::aggregate(x = cai[, 5], by = cai[, 2], FUN = function(x) stats::sd(x) / mean(x) * 100)

    return(tibble::new_tibble(list(level = rep("class", nrow(cai_cv)),
                          class = as.integer(cai_cv$class),
                          id = rep(as.integer(NA), nrow(cai_cv)),
                          metric = rep("cai_cv", nrow(cai_cv)),
                          value = as.double(cai_cv$value))))
}
