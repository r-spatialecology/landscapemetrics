#' CAI_CV (landscape level)
#'
#' @description Coefficient of variation of core area index (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
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
#' CAI_CV is a 'Core area metric'. The metric summarises the landscape
#' as the Coefficient of variation of the core area index of all patches
#' in the landscape. The core area index is the percentage of core area
#' in relation to patch area. A cell is defined as core area if the cell has
#' no neighbour with a different value than itself (rook's case). The metric
#' describes the differences among all patches in the landscape. Because it is
#' scaled to the mean, it is easily comparable.
#'
#' \subsection{Units}{Percent}
#' \subsection{Range}{CAI_CV >= 0}
#' \subsection{Behaviour}{Equals CAI_CV = 0 if the core area index is identical for
#' all patches. Increases, without limit, as the variation of the core area
#' indices increases.}
#'
#' @seealso
#' \code{\link{lsm_p_cai}},
#' \code{\link{cv}}, \cr
#' \code{\link{lsm_c_cai_mn}},
#' \code{\link{lsm_c_cai_sd}},
#' \code{\link{lsm_c_cai_cv}}, \cr
#' \code{\link{lsm_l_cai_mn}},
#' \code{\link{lsm_l_cai_sd}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_cai_cv(landscape)
#'
#' @aliases lsm_l_cai_cv
#' @rdname lsm_l_cai_cv
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_cai_cv <- function(landscape,
                              directions = 8,
                              consider_boundary = FALSE,
                              edge_depth = 1) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_cai_cv_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_cai_cv_calc <- function(landscape, directions, consider_boundary, edge_depth, resolution = NULL){

    cai_patch <- lsm_p_cai_calc(landscape,
                                directions = directions,
                                consider_boundary = consider_boundary,
                                edge_depth = edge_depth,
                                resolution = resolution)

    # all values NA
    if (all(is.na(cai_patch$value))) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "cai_cv",
                              value = as.double(NA)))
    }

    cai_cv <- raster::cv(cai_patch$value)

    return(tibble::tibble(level = "landscape",
                          class = as.integer(NA),
                          id = as.integer(NA),
                          metric = "cai_cv",
                          value = as.double(cai_cv)))
}
