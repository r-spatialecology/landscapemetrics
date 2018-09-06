#' CAI_MN (landscape level)
#'
#' @description Mean of core area index (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param consider_boundary Logical if cells that only neighbour the landscape
#' boundary should be considered as core
#'
#' @details
#' \deqn{CAI_{MN} = mean(CAI[patch_{ij}]}
#' where \eqn{CAI[patch_{ij}]} is the core area index of each patch.
#'
#' CAI_MN is a 'Core area metric'. The metric summarises the landscape
#' as the mean of the core area index of all patches in the landscape.
#' The core area index is the percentage of core area in relation to patch area.
#' A cell is defined as core area if the cell has no neighbour with a different
#' value than itself (rook's case).
#'
#' \subsection{Units}{Percent}
#' \subsection{Range}{CAI_MN >= 0}
#' \subsection{Behaviour}{Equals CAI_MN = 0 if CAI = 0 for all patches. Increases,
#' without limit, as the core area indices increase.}
#'
#' @seealso
#' \code{\link{lsm_p_cai}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_cai_sd}},
#' \code{\link{lsm_c_cai_sd}},
#' \code{\link{lsm_c_cai_cv}}, \cr
#' \code{\link{lsm_l_cai_sd}},
#' \code{\link{lsm_l_cai_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_cai_mn(landscape)
#'
#' @aliases lsm_l_cai_mn
#' @rdname lsm_l_cai_mn
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_cai_mn <- function(landscape, directions, consider_boundary) UseMethod("lsm_l_cai_mn")

#' @name lsm_l_cai_mn
#' @export
lsm_l_cai_mn.RasterLayer <- function(landscape, directions = 8, consider_boundary = FALSE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_cai_mn_calc,
                     directions = directions,
                     consider_boundary = consider_boundary)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_cai_mn
#' @export
lsm_l_cai_mn.RasterStack <- function(landscape, directions = 8, consider_boundary = FALSE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_cai_mn_calc,
                     directions = directions,
                     consider_boundary = consider_boundary)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_cai_mn
#' @export
lsm_l_cai_mn.RasterBrick <- function(landscape, directions = 8, consider_boundary = FALSE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_cai_mn_calc,
                     directions = directions,
                     consider_boundary = consider_boundary)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_cai_mn
#' @export
lsm_l_cai_mn.stars <- function(landscape, directions = 8, consider_boundary = FALSE) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_cai_mn_calc,
                     directions = directions,
                     consider_boundary = consider_boundary)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_cai_mn
#' @export
lsm_l_cai_mn.list <- function(landscape, directions = 8, consider_boundary = FALSE) {

    result <- lapply(X = landscape,
                     FUN = lsm_l_cai_mn_calc,
                     directions = directions,
                     consider_boundary = consider_boundary)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

lsm_l_cai_mn_calc <- function(landscape, directions, consider_boundary){

    cai_mean <- dplyr::summarise(lsm_p_cai(landscape,
                                           directions = directions,
                                           consider_boundary = consider_boundary),
                                 value = mean(value))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "cai_mn",
        value = as.double(cai_mean$value)
    )
}
