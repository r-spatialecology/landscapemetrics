#' CAI_MN (class level)
#'
#' @description Mean of core area index (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{CAI_{MN} = mean(CAI[patch_{ij}]}
#' where \eqn{CAI[patch_{ij}]} is the core area index of each patch.
#'
#' CAI_MN is a 'Core area metric'. The metric summarises each class
#' as the mean of the core area index of all patches belonging to class i.
#' The core area index is the percentage of core area in relation to patch area.
#' A cell is defined as core area if the cell has no neighbour with a different
#' value than itself (rook's case).
#'
#' \subsection{Units}{Percent}
#' \subsection{Range}{CAI_MN >= 0}
#' \subsection{Behaviour}{Equals CAI_MN = 0 if CAI = 0 for all patches.
#' Increases, without limit, as the core area indices increase.}
#'
#' @seealso
#' \code{\link{lsm_p_cai}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_cai_sd}},
#' \code{\link{lsm_c_cai_cv}}, \cr
#' \code{\link{lsm_l_cai_mn}},
#' \code{\link{lsm_l_cai_sd}},
#' \code{\link{lsm_l_cai_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_cai_mn(landscape)
#'
#' @aliases lsm_c_cai_mn
#' @rdname lsm_c_cai_mn
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_cai_mn <- function(landscape, directions) UseMethod("lsm_c_cai_mn")

#' @name lsm_c_cai_mn
#' @export
lsm_c_cai_mn.RasterLayer <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_cai_mn_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_cai_mn
#' @export
lsm_c_cai_mn.RasterStack <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_cai_mn_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_cai_mn
#' @export
lsm_c_cai_mn.RasterBrick <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_cai_mn_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_cai_mn
#' @export
lsm_c_cai_mn.stars <- function(landscape, directions = 8) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_cai_mn_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_cai_mn
#' @export
lsm_c_cai_mn.list <- function(landscape, directions = 8) {

    result <- lapply(X = landscape,
                     FUN = lsm_c_cai_mn_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

lsm_c_cai_mn_calc <- function(landscape, directions = 8){

    cai <- lsm_p_cai_calc(landscape, directions = directions)

    cai_mean <- dplyr::summarise(dplyr::group_by(cai, class),
                                 value = mean(value))

    tibble::tibble(
        level = "class",
        class = as.integer(cai_mean$class),
        id = as.integer(NA),
        metric = "cai_mn",
        value = as.double(cai_mean$value)
    )
}
