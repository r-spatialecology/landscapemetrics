#' DCORE_MN (class level)
#'
#' @description Mean number of disjunct core areas (Core area metric)
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
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
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_dcore_sd}},
#' \code{\link{lsm_c_dcore_cv}}, \cr
#' \code{\link{lsm_l_dcore_mn}},
#' \code{\link{lsm_l_dcore_sd}},
#' \code{\link{lsm_l_dcore_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_dcore_mn(landscape)
#'
#' @aliases lsm_c_dcore_mn
#' @rdname lsm_c_dcore_mn
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_dcore_mn <- function(landscape, directions) UseMethod("lsm_c_dcore_mn")

#' @name lsm_c_dcore_mn
#' @export
lsm_c_dcore_mn.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_dcore_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_dcore_mn
#' @export
lsm_c_dcore_mn.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_dcore_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_dcore_mn
#' @export
lsm_c_dcore_mn.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_dcore_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_dcore_mn
#' @export
lsm_c_dcore_mn.stars <- function(landscape, directions = 8) {

    landscape <- methods::as(landscape, "Raster")

    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_dcore_mn_calc,
                   directions = directions,  .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_dcore_mn
#' @export
lsm_c_dcore_mn.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_c_dcore_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_dcore_mn_calc <- function(landscape, directions){
    dcore_mean <- landscape %>%
        lsm_p_ncore_calc(., directions = directions) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = mean(value))

    tibble::tibble(
        level = "class",
        class = as.integer(dcore_mean$class),
        id = as.integer(NA),
        metric = "dcore_mn",
        value = as.double(dcore_mean$value)
    )
}
