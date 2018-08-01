#' CIRCLE_MN (landscape level)
#'
#' @description Mean of related circumscribing circle (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{CIRCLE_{MN} = mean(CIRCLE[patch_{ij}])}
#' where \eqn{CIRCLE[patch_{ij}]} is the related circumscribing circle of each patch.
#'
#' CIRCLE_MN is a 'Shape metric' and summarises the landscape as the mean of the related
#' circumscribing circle of all patches in the landscape. CIRCLE describes
#' the ratio between the patch area and the smallest circumscribing circle of the patch
#' and characterises the compactness of the patch.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{CIRCLE_MN > 0}
#' \subsection{Behaviour}{Approaches CIRCLE_MN = 0 if the related circumscribing circle
#' of all patches is small. Increases, without limit, as the related circumscribing circles
#' increase.}
#'
#' @seealso
#' \code{\link{lsm_p_circle}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_circle_mn}},
#' \code{\link{lsm_c_circle_sd}},
#' \code{\link{lsm_c_circle_cv}}, \cr
#' \code{\link{lsm_l_circle_sd}},
#' \code{\link{lsm_l_circle_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_circle_mn(landscape)
#'
#' @aliases lsm_l_circle_mn
#' @rdname lsm_l_circle_mn
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_circle_mn <- function(landscape, directions) UseMethod("lsm_l_circle_mn")

#' @name lsm_l_circle_mn
#' @export
lsm_l_circle_mn.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_circle_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_circle_mn
#' @export
lsm_l_circle_mn.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_circle_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_circle_mn
#' @export
lsm_l_circle_mn.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_circle_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_circle_mn
#' @export
lsm_l_circle_mn.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_l_circle_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_circle_mn_calc <- function(landscape, directions) {

    circle_mn <- landscape %>%
        lsm_p_circle_calc(., directions = directions) %>%
        dplyr::summarize(value = mean(value))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "circle_mn",
        value = as.double(circle_mn$value)
    )

}

