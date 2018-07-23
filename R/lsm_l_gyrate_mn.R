#' GYRATE_MN (landscape level)
#'
#' @description Mean radius of gyration (Area and edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which cells should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{GYRATE_{MN} = mean(GYRATE[patch_{ij}])}
#' where \eqn{GYRATE[patch_{ij}]} equals the radius of gyration of each patch.
#'
#' GYRATE_MN is an 'Area and edge metric'. The metric summarises the landscape
#' as the mean of the radius of gyration of all patches in the landscape.
#' GYRATE measures the distance from each cell to the patch centroid and is based on
#' cell center-to-cell center distances. The metrics characterises
#' both the patch area and compactness. The coeffiecent of variation is
#' scaled to the mean and comparable among different landscapes.
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{GYRATE_MN >= 0 }
#' \subsection{Behaviour}{Approaches GYRATE_MN = 0 if every patch is a single cell. Increases,
#' without limit, when only one patch is present.}
#'
#' @seealso
#' \code{\link{lsm_p_gyrate}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_gyrate_mn}},
#' \code{\link{lsm_c_gyrate_sd}},
#' \code{\link{lsm_c_gyrate_cv}}, \cr
#' \code{\link{lsm_l_gyrate_sd}},
#' \code{\link{lsm_l_gyrate_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_gyrate_mn(landscape)
#'
#' @aliases lsm_l_gyrate_mn
#' @rdname lsm_l_gyrate_mn
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_gyrate_mn <- function(landscape, directions) UseMethod("lsm_l_gyrate_mn")

#' @name lsm_l_gyrate_mn
#' @export
lsm_l_gyrate_mn.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_gyrate_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_gyrate_mn
#' @export
lsm_l_gyrate_mn.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_gyrate_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_gyrate_mn
#' @export
lsm_l_gyrate_mn.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_gyrate_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_gyrate_mn
#' @export
lsm_l_gyrate_mn.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_l_gyrate_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_gyrate_mn_calc <- function(landscape, directions) {

    gyrate_mn <- landscape %>%
        lsm_p_gyrate_calc(directions = directions) %>%
        dplyr::summarize(value = mean(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "gyrate_mn",
        value = as.double(gyrate_mn$value)
    )

}

