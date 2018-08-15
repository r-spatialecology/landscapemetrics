#' GYRATE_MN (class level)
#'
#' @description Mean radius of gyration (Area and edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{GYRATE_{MN} = mean(GYRATE[patch_{ij}])}
#' where \eqn{GYRATE[patch_{ij}]} equals the radius of gyration of each patch.
#'
#' GYRATE_MN is an 'Area and edge metric'. The metric summarises each class
#' as the mean of the radius of gyration of all patches belonging to class i.
#' GYRATE measures the distance from each cell to the patch centroid and is based on
#' cell center-to-cell center distances. The metrics characterises
#' both the patch area and compactness. The Coefficient of variation is
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
#' \code{\link{lsm_c_gyrate_sd}},
#' \code{\link{lsm_c_gyrate_cv}}, \cr
#' \code{\link{lsm_l_gyrate_mn}},
#' \code{\link{lsm_l_gyrate_sd}},
#' \code{\link{lsm_l_gyrate_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_gyrate_mn(landscape)
#'
#' @aliases lsm_c_gyrate_mn
#' @rdname lsm_c_gyrate_mn
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' Keitt, T. H., Urban, D. L., & Milne, B. T. 1997. Detecting critical scales
#' in fragmented landscapes. Conservation ecology, 1(1).
#'
#' @export
lsm_c_gyrate_mn <- function(landscape, directions) UseMethod("lsm_c_gyrate_mn")

#' @name lsm_c_gyrate_mn
#' @export
lsm_c_gyrate_mn.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_gyrate_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_gyrate_mn
#' @export
lsm_c_gyrate_mn.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_gyrate_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_gyrate_mn
#' @export
lsm_c_gyrate_mn.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_gyrate_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_gyrate_mn
#' @export
lsm_c_gyrate_mn.stars <- function(landscape, directions = 8) {

    landscape <- methods::as(landscape, "Raster")

    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_gyrate_mn_calc,
                   directions = directions,  .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_gyrate_mn
#' @export
lsm_c_gyrate_mn.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_c_gyrate_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_gyrate_mn_calc <- function(landscape, directions) {

    gyrate_mn  <- landscape %>%
        lsm_p_gyrate_calc(directions = directions) %>%
        dplyr::group_by(class)  %>%
        dplyr::summarize(value = mean(value))

    tibble::tibble(
        level = "class",
        class = as.integer(gyrate_mn$class),
        id = as.integer(NA),
        metric = "gyrate_mn",
        value = as.double(gyrate_mn$value)
    )

}

