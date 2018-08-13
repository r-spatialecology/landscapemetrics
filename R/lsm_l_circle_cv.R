#' CIRCLE_CV (landscape level)
#'
#' @description Coefficient of variation of related circumscribing circle (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{CIRCLE_{CV} = cv(CIRCLE[patch_{ij}])}
#' where \eqn{CIRCLE[patch_{ij}]} is the related circumscribing circle of each patch.
#'
#' CIRCLE_CV is a 'Shape metric' and summarises the landscape as the Coefficient of variation
#' of the related circumscribing circle of all patches in the landscape. CIRCLE describes
#' the ratio between the patch area and the smallest circumscribing circle of the patch
#' and characterises the compactness of the patch. CIRCLE_CV describes the differences among
#' all patches in the landscape. Because it is scaled to the mean, it is easily comparable.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{CIRCLE_CV >= 0}
#' \subsection{Behaviour}{Equals CIRCLE_CV if the related circumscribing circle is identical
#' for all patches. Increases, without limit, as the variation of related circumscribing
#' circles increases.}
#'
#' @seealso
#' \code{\link{lsm_p_circle}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_circle_mn}},
#' \code{\link{lsm_c_circle_sd}},
#' \code{\link{lsm_c_circle_cv}}, \cr
#' \code{\link{lsm_l_circle_mn}},
#' \code{\link{lsm_l_circle_sd}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_circle_cv(landscape)
#'
#' @aliases lsm_l_circle_cv
#' @rdname lsm_l_circle_cv
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' Baker, W. L., and Y. Cai. 1992. The r.le programs for multiscale analysis of
#' landscape structure using the GRASS geographical information system.
#' Landscape Ecology 7: 291-302.
#'
#' @export
lsm_l_circle_cv <- function(landscape, directions) UseMethod("lsm_l_circle_cv")

#' @name lsm_l_circle_cv
#' @export
lsm_l_circle_cv.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_circle_cv_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_circle_cv
#' @export
lsm_l_circle_cv.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_circle_cv_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_circle_cv
#' @export
lsm_l_circle_cv.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_circle_cv_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_circle_cv
#' @export
lsm_l_circle_cv.stars <- function(landscape, directions = 8) {

    landscape <- methods::as(landscape, "Raster")

    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_circle_cv_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_circle_cv
#' @export
lsm_l_circle_cv.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_l_circle_cv_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_circle_cv_calc <- function(landscape, directions) {

    circle_mn <- landscape %>%
        lsm_p_circle_calc(., directions = directions) %>%
        dplyr::summarize(value = raster::cv(value))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "circle_cv",
        value = as.double(circle_mn$value)
    )

}

