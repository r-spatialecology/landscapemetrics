#' PRD (landscape level)
#'
#' @description Patch richness density (Diversity metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{PRD = \frac{m} {A} * 10000 * 100 }
#' where \eqn{m} is the number of classes and \eqn{A} is the total landscape area in
#' square meters.
#'
#' PRD is a 'Diversity metric'. It is one of the simplest diversity and composition measures.
#' In contrast to \code{\link{lsm_l_pr}}, it is a relative measure and following, comparable
#' among landscapes with different total landscape areas.
#'
#' \subsection{Units}{Number per 100 hectares}
#' \subsection{Range}{PR > 0}
#' \subsection{Behaviour}{Approaches PRD > 1 when only one patch is present and the landscape
#' is rather large. Increases, without limit, as the number of classes increases and the
#' total landscape area decreases.}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_prd(landscape)
#'
#' @aliases lsm_l_prd
#' @rdname lsm_l_prd
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_prd <- function(landscape, directions) UseMethod("lsm_l_prd")

#' @name lsm_l_prd
#' @export
lsm_l_prd.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_prd_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_prd
#' @export
lsm_l_prd.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_prd_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_prd
#' @export
lsm_l_prd.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_prd_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_prd
#' @export
lsm_l_prd.stars <- function(landscape, directions = 8) {

    landscape <- methods::as(landscape, "Raster")

    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_prd_calc,
                   directions = directions,  .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_prd
#' @export
lsm_l_prd.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_l_prd_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_prd_calc <- function(landscape, directions) {

    area_landscape <- lsm_l_ta_calc(landscape, directions = directions)

    pr_landscape <- lsm_l_pr_calc(landscape)

    prd <- dplyr::mutate(pr_landscape,
                         value = (value / area_landscape$value) * 100)

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "prd",
        value = as.double(prd$value)
    )
}
