#' COHESION (landscape level)
#'
#' @description Patch Cohesion Index (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which cells should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{COHESION = 1 - (\frac{\sum \limits_{i = 1}^{m} \sum \limits_{j = 1}^{n} p_{ij}} {\sum \limits_{i = 1}^{m} \sum \limits_{j = 1}^{n} p_{ij} \sqrt{a_{ij}}}) * (1 - \frac{1} {\sqrt{Z}}) ^ {-1} * 100}
#' where \eqn{p_{ij}} is the perimeter in meters, \eqn{a_{ij}} is the area in square
#' meters and \eqn{Z} is the number of cells.
#'
#' COHESION is an 'Aggregation metric'.
#'
#' \subsection{Units}{Percent}
#' \subsection{Ranges}{Unkown}
#' \subsection{Behaviour}{Unkown}
#'
#' @seealso
#' \code{\link{lsm_p_perim}},
#' \code{\link{lsm_p_area}}, \cr
#' \code{\link{lsm_l_cohesion}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_cohesion(landscape)
#'
#' @aliases lsm_l_cohesion
#' @rdname lsm_l_cohesion
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_cohesion <- function(landscape, directions)
    UseMethod("lsm_l_cohesion")

#' @name lsm_l_cohesion
#' @export
lsm_l_cohesion.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_cohesion_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_cohesion
#' @export
lsm_l_cohesion.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_cohesion_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_cohesion
#' @export
lsm_l_cohesion.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_cohesion_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_cohesion
#' @export
lsm_l_cohesion.list <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_cohesion_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_cohesion_calc <- function(landscape, directions) {

    ncells_landscape <- landscape %>%
        lsm_l_ta_calc(directions = directions) %>%
        dplyr::mutate(value = value * 10000 / prod(raster::res(landscape)))

    ncells_patch <- landscape %>%
        lsm_p_area_calc(., directions = directions) %>%
        dplyr::mutate(value = value * 10000 / prod(raster::res(landscape)))

    perim_patch <- landscape %>%
        lsm_p_perim_calc(., directions = directions)

    denominator <- perim_patch %>%
        dplyr::mutate(value = value * sqrt(ncells_patch$value)) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE))

    cohesion <- perim_patch %>%
        dplyr::summarise(value = sum(value)) %>%
        dplyr::mutate(
            value = (1 - (value / denominator$value)) *
                ((1 - (1 / sqrt(ncells_landscape$value))) ^ - 1) * 100
        )

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "cohesion",
        value = as.double(cohesion$value)
    )
}
