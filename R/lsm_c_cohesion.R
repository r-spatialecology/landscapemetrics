#' COHESION (class level)
#'
#' @description Patch Cohesion Index (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which cells should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{COHESION = 1 - (\frac{\sum \limits_{j = 1}^{n} p_{ij}} {\sum \limits_{j = 1}^{n} p_{ij} \sqrt{a_{ij}}}) * (1 - \frac{1} {\sqrt{Z}}) ^ {-1} * 100}
#' where \eqn{p_{ij}} is the perimeter in meters, \eqn{a_{ij}} is the area in square
#' meters and \eqn{Z} is the number of cells.
#'
#' COHESION is an 'Aggregation metric'. It characterises the conncectedness of patches
#' belonging to class i. It can be used to asses if patches of the same class are located
#' aggregated or rather isolated and thereby COHESION gives information about the
#' configuration of the landscape.
#'
#' \subsection{Units}{Percent}
#' \subsection{Ranges}{0 < COHESION < 100}
#' \subsection{Behaviour}{Approaches COHESION = 0 if patches of class i become more isolated.
#' Increases if patches of class i become more aggregated.}
#'
#' @seealso
#' \code{\link{lsm_p_perim}},
#' \code{\link{lsm_p_area}}, \cr
#' \code{\link{lsm_l_cohesion}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_cohesion(landscape)
#'
#' @aliases lsm_c_cohesion
#' @rdname lsm_c_cohesion
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_cohesion <- function(landscape, directions)
    UseMethod("lsm_c_cohesion")

#' @name lsm_c_cohesion
#' @export
lsm_c_cohesion.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_cohesion_calc, directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_cohesion
#' @export
lsm_c_cohesion.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_cohesion_calc, directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_cohesion
#' @export
lsm_c_cohesion.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_cohesion_calc, directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_cohesion
#' @export
lsm_c_cohesion.list <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_cohesion_calc, directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_cohesion_calc <- function(landscape) {

    ncells_landscape <- landscape %>%
        lsm_l_ta_calc() %>%
        dplyr::mutate(value = value * 10000 / prod(raster::res(landscape)))

    ncells_patch <- landscape %>%
        lsm_p_area_calc(., directions = directions) %>%
        dplyr::mutate(value = value * 10000 / prod(raster::res(landscape)))

    perim_patch <- landscape %>%
        lsm_p_perim_calc(., directions = directions)

    denominator <- perim_patch %>%
        dplyr::mutate(value = value * sqrt(ncells_patch$value)) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE))

    cohesion <- perim_patch %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value)) %>%
        dplyr::mutate(
            value = (1 - (value / denominator$value)) *
                          ((1 - (1 / sqrt(ncells_landscape$value))) ^ - 1) * 100
            )

   tibble::tibble(
       level = "class",
       class = as.integer(cohesion$class),
       id = as.integer(NA),
       metric = "cohesion",
       value = as.double(cohesion$value)
   )
}
