#' DIVISION (landscape level)
#'
#' @description Landscape division index (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{DIVISON = (1 - \sum \limits_{i = 1}^{m} \sum \limits_{j = 1}^{n} (\frac{a_{ij}} {A}) ^ 2) }
#' where \eqn{a_{ij}} is the area in square meters and \eqn{A} is the total
#' landscape area in square meters.
#'
#' DIVISION is an 'Aggregation metric. It can be in as the probability that two
#' randomly selected cells are not located in the same patch. The landscape
#' division index is negativly correlated with the  effective mesh size (\code{\link{lsm_c_mesh}}).
#'
#' \subsection{Units}{Proportion }
#' \subsection{Ranges}{0 <= Division < 1}
#' \subsection{Behaviour}{Equals DIVISION = 0 if only one patch is present. Approaches
#' DIVISION = 1 if all patches of class i are single cells.}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{lsm_l_ta}}, \cr
#' \code{\link{lsm_c_division}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_division(landscape)
#'
#' @aliases lsm_l_division
#' @rdname lsm_l_division
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_division <- function(landscape) UseMethod("lsm_l_division")

#' @name lsm_l_division
#' @export
lsm_l_division.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_division_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_division
#' @export
lsm_l_division.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_division_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_division
#' @export
lsm_l_division.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_division_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_division
#' @export
lsm_l_division.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_division_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_division_calc <- function(landscape) {

    area_landscape <- lsm_l_ta_calc(landscape)

    area_patch <- lsm_p_area_calc(landscape)

    division <- area_patch %>%
        dplyr::mutate(value = (value / area_landscape$value) ^ 2) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
        dplyr::mutate(value = 1 - value)

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "landscape division index",
        value = as.double(division$value)
    )
}
