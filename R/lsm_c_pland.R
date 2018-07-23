#' PLAND (class level)
#'
#' @description Percentage of landscape of class (Area and Edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which cells should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{PLAND = \frac{\sum \limits_{j = 1}^{n} a_{ij}} {A} * 100}
#' where \eqn{a_{ij}} is the area of each patch and \eqn{A} is the total
#' landscape area.
#'
#' PLAND is an 'Area and edge metric'. It is the percentage of the landscape
#' belonging to class i. It is a measure of compostion and because of the relative
#' character directly comparable among landscapes with different total areas.
#'
#' \subsection{Units}{Percentage}
#' \subsection{Range}{0 < PLAND <= 100}
#' \subsection{Behaviour}{Approaches PLAND = 0 when the proportional class area is decreasing.
#' Equals PLAND = 100 when only one patch is present.}
#'
#' @seealso
#' \code{\link{lsm_c_ca}},
#' \code{\link{lsm_l_ta}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_pland(landscape)
#'
#' @aliases lsm_c_pland
#' @rdname lsm_c_pland
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_pland <- function(landscape, directions) UseMethod("lsm_c_pland")

#' @name lsm_c_pland
#' @export
lsm_c_pland.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_pland_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

    }

#' @name lsm_c_pland
#' @export
lsm_c_pland.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_pland_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_pland
#' @export
lsm_c_pland.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_pland_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_pland
#' @export
lsm_c_pland.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_c_pland_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_pland_calc <- function(landscape, directions){

    pland <- landscape %>%
        lsm_p_area_calc(directions = directions) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
        dplyr::mutate(value = value / sum(value) * 100)

    tibble::tibble(
        level = "class",
        class = as.integer(pland$class),
        id = as.integer(NA),
        metric = "pland",
        value = as.double(pland$value)
    )
}

