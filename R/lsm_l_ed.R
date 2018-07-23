#' ED (landscape level)
#'
#' @description Edge Density (Area and Edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param count_boundary Count landscape boundary as edge
#' @param directions The number of directions in which cells should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{ED = \frac{E} {A} * 10000}
#' where \eqn{E} is the total landscape edge in meters and \eqn{A} is the total
#' landcape area in square meters.
#'
#' ED is an 'Area and Edge metric'. The edge density equals all edges in the landscape
#' in relation to the landcape area. The boundary of the landscape is only included in the
#' corresponding total class edge length if \code{count_boundary = TRUE}.
#' The metric describes the configuration of the landscape, e.g. because an overall aggregation
#' of  classes will result in a low edge density. The metric is standarized to the
#' total landscape area, and therefore comparisons among landscapes with different total
#' areas are possible.

#' \subsection{Units}{Meters per hectare}
#' \subsection{Range}{ED >= 0}
#' \subsection{Behaviour}{Equals ED = 0 if only one patch is present (and the landcape
#' boundary is not included) and increases, without limit, as the landcapes becomes more
#' patchy}
#'
#' @seealso
#' \code{\link{lsm_l_te}},
#' \code{\link{lsm_l_ta}}, \cr
#' \code{\link{lsm_c_ed}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_ed(landscape)
#'
#' @aliases lsm_l_ed
#' @rdname lsm_l_ed
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_ed <- function(landscape, count_boundary, directions) UseMethod("lsm_l_ed")

#' @name lsm_l_ed
#' @export
lsm_l_ed.RasterLayer <- function(landscape,
                                 count_boundary = FALSE, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_ed_calc,
                   count_boundary = count_boundary,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_ed
#' @export
lsm_l_ed.RasterStack <- function(landscape,
                                 count_boundary = FALSE, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_ed_calc,
                   count_boundary = count_boundary,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_ed
#' @export
lsm_l_ed.RasterBrick <- function(landscape,
                                 count_boundary = FALSE, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_ed_calc,
                   count_boundary = count_boundary,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_ed
#' @export
lsm_l_ed.list <- function(landscape,
                          count_boundary = FALSE, directions = 8) {
    purrr::map_dfr(landscape, lsm_l_ed_calc,
                   count_boundary = count_boundary,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_ed_calc <- function(landscape, count_boundary, directions) {

    area_landscape <- lsm_l_ta_calc(landscape, directions = directions)

    edge_landscape <- lsm_l_te_calc(landscape,
                                    count_boundary = count_boundary)

    ed <- dplyr::mutate(edge_landscape, value = value / area_landscape$value)

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "ed",
        value = as.double(ed$value)
    )
}
