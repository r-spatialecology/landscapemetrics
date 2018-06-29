#' ED (class level)
#'
#' @description Edge Density (Area and Edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param count_boundary Count landscape boundary as edge
#'
#' @details
#' \deqn{ED = \frac{\sum \limits_{k = 1}^{m} e_{ik}} {A} * 10000}
#' where \eqn{e_{ik}} is the total edge length in meters and \eqn{A} is the total
#' landcape area in square meters.
#'
#' ED is an 'Area and Edge metric'. The edge density equals the sum of all edges of class i
#' in relation to the landcape area. The boundary of the landscape is only included in the
#' corresponding total class edge length if \code{count_boundary = TRUE}.
#' The metric describes the configuration of the landscape, e.g. because  an  aggregation
#' of the same class will result in a low edge density. The metric is standarized to the
#' total landscape area, and therefore comparisons among landscapes with different total
#' areas are possible.

#' \subsection{Units}{Meters per hectare}
#' \subsection{Range}{ED >= 0}
#' \subsection{Behaviour}{Equals ED = 0 if only one patch is present (and the landcape
#' boundary is not included) and increases, without limit, as the landcapes becomes more
#' patchy}
#'
#' @seealso
#' \code{\link{lsm_c_te}},
#' \code{\link{lsm_l_ta}}, \cr
#' \code{\link{lsm_l_ed}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_ed(landscape)
#'
#' @aliases lsm_c_ed
#' @rdname lsm_c_ed
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_ed <- function(landscape, count_boundary) UseMethod("lsm_c_ed")

#' @name lsm_c_ed
#' @export
lsm_c_ed.RasterLayer <- function(landscape, count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_ed_calc,
                   count_boundary = count_boundary,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ed
#' @export
lsm_c_ed.RasterStack <- function(landscape, count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_ed_calc,
                   count_boundary = count_boundary,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_ed
#' @export
lsm_c_ed.RasterBrick <- function(landscape, count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_ed_calc,
                   count_boundary = count_boundary,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_ed
#' @export
lsm_c_ed.list <- function(landscape, count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_ed_calc,
                   count_boundary = count_boundary,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_ed_calc <- function(landscape, count_boundary = FALSE) {

    area_landscape <- lsm_l_ta_calc(landscape)

    ed <- landscape %>%
        lsm_c_te_calc(count_boundary = count_boundary) %>%
        dplyr::mutate(value = value / area_landscape$value)

    tibble::tibble(
        level = "class",
        class = as.integer(ed$class),
        id = as.integer(NA),
        metric = "edge density",
        value = as.double(ed$value)
    )
}
