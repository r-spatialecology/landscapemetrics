#' ED (landscape level)
#'
#' @description Edge Density (Area and Edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, stars, or a list of rasterLayers.
#' @param count_boundary Count landscape boundary as edge
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{ED = \frac{E} {A} * 10000}
#' where \eqn{E} is the total landscape edge in meters and \eqn{A} is the total
#' landscape area in square meters.
#'
#' ED is an 'Area and Edge metric'. The edge density equals all edges in the landscape
#' in relation to the landscape area. The boundary of the landscape is only included in the
#' corresponding total class edge length if \code{count_boundary = TRUE}.
#' The metric describes the configuration of the landscape, e.g. because an overall aggregation
#' of  classes will result in a low edge density. The metric is standardized to the
#' total landscape area, and therefore comparisons among landscapes with different total
#' areas are possible.

#' \subsection{Units}{Meters per hectare}
#' \subsection{Range}{ED >= 0}
#' \subsection{Behaviour}{Equals ED = 0 if only one patch is present (and the landscape
#' boundary is not included) and increases, without limit, as the landscapes becomes more
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
lsm_l_ed <- function(landscape,
                          count_boundary = FALSE, directions = 8) {
    landscape <- lsm_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_ed_calc,
                     count_boundary = count_boundary,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_ed_calc <- function(landscape, count_boundary, directions, resolution = NULL) {

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        resolution <- raster::res(landscape)

        landscape <- raster::as.matrix(landscape)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "ed",
                              value = as.double(NA)))
    }

    # get patch area
    area_patch <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution)

    # summarise to total area
    area_total <- sum(area_patch$value)

    # get total edge
    edge_landscape <- lsm_l_te_calc(landscape,
                                    count_boundary = count_boundary,
                                    resolution = resolution)

    # relative edge density
    ed <- edge_landscape$value / area_total

    return(tibble::tibble(level = "landscape",
                          class = as.integer(NA),
                          id = as.integer(NA),
                          metric = "ed",
                          value = as.double(ed)))
}
