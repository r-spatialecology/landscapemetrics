#' DIVISION (landscape level)
#'
#' @description Landscape division index (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{DIVISON = (1 - \sum \limits_{i = 1}^{m} \sum \limits_{j = 1}^{n} (\frac{a_{ij}} {A}) ^ 2) }
#' where \eqn{a_{ij}} is the area in square meters and \eqn{A} is the total
#' landscape area in square meters.
#'
#' DIVISION is an 'Aggregation metric. It can be in as the probability that two
#' randomly selected cells are not located in the same patch. The landscape
#' division index is negatively correlated with the  effective mesh size (\code{\link{lsm_c_mesh}}).
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
#' Jaeger, J. A. 2000. Landscape division, splitting index, and effective mesh
#' size: new measures of landscape fragmentation.
#' Landscape ecology, 15(2), 115-130.
#'
#' @export
lsm_l_division <- function(landscape, directions = 8) {
    landscape <- lsm_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_division_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_division_calc <- function(landscape, directions, resolution = NULL) {

    # get patch area
    area_patch <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution)

    # summarise to total area
    area_total <- sum(area_patch$value)

    # all values NA
    if (is.na(area_total)) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "division",
                              value = as.double(NA)))
    }

    # divison for each patch
    area_patch$value <- (area_patch$value / area_total) ^ 2

    # summarise for whole landscape
    division <- 1 - sum(area_patch$value)

    return(tibble::tibble(level = "landscape",
                          class = as.integer(NA),
                          id = as.integer(NA),
                          metric = "division",
                          value = as.double(division)))
}
