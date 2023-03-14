#' PD (class level)
#'
#' @description Patch density (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{PD = \frac{n_{i}} {A} * 10000 * 100}
#' where \eqn{n_{i}} is the number of patches and \eqn{A} is the total landscape
#' area in square meters.
#'
#' PD is an 'Aggregation metric'. It describes the fragmentation of a class, however, does not
#' necessarily contain information about the configuration or composition of the class. In
#' contrast to \code{\link{lsm_c_np}} it is standardized to the area and comparisons among
#' landscapes with different total area are possible.
#'
#' \subsection{Units}{Number per 100 hectares}
#' \subsection{Ranges}{0 < PD <= 1e+06}
#' \subsection{Behaviour}{Increases as the landscape gets more patchy. Reaches its maximum
#' if every cell is a different patch.}
#'
#' @seealso
#' \code{\link{lsm_c_np}},
#' \code{\link{lsm_l_ta}}, \cr
#' \code{\link{lsm_l_pd}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_pd(terra::unwrap(landscape))
#'
#' @aliases lsm_c_pd
#' @rdname lsm_c_pd
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: https://www.umass.edu/landeco/
#'
#' @export
lsm_c_pd <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_pd_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_pd_calc <- function(landscape, directions, resolution = NULL) {

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        resolution <- terra::res(landscape)

        landscape <-terra::as.matrix(landscape, wide = TRUE)
    }

    # all cells are NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "pd",
                              value = as.double(NA)))
    }

    # get patch area
    area_patch <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution)

    # summarise to total area
    area_patch <- sum(area_patch$value)

    # get number of patches
    np_class <- lsm_c_np_calc(landscape, directions = directions)

    # calculate relative patch density
    np_class$value <- (np_class$value / area_patch) * 100

    return(tibble::tibble(level = "class",
                          class = as.integer(np_class$class),
                          id = as.integer(NA),
                          metric = "pd",
                          value = as.double(np_class$value)))
}
