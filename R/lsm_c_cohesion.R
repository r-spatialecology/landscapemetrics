#' COHESION (class level)
#'
#' @description Patch Cohesion Index (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{COHESION = 1 - (\frac{\sum \limits_{j = 1}^{n} p_{ij}} {\sum \limits_{j = 1}^{n} p_{ij} \sqrt{a_{ij}}}) * (1 - \frac{1} {\sqrt{Z}}) ^ {-1} * 100}
#' where \eqn{p_{ij}} is the perimeter in meters, \eqn{a_{ij}} is the area in square
#' meters and \eqn{Z} is the number of cells.
#'
#' COHESION is an 'Aggregation metric'. It characterises the connectedness of patches
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
#' lsm_c_cohesion(terra::unwrap(landscape))
#'
#' @aliases lsm_c_cohesion
#' @rdname lsm_c_cohesion
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: https://www.umass.edu/landeco/
#'
#' Schumaker, N. H. 1996. Using landscape indices to predict habitat
#' connectivity. Ecology, 77(4), 1210-1225.
#'
#' @export
lsm_c_cohesion <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_cohesion_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_cohesion_calc <- function(landscape, directions, resolution = NULL) {

    # convert to raster to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        resolution <- terra::res(landscape)

        landscape <- terra::as.matrix(landscape, wide = TRUE)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "cohesion",
                              value = as.double(NA)))
    }

    # get number of cells (only not NAs)
    ncells_landscape <- length(landscape[!is.na(landscape)])

    # get patch area
    patch_area <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution)

    # get number of cells for each patch -> area = n_cells * res / 10000
    patch_area$ncells <- patch_area$value * 10000 / prod(resolution)

    # get perim of patch
    perim_patch <- lsm_p_perim_calc(landscape,
                                    directions = directions,
                                    resolution = resolution)

    # calculate denominator of cohesion
    perim_patch$denominator <- perim_patch$value * sqrt(patch_area$ncells)

    # group by class and sum
    denominator <- stats::aggregate(x = perim_patch[, 6], by = perim_patch[, 2],
                                    FUN = sum)

    cohesion <- stats::aggregate(x = perim_patch[, 5], by = perim_patch[, 2],
                                 FUN = sum)

    # calculate cohesion
    cohesion$value <- (1 - (cohesion$value / denominator$denominator)) *
        ((1 - (1 / sqrt(ncells_landscape))) ^ -1) * 100

    return(tibble::tibble(level = "class",
                          class = as.integer(cohesion$class),
                          id = as.integer(NA),
                          metric = "cohesion",
                          value = as.double(cohesion$value)))
}
