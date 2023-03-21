#' LPI (landscape level)
#'
#' @description Largest patch index (Area and Edge metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{LPI = \frac{\max (a_{ij})} {A} * 100}
#' where \eqn{max(a_{ij})} is the area of the patch in square meters and \eqn{A}
#' is the total landscape area in square meters.
#'
#' The largest patch index is an 'Area and edge metric'. It is the percentage of the
#' landscape covered by the largest patch in the landscape. It is a simple
#' measure of dominance.
#'
#' \subsection{Units}{Percentage}
#' \subsection{Range}{0 < LPI <= 100}
#' \subsection{Behaviour}{Approaches LPI = 0 when the largest patch is becoming small
#' and equals LPI = 100 when only one patch is present}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{lsm_l_ta}}, \cr
#' \code{\link{lsm_c_lpi}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_lpi(landscape)
#'
#' @aliases lsm_l_lpi
#' @rdname lsm_l_lpi
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: https://www.umass.edu/landeco/
#'
#' @export
lsm_l_lpi <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_lpi_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_lpi_calc <- function(landscape, directions, resolution = NULL) {

    # get patch area
    patch_area <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution)

    # summarise to total area
    total_area <- sum(patch_area$value)

    # all values NA
    if (is.na(total_area)) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "lpi",
                              value = as.double(NA)))
    }

    # maximum value of patch_area / total_area
    lpi <- max(patch_area$value / total_area * 100)

    return(tibble::tibble(level = "landscape",
                          class = as.integer(NA),
                          id = as.integer(NA),
                          metric = "lpi",
                          value = as.double(lpi)))
}
