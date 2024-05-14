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
#' measure of dominance. Because the metric is based on distances or areas please
#' make sure your data is valid using \code{\link{check_landscape}}.
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
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
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

lsm_l_lpi_calc <- function(landscape, directions, resolution, extras = NULL) {

    # get patch area
    patch_area <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution,
                                  extras = extras)

    # summarise to total area
    total_area <- sum(patch_area$value)

    # all values NA
    if (is.na(total_area)) {
        return(tibble::new_tibble(list(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "lpi",
                              value = as.double(NA))))
    }

    # maximum value of patch_area / total_area
    lpi <- max(patch_area$value / total_area * 100)

    return(tibble::new_tibble(list(level = rep("landscape", length(lpi)),
                 class = rep(as.integer(NA), length(lpi)),
                 id = rep(as.integer(NA), length(lpi)),
                 metric = rep("lpi", length(lpi)),
                 value = as.double(lpi))))
}
