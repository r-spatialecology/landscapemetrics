#' DIVISION (class level)
#'
#' @description Landscape division index (Aggregation metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{DIVISON = (1 - \sum \limits_{j = 1}^{n} (\frac{a_{ij}} {A}) ^ 2)}
#' where \eqn{a_{ij}} is the area in square meters and \eqn{A} is the total
#' landscape area in square meters.
#'
#' DIVISION is an 'Aggregation metric. It can be in as the probability that two
#' randomly selected cells are not located in the same patch of class i. The landscape
#' division index is negatively correlated with the  effective mesh size  (\code{\link{lsm_c_mesh}}).
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Proportion }
#' \subsection{Ranges}{0 <= Division < 1}
#' \subsection{Behaviour}{Equals DIVISION = 0 if only one patch is present. Approaches
#' DIVISION = 1 if all patches of class i are single cells.}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{lsm_l_ta}}, \cr
#' \code{\link{lsm_l_division}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_division(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' Jaeger, J. A. 2000. Landscape division, splitting index, and effective mesh
#' size: new measures of landscape fragmentation.
#' Landscape ecology, 15(2), 115-130.
#'
#' @export
lsm_c_division <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_division_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_division_calc <- function(landscape, directions, resolution, extras = NULL) {

    # get patch area
    patch_area <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution,
                                  extras = extras)

    # get total area
    total_area <- sum(patch_area$value)

    # all values NA
    if (is.na(total_area)) {
        return(tibble::new_tibble(list(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "division",
                              value = as.double(NA))))
    }

    # calculate division for each patch
    patch_area$value <- (patch_area$value / total_area) ^ 2

    # summarise for classes
    division <- stats::aggregate(x = patch_area[, 5], by = patch_area[, 2],
                                 FUN = sum)

    division$value <- 1 - division$value

    return(tibble::new_tibble(list(
        level = rep("class", nrow(division)),
        class = as.integer(division$class),
        id = rep(as.integer(NA), nrow(division)),
        metric = rep("division", nrow(division)),
        value = as.double(division$value)
    )))
}
