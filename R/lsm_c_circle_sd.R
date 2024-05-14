#' CIRCLE_SD (Class level)
#'
#' @description Standard deviation of related circumscribing circle (Shape metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{CIRCLE_{SD} = sd(CIRCLE[patch_{ij}])}
#' where \eqn{CIRCLE[patch_{ij}]} is the related circumscribing circle of each patch.
#'
#' CIRCLE_SD is a 'Shape metric' and summarises each class as the standard deviation of
#' the related circumscribing circle of all patches belonging to class i. CIRCLE describes
#' the ratio between the patch area and the smallest circumscribing circle of the patch
#' and characterises the compactness of the patch. The metric describes the differences
#' among patches of the same class i in the landscape. Because the metric is based on
#' distances or areas please make sure your data is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{CIRCLE_SD >= 0}
#' \subsection{Behaviour}{Equals CIRCLE_SD if the related circumscribing circle is identical
#' for all patches. Increases, without limit, as the variation of related circumscribing
#' circles increases.}
#'
#' @seealso
#' \code{\link{lsm_p_circle}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_circle_mn}},
#' \code{\link{lsm_c_circle_cv}}, \cr
#' \code{\link{lsm_l_circle_mn}},
#' \code{\link{lsm_l_circle_sd}},
#' \code{\link{lsm_l_circle_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_circle_sd(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' Baker, W. L., and Y. Cai. 1992. The r.le programs for multiscale analysis of
#' landscape structure using the GRASS geographical information system.
#' Landscape Ecology 7: 291-302.
#'
#' Based on C++ code from Project Nayuki (https://www.nayuki.io/page/smallest-enclosing-circle).
#'
#' @export
lsm_c_circle_sd <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_circle_sd_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_circle_sd_calc <- function(landscape, directions, resolution, extras = NULL) {

    # calculate circumscribing circle for each patch
    circle <- lsm_p_circle_calc(landscape,
                                directions = directions,
                                resolution = resolution,
                                extras = extras)

    # all values NA
    if (all(is.na(circle$value))) {
        return(tibble::new_tibble(list(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "circle_sd",
                              value = as.double(NA))))
    }

    # summarise for classes
    circle_sd <- stats::aggregate(x = circle[, 5], by = circle[, 2], FUN = stats::sd)

    return(tibble::new_tibble(list(
        level = rep("class", nrow(circle_sd)),
        class = as.integer(circle_sd$class),
        id = rep(as.integer(NA), nrow(circle_sd)),
        metric = rep("circle_sd", nrow(circle_sd)),
        value = as.double(circle_sd$value)
    )))
}

