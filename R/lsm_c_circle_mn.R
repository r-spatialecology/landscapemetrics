#' CIRCLE_MN (Class level)
#'
#' @description Mean of related circumscribing circle (Shape metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{CIRCLE_{MN} = mean(CIRCLE[patch_{ij}])}
#' where \eqn{CIRCLE[patch_{ij}]} is the related circumscribing circle of each patch.
#'
#' CIRCLE_MN is a 'Shape metric' and summarises each class as the mean of the related
#' circumscribing circle of all patches belonging to class i. CIRCLE describes
#' the ratio between the patch area and the smallest circumscribing circle of the patch
#' and characterises the compactness of the patch.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{CIRCLE_MN > 0}
#' \subsection{Behaviour}{Approaches CIRCLE_MN = 0 if the related circumscribing circle
#' of all patches is small. Increases, without limit, as the related circumscribing circles
#' increase.}
#'
#' @seealso
#' \code{\link{lsm_p_circle}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_circle_sd}},
#' \code{\link{lsm_c_circle_cv}}, \cr
#' \code{\link{lsm_l_circle_mn}},
#' \code{\link{lsm_l_circle_sd}},
#' \code{\link{lsm_l_circle_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_circle_mn(landscape)
#'
#' @aliases lsm_c_circle_mn
#' @rdname lsm_c_circle_mn
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
lsm_c_circle_mn <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_circle_mn_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_circle_mn_calc <- function(landscape, directions, resolution, extras = NULL) {

    # calculate circumscribing circle for each patch
    circle <- lsm_p_circle_calc(landscape,
                                directions = directions,
                                resolution = resolution,
                                extras = extras)

    # all values NA
    if (all(is.na(circle$value))) {
        return(tibble::tibble(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "circle_mn",
                              value = as.double(NA)))
    }

    # summarise for classes
    circle_mn <- stats::aggregate(x = circle[, 5], by = circle[, 2], FUN = mean)

    return(tibble::tibble(level = "class",
                          class = as.integer(circle_mn$class),
                          id = as.integer(NA),
                          metric = "circle_mn",
                          value = as.double(circle_mn$value)))
}

