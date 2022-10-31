#' CIRCLE_CV (Class level)
#'
#' @description Coefficient of variation of related circumscribing circle (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{CIRCLE_{CV} = cv(CIRCLE[patch_{ij}])}
#' where \eqn{CIRCLE[patch_{ij}]} is the related circumscribing circle of each patch.
#'
#' CIRCLE_CV is a 'Shape metric' and summarises each class as the Coefficient of variation of
#' the related circumscribing circle of all patches belonging to class i. CIRCLE describes
#' the ratio between the patch area and the smallest circumscribing circle of the patch
#' and characterises the compactness of the patch. CIRCLE_CV describes the differences among
#' patches of the same class i in the landscape. Because it is scaled to the mean,
#' it is easily comparable.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{CIRCLE_CV >= 0}
#' \subsection{Behaviour}{Equals CIRCLE_CV if the related circumscribing circle is identical
#' for all patches. Increases, without limit, as the variation of related circumscribing
#' circles increases.}
#'
#' @seealso
#' \code{\link{lsm_p_circle}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_circle_mn}},
#' \code{\link{lsm_c_circle_sd}}, \cr
#' \code{\link{lsm_l_circle_mn}},
#' \code{\link{lsm_l_circle_sd}},
#' \code{\link{lsm_l_circle_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_circle_cv(landscape)
#'
#' @aliases lsm_c_circle_cv
#' @rdname lsm_c_circle_cv
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: https://www.umass.edu/landeco/
#'
#' Baker, W. L., and Y. Cai. 1992. The r.le programs for multiscale analysis of
#' landscape structure using the GRASS geographical information system.
#' Landscape Ecology 7: 291-302.
#'
#' Based on C++ code from Project Nayuki (https://www.nayuki.io/page/smallest-enclosing-circle).
#'
#' @export
lsm_c_circle_cv <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_circle_cv_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_circle_cv_calc <- function(landscape, directions, resolution = NULL) {

    # calculate circumscribing circle for each patch
    circle <- lsm_p_circle_calc(landscape,
                                directions = directions,
                                resolution = resolution)

    # all values NA
    if (all(is.na(circle$value))) {
        return(tibble::tibble(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "circle_cv",
                              value = as.double(NA)))
    }

    # summarise for classes
    circle_cv <- stats::aggregate(x = circle[, 5], by = circle[, 2], FUN = raster::cv)

    return(tibble::tibble(level = "class",
                          class = as.integer(circle_cv$class),
                          id = as.integer(NA),
                          metric = "circle_cv",
                          value = as.double(circle_cv$value)))
}

