#' SHAPE (patch level)
#'
#' @description Shape index (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{SHAPE = \frac{p_{ij}} {\min p_{ij}}}
#' where \eqn{p_{ij}} is the perimeter in terms of cell surfaces and \eqn{\min p_{ij}}
#' is the minimum perimeter of the patch in terms of cell surfaces.
#'
#' SHAPE is a 'Shape metric'. It describes the ratio between the actual perimeter of
#' the patch and the hypothetical minimum perimeter of the patch. The minimum perimeter
#' equals the perimeter if the patch would be maximally compact.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{SHAPE >= 1}
#' \subsection{Behaviour}{Equals SHAPE = 1 for a squared patch and
#' increases, without limit, as the patch shape becomes more complex.}
#'
#' @seealso
#' \code{\link{lsm_p_perim}},
#' \code{\link{lsm_p_area}}, \cr
#' \code{\link{lsm_c_shape_mn}},
#' \code{\link{lsm_c_shape_sd}},
#' \code{\link{lsm_c_shape_cv}}, \cr
#' \code{\link{lsm_l_shape_mn}},
#' \code{\link{lsm_l_shape_sd}},
#' \code{\link{lsm_l_shape_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_p_shape(terra::unwrap(landscape))
#'
#' @aliases lsm_p_shape
#' @rdname lsm_p_shape
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: https://www.umass.edu/landeco/
#'
#' Patton, D. R. 1975. A diversity index for quantifying habitat "edge".
#' Wildl. Soc.Bull. 3:171-173.
#'
#' @export
lsm_p_shape <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_p_shape_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_p_shape_calc <- function(landscape, directions, resolution = NULL){

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        resolution <- terra::res(landscape)
        landscape <-terra::as.matrix(landscape, wide = TRUE)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "patch",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "shape",
                              value = as.double(NA)))
    }

    # get perimeter of patches
    perimeter_patch <- lsm_p_perim_calc(landscape,
                                        directions = directions,
                                        resolution = resolution)

    # get area of patches
    area_patch <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution)

    # calculate shape index
    area_patch$value <- area_patch$value * 10000

    area_patch$n <- trunc(sqrt(area_patch$value))

    area_patch$m <- area_patch$value - area_patch$n ^ 2

    # m should be 0 but is not due to some integer/double numerical issues
    area_patch$m[area_patch$m < 0] <- 0

    area_patch$minp <- ifelse(test = area_patch$m == 0, yes = area_patch$n * 4,
                              no = ifelse(test = area_patch$n ^ 2 < area_patch$value & area_patch$value <= area_patch$n * (1 + area_patch$n),
                                          yes = 4 * area_patch$n + 2,
                                          no = ifelse(test = area_patch$value > area_patch$n * (1 + area_patch$n),
                                                      yes = 4 * area_patch$n + 4,
                                                      no = NA)))
    # this should not be possible anymore
    # # Throw warning that ifelse didn't work
    # if (anyNA(area_patch$minp)) {
    #     warning("Calculation of shape index produced NA", call. = FALSE)
    # }

    tibble::tibble(
        level = "patch",
        class = as.integer(perimeter_patch$class),
        id = as.integer(perimeter_patch$id),
        metric = "shape",
        value = as.double(perimeter_patch$value / area_patch$minp)
    )
}
