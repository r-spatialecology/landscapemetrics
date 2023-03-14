#' FRAC (patch level)
#'
#' @description Fractal dimension index (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{FRAC = \frac{2 * \ln * (0.25 * p_{ij})} {\ln a_{ij}}}
#' where \eqn{p_{ij}} is the perimeter in meters and \eqn{a_{ij}} is the
#' area in square meters
#'
#' FRAC is a 'Shape metric'. The index is based on the patch perimeter and
#' the patch area and describes the patch complexity. Because it is standardized,
#' it is scale independent, meaning that increasing the patch size while not changing the
#' patch form will not change the ratio.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{1 <= FRAC <= 2 }
#' \subsection{Behaviour}{Approaches FRAC = 1 for a squared patch shape form and FRAC = 2
#' for a irregular patch shape.}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{lsm_p_perim}}, \cr
#' \code{\link{lsm_c_frac_mn}},
#' \code{\link{lsm_c_frac_sd}},
#' \code{\link{lsm_c_frac_cv}}, \cr
#' \code{\link{lsm_l_frac_mn}},
#' \code{\link{lsm_l_frac_sd}},
#' \code{\link{lsm_l_frac_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_p_frac(terra::unwrap(landscape))
#'
#' @aliases lsm_p_frac
#' @rdname lsm_p_frac
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: https://www.umass.edu/landeco/
#'
#' Mandelbrot, B. B. 1977. Fractals: Form, Chance, and Dimension.
#' San Francisco. W. H. Freeman and Company.
#'
#' @export
lsm_p_frac <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_p_frac_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_p_frac_calc <- function(landscape, directions, resolution = NULL){

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
                              metric = "frac",
                              value = as.double(NA)))
    }

    # get patch perimeter
    perimeter_patch <- lsm_p_perim_calc(landscape,
                                        directions = directions,
                                        resolution = resolution)

    # get patch area
    area_patch <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution)

    # calculate frac
    frac_patch <- 2 * log(0.25 * perimeter_patch$value) / log(area_patch$value * 10000)

    # NaN for patches with only one cell (mathematical reasons) -> should be 1
    frac_patch[is.na(frac_patch)] <- 1

    tibble::tibble(
        level = "patch",
        class = as.integer(perimeter_patch$class),
        id = as.integer(perimeter_patch$id),
        metric = "frac",
        value = as.double(frac_patch)
    )
}
