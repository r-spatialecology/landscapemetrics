#' FRAC_CV (class level)
#'
#' @description Coefficient of variation fractal dimension index (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{FRAC_{CV} = cv(FRAC[patch_{ij}])}
#' where \eqn{FRAC[patch_{ij}]} equals the fractal dimension index of each patch.
#'
#' FRAC_CV is a 'Shape metric'. The metric summarises each class
#' as the Coefficient of variation of the fractal dimension index of all patches
#' belonging to class i. The fractal dimension index is based on the patch perimeter and
#' the patch area and describes the patch complexity. The Coefficient of variation is
#' scaled to the mean and comparable among different landscapes.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{FRAC_CV >= 0 }
#' \subsection{Behaviour}{Equals FRAC_CV = 0 if the fractal dimension index is identical
#' for all patches. Increases, without limit, as the variation of the fractal dimension
#' indices increases.}
#'
#' @seealso
#' \code{\link{lsm_p_frac}}, \cr
#' \code{\link{lsm_c_frac_mn}},
#' \code{\link{lsm_c_frac_sd}}, \cr
#' \code{\link{lsm_l_frac_mn}},
#' \code{\link{lsm_l_frac_sd}},
#' \code{\link{lsm_l_frac_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_frac_cv(terra::unwrap(landscape))
#'
#' @aliases lsm_c_frac_cv
#' @rdname lsm_c_frac_cv
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
lsm_c_frac_cv <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_frac_cv_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_frac_cv_calc <- function(landscape, directions, resolution = NULL){

    frac <- lsm_p_frac_calc(landscape,
                            directions = directions,
                            resolution = resolution)

    # all cells are NA
    if (all(is.na(frac$value))) {
        return(tibble::tibble(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "frac_cv",
                              value = as.double(NA)))
    }

    frac_cv <- stats::aggregate(x = frac[, 5], by = frac[, 2],
                                FUN = function(x) stats::sd(x) / mean(x) * 100)

    return(tibble::tibble(level = "class",
                          class = as.integer(frac_cv$class),
                          id = as.integer(NA),
                          metric = "frac_cv",
                          value = as.double(frac_cv$value)))
}
