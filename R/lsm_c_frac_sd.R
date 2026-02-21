#' FRAC_SD (class level)
#'
#' @description Standard deviation fractal dimension index (Shape metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{FRAC_{SD} = sd(FRAC[patch_{ij}])}
#' where \eqn{FRAC[patch_{ij}]} equals the fractal dimension index of each patch.
#'
#' FRAC_SD is a 'Shape metric'. The metric summarises each class
#' as the standard deviation of the fractal dimension index of all patches
#' belonging to class i. The fractal dimension index is based on the patch perimeter and
#' the patch area and describes the patch complexity.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{FRAC_SD>= 0 }
#' \subsection{Behaviour}{Equals FRAC_SD = 0 if the fractal dimension index is identical
#' for all patches. Increases, without limit, as the variation of the fractal dimension
#' indices increases.}
#'
#' @seealso
#' \code{\link{lsm_p_frac}},
#' \code{\link[stats]{sd}} \cr
#' \code{\link{lsm_c_frac_mn}},
#' \code{\link{lsm_c_frac_cv}}, \cr
#' \code{\link{lsm_l_frac_mn}},
#' \code{\link{lsm_l_frac_sd}},
#' \code{\link{lsm_l_frac_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_frac_sd(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' Mandelbrot, B. B. 1977. Fractals: Form, Chance, and Dimension.
#' San Francisco. W. H. Freeman and Company.
#'
#' @export
lsm_c_frac_sd <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         frac_sd <- lsm_c_frac_sd_calc(x, directions = directions)
                         lsm_class_output(metric = "frac_sd",
                                          class = frac_sd$class,
                                          value = frac_sd$value)
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_frac_sd_calc <- function(landscape, directions, resolution, extras = NULL){

    frac <- lsm_p_frac_calc(landscape,
                            directions = directions,
                            resolution = resolution,
                            extras = extras)
    frac <- lsm_patch_output(metric = "frac",
                             class = frac$class,
                             value = frac$value,
                             id = frac$id)

    # all cells are NA
    if (all(is.na(frac$value))) {
        return(list(class = as.integer(NA), value = as.double(NA)))
    }

    frac_sd <- stats::aggregate(x = frac[, 5], by = frac[, 2],
                                FUN = stats::sd)

    return(list(class = as.integer(frac_sd$class),
                value = as.double(frac_sd$value)))
}
