#' FRAC_MN (class level)
#'
#' @description Mean fractal dimension index (Shape metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{FRAC_{MN} = mean(FRAC[patch_{ij}])}
#' where \eqn{FRAC[patch_{ij}]} equals the fractal dimension index of each patch.
#'
#' FRAC_MN is a 'Shape metric'. The metric summarises each class
#' as the mean of the fractal dimension index of all patches belonging to class i.
#' The fractal dimension index is based on the patch perimeter and
#' the patch area and describes the patch complexity. The Coefficient of variation is
#' scaled to the mean and comparable among different landscapes.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{FRAC_MN > 0 }
#' \subsection{Behaviour}{Approaches FRAC_MN = 1 if all patches are squared and FRAC_MN = 2
#'  if all patches are irregular.}
#'
#' @seealso
#' \code{\link{lsm_p_frac}},
#' \code{\link[base]{mean}}, \cr
#' \code{\link{lsm_c_frac_sd}},
#' \code{\link{lsm_c_frac_cv}}, \cr
#' \code{\link{lsm_l_frac_mn}},
#' \code{\link{lsm_l_frac_sd}},
#' \code{\link{lsm_l_frac_cv}}
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_frac_mn(landscape)
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
lsm_c_frac_mn <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         frac_mn <- lsm_c_frac_mn_calc(x, directions = directions)
                         lsm_class_output(metric = "frac_mn",
                                          class = frac_mn$class,
                                          value = frac_mn$value)
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_frac_mn_calc <- function(landscape, directions, resolution, extras = NULL){

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

    frac_mean <- stats::aggregate(x = frac[, 5], by = frac[, 2], FUN = mean)

    return(list(class = as.integer(frac_mean$class),
                value = as.double(frac_mean$value)))
}
