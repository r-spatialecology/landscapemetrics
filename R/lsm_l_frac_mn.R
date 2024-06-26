#' FRAC_MN (landscape level)
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
#' FRAC_MN is a 'Shape metric'. The metric summarises the landscape
#' as the mean of the fractal dimension index of all patches in the landscape.
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
#' if all patches are irregular.}
#'
#' @seealso
#' \code{\link{lsm_p_frac}},
#' \code{\link[base]{mean}}, \cr
#' \code{\link{lsm_c_frac_mn}},
#' \code{\link{lsm_c_frac_sd}},
#' \code{\link{lsm_c_frac_cv}}, \cr
#' \code{\link{lsm_l_frac_sd}},
#' \code{\link{lsm_l_frac_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_frac_mn(landscape)
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
lsm_l_frac_mn <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_frac_mn_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_frac_mn_calc <- function(landscape, directions, resolution, extras = NULL){

    frac_patch <- lsm_p_frac_calc(landscape,
                                  directions = directions,
                                  resolution = resolution,
                                  extras = extras)

    # all values NA
    if (all(is.na(frac_patch$value))) {
        return(tibble::new_tibble(list(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "frac_mn",
                              value = as.double(NA))))
    }

    frac_mn <- mean(frac_patch$value)

    return(tibble::new_tibble(list(level = rep("landscape", length(frac_mn)),
                        class = rep(as.integer(NA), length(frac_mn)),
                        id = rep(as.integer(NA), length(frac_mn)),
                        metric = rep("frac_mn", length(frac_mn)),
                        value = as.double(mean(frac_patch$value)))))
}
