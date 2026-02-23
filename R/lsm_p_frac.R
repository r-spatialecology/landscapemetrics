#' FRAC (patch level)
#'
#' @description Fractal dimension index (Shape metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
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
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
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
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_p_frac(landscape)
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
lsm_p_frac <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         frac <- lsm_p_frac_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             resolution = resolution
                         )

                         lsm_patch_output(metric = "frac",
                                          class = as.integer(names(frac)),
                                          value = unname(frac),
                                          id = seq_along(frac))
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_p_frac_calc <- function(landscape_mat, directions = NULL, resolution = NULL,
                             classes = NULL, class_patches = NULL, perimeter_patch = NULL, area_patches = NULL) {

    # all values NA
    if (all(is.na(landscape_mat))) {
        return(stats::setNames(as.double(NA), NA_character_))
    }

    # lazy dependency resolution
    if (is.null(classes) || is.null(class_patches) || is.null(perimeter_patch) || is.null(area_patches)) {
        deps <- resolve_extras(
            landscape_mat = landscape_mat,
            directions = directions,
            required = c("classes", "class_patches", "perimeter_patch", "area_patches"),
            resolution = resolution
        )
        classes <- deps$classes
        class_patches <- deps$class_patches
        perimeter_patch <- deps$perimeter_patch
        area_patches <- deps$area_patches
    }

    # calculate frac
    frac_patch <- 2 * log(0.25 * perimeter_patch) / log(area_patches * 10000)

    # NaN for patches with only one cell (mathematical reasons) -> should be 1
    frac_patch[is.na(frac_patch)] <- 1

    # return named vector (preserve names)
    structure(as.double(frac_patch), names = names(frac_patch))
}
