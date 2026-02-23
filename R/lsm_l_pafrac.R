#' PAFRAC  (landscape level)
#'
#' @description Perimeter-Area Fractal Dimension (Shape metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param verbose Print warning message if not sufficient patches are present
#'
#' @details
#' \deqn{PAFRAC = \frac{2}{\beta}}
#' where \eqn{\beta} is the slope of the regression of the area against the perimeter
#' (logarithm) \eqn{N \sum \limits_{i = 1}^{m} \sum \limits_{j = 1}^{n} \ln a_{ij} = a + \beta N \sum \limits_{i = 1}^{m} \sum \limits_{j = 1}^{n} \ln p_{ij}}
#'
#' PAFRAC is a 'Shape metric'. It describes the patch complexity of the landscape while being
#' scale independent. This means that increasing the patch size while not changing the
#' patch form will not change the metric. However, it is only meaningful if the relationship
#' between the area and perimeter is linear on a logarithmic scale. Furthermore, if there
#' are less than 10 patches in the landscape, the metric returns NA because of the small-sample
#' issue.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{1 <= PAFRAC <= 2}
#' \subsection{Behaviour}{Approaches PAFRAC = 1 for patches with simple shapes and
#' approaches PAFRAC = 2 for irregular shapes}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{lsm_p_perim}}, \cr
#' \code{\link{lsm_c_pafrac}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_pafrac(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' Burrough, P. A. 1986. Principles of Geographical Information Systems for
#' Land Resources Assessment. Monographs on Soil and Resources Survey No. 12.
#' Clarendon Press, Oxford
#'
#' @export
lsm_l_pafrac <- function(landscape, directions = 8, verbose = TRUE) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         pafrac <- lsm_l_pafrac_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             verbose = verbose,
                             resolution = resolution
                         )

                         lsm_landscape_output(metric = "pafrac", value = pafrac)
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_pafrac_calc <- function(landscape_mat, directions = NULL, verbose = TRUE, resolution = NULL,
                               classes = NULL, class_patches = NULL, area_patches = NULL, perimeter_patch = NULL) {

    # all values NA
    if (all(is.na(landscape_mat))) {
        return(as.double(NA))
    }

    # lazy dependency resolution
    if (is.null(classes) || is.null(class_patches) || is.null(area_patches) || is.null(perimeter_patch)) {
        deps <- resolve_extras(
            landscape_mat = landscape_mat,
            directions = directions,
            required = c("classes", "class_patches", "area_patches", "perimeter_patch"),
            resolution = resolution
        )
        classes <- deps$classes
        class_patches <- deps$class_patches
        area_patches <- deps$area_patches
        perimeter_patch <- deps$perimeter_patch
    }

    # get total number of patches using lsm_c_np_calc
    np_class <- lsm_c_np_calc(
        landscape_mat = landscape_mat,
        directions = directions,
        classes = classes,
        class_patches = class_patches
    )
    number_patches <- sum(np_class)

    # PAFRAC NA for less than 10 patches
    if (number_patches < 10) {

        pafrac <-  NA

        if (verbose) {
            warning("PAFRAC = NA for NP < 10", call. = FALSE)
        }

    # calculate pafrac as regression between area and perimeter (beta)
    } else {

        # area_patches is a named vector - convert to sqm
        area_sqm <- area_patches * 10000

        regression_model <- stats::lm(log(area_sqm) ~ log(perimeter_patch))

        pafrac <- 2 / regression_model$coefficients[[2]]
    }

    return(as.double(pafrac))
}
