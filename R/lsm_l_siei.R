#' SIEI (landscape level)
#'
#' @description Simpson's evenness index (Diversity metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{SIEI = \frac{1 - \sum \limits_{i = 1}^{m} P_{i}^{2}} {1 - \frac{1} {m}}}
#' where \eqn{P_{i}} is the proportion of class i and \eqn{m} is the
#' number of classes.
#'
#' SIEI is a 'Diversity metric'. The metric is widely used in biodiversity and ecology.
#' It is the ratio between the actual Simpson's diversity  index and the theoretical maximum
#' Simpson's diversity index.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{0 < SIEI <= 1}
#' \subsection{Behaviour}{Equals SIEI = 0 when only one patch is present and approaches
#' SIEI = 1 when the number of class types increases while the proportions are
#' equally distributed}
#'
#' @seealso
#' \code{\link{lsm_c_pland}},
#' \code{\link{lsm_l_pr}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_siei(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' Simpson, E. H. 1949. Measurement of diversity. Nature 163:688
#'
#' @export
lsm_l_siei <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         value <- lsm_l_siei_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             resolution = resolution
                         )

                         lsm_landscape_output(metric = "siei", value = value)
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_siei_calc <- function(landscape_mat, directions = NULL, resolution = NULL,
                            classes = NULL, class_patches = NULL, area_patches = NULL) {

    # lazy dependency resolution
    if (is.null(classes) || is.null(class_patches) || is.null(area_patches)) {
        deps <- resolve_extras(
            landscape_mat = landscape_mat,
            directions = directions,
            required = c("classes", "class_patches", "area_patches"),
            resolution = resolution
        )
        classes <- deps$classes
        class_patches <- deps$class_patches
        area_patches <- deps$area_patches
    }

    sidi <- lsm_l_sidi_calc(
        landscape_mat = landscape_mat,
        directions = directions,
        resolution = resolution,
        classes = classes,
        class_patches = class_patches,
        area_patches = area_patches
    )

    # all values NA
    if (is.na(sidi)) {
        return(as.double(NA))
    }

    pr <- lsm_l_pr_calc(
        landscape_mat = landscape_mat,
        classes = classes
    )

    siei <- sidi / (1 - (1 / pr))

    return(as.double(siei))
}
