#' MSIDI (landscape level)
#'
#' @description Modified Simpson's diversity index (Diversity metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{MSIDI = -\ln \sum \limits_{i = 1}^{m} P_{i}^{2}}
#' where \eqn{P_{i}} is the landscape area proportion of class i.
#'
#' MSIDI is a 'Diversity metric'.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{MSIDI >= 0}
#' \subsection{Behaviour}{MSIDI = 0 if only one patch is present and increases, without
#' limit, as the amount of patches with equally distributed landscape proportions increases}
#'
#' @seealso
#' \code{\link{lsm_l_sidi}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_msidi(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' Simpson, E. H. 1949. Measurement of diversity. Nature 163:688
#'
#' Pielou, E. C. 1975. Ecological Diversity. Wiley-Interscience, New York.
#'
#' Romme, W. H. 1982. Fire and landscapediversity in subalpine forests of
#' Yellowstone National Park.Ecol.Monogr. 52:199-221
#'
#' @export
lsm_l_msidi <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         msidi <- lsm_l_msidi_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             resolution = resolution
                         )

                         lsm_landscape_output(metric = "msidi", value = msidi)
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_msidi_calc <- function(landscape_mat, directions = NULL, resolution = NULL,
                             classes = NULL, class_patches = NULL, area_patches = NULL) {

    # all values NA
    if (all(is.na(landscape_mat))) {
        return(as.double(NA))
    }

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

    patch_area <- lsm_p_area_calc(
        landscape_mat = landscape_mat,
        directions = directions,
        resolution = resolution,
        classes = classes,
        class_patches = class_patches,
        area_patches = area_patches
    )

    # aggregate by class using tapply on named vector
    class_area <- tapply(patch_area, names(patch_area), sum)

    msidi <- -log(sum((class_area / sum(class_area)) ^ 2))

    return(as.double(msidi))
}
