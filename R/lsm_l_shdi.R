#' SHDI (landscape level)
#'
#' @description Shannon's diversity index (Diversity metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#'
#' @details
#' \deqn{SHDI = - \sum \limits_{i = 1}^{m} (P_{i} * \ln P_{i})}
#' where \eqn{P_{i}} is the proportion of class i.
#'
#' SHDI is a 'Diversity metric'. It is a widely used metric in biodiversity and ecology
#' and takes both the number of classes and the abundance of each class into account.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{SHDI >= 0}
#' \subsection{Behaviour}{Equals SHDI = 0 when only one patch is present and increases,
#' without limit, as the number of classes increases while the proportions are
#' equally distributed}
#'
#' @seealso
#' \code{\link{lsm_c_pland}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_shdi(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' Shannon, C., and W. Weaver. 1949. The mathematical theory of
#' communication. Univ. IllinoisPress, Urbana
#'
#' @export
lsm_l_shdi <- function(landscape) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         shdi <- lsm_l_shdi_calc(
                             landscape_mat = landscape_mat,
                             resolution = resolution
                         )

                         lsm_landscape_output(metric = "shdi", value = shdi)
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_shdi_calc <- function(landscape_mat, resolution = NULL,
                            classes = NULL, class_patches = NULL, area_patches = NULL) {

    # lazy dependency resolution
    if (is.null(classes) || is.null(class_patches) || is.null(area_patches)) {
        deps <- resolve_extras(
            landscape_mat = landscape_mat,
            directions = 8,
            required = c("classes", "class_patches", "area_patches"),
            resolution = resolution
        )
        classes <- deps$classes
        class_patches <- deps$class_patches
        area_patches <- deps$area_patches
    }

    # get class proportions (direction doesn't matter)
    prop <- lsm_c_pland_calc(
        landscape_mat = landscape_mat,
        directions = 8,
        resolution = resolution,
        classes = classes,
        class_patches = class_patches,
        area_patches = area_patches
    )

    # all values NA
    if (all(is.na(prop))) {
        return(as.double(NA))
    }

    prop <- prop / 100

    shdi <- sum(-prop * log(prop, exp(1)))

    return(as.double(shdi))
}
