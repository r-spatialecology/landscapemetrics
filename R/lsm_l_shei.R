#' SHEI (landscape level)
#'
#' @description Shannons's evenness index (Diversity metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#'
#' @details
#' \deqn{SHEI = \frac{- \sum \limits_{i = 1} ^ {m} (P_{i} * \ln P_{i})} {\ln m}}
#' where \eqn{P_{i}} is the proportion of class i and \eqn{m} is the
#' number of classes.
#'
#' SHEI is a 'Diversity metric'. It is the ratio between the actual Shannon's diversity index
#' and and the theoretical maximum of the Shannon diversity index. It can be understood as a
#' measure of dominance.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{0 <= SHEI < 1}
#' \subsection{Behaviour}{Equals SHEI = 0 when only one patch  present and equals SHEI = 1
#' when the proportion of classes is completely equally distributed}
#'
#' @seealso
#' \code{\link{lsm_c_pland}},
#' \code{\link{lsm_l_pr}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_shei(landscape)
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
lsm_l_shei <- function(landscape){
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         shei <- lsm_l_shei_calc(
                             landscape_mat = landscape_mat,
                             resolution = resolution
                         )

                         lsm_landscape_output(metric = "shei", value = shei)
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_shei_calc <- function(landscape_mat, resolution = NULL,
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

    if (length(prop) == 1){
        shei <- 0
    } else {
        shei <- -sum(prop * log(prop)) / log(length(prop))
    }

    return(as.double(shei))
}
