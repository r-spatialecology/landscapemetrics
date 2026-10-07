#' AI (landscape level)
#'
#' @description Aggregation index (Aggregation metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{AI = \Bigg[\sum\limits_{i=1}^m \Big( \frac{g_{ii}}{max-g_{ii}} \Big) P_{i} \Bigg](100) }
#'
#' where \eqn{g_{ii}} is the number of like adjacencies based on the single-count method and
#' \eqn{max-g_{ii}} is the classwise maximum number of like adjacencies of class i and \eqn{P_{i}}
#' the proportion of landscape compromised of class i.
#'
#' AI is an 'Aggregation metric'. It equals the number of like adjacencies divided
#' by the theoretical maximum possible number of like adjacencies for that class
#' summed over each class for the entire landscape. The metric is based on the
#' adjacency matrix and the single-count method.
#'
#' \subsection{Units}{Percent}
#' \subsection{Range}{0 <= AI <= 100}
#' \subsection{Behaviour}{Equals 0 for maximally disaggregated and 100
#'  for maximally aggregated classes.}
#'
#' @return tibble
#' @seealso
#' \code{\link{lsm_c_ai}}
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_ai(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' He, H. S., DeZonia, B. E., & Mladenoff, D. J. 2000. An aggregation index (AI)
#' to quantify spatial patterns of landscapes. Landscape ecology, 15(7), 591-601.
#'
#' @export
lsm_l_ai <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         ai <- lsm_l_ai_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             resolution = resolution
                         )

                         lsm_landscape_output(metric = "ai", value = ai)
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_ai_calc <- function(landscape_mat, directions = NULL, resolution = NULL,
                          classes = NULL, class_patches = NULL, area_patches = NULL, composition_vector = NULL) {

    # all values NA
    if (all(is.na(landscape_mat))) {
        return(as.double(NA))
    }

    # lazy dependency resolution
    if (is.null(classes) || is.null(class_patches) || is.null(area_patches) || is.null(composition_vector)) {
        deps <- resolve_extras(
            landscape_mat = landscape_mat,
            directions = directions,
            required = c("classes", "class_patches", "area_patches", "composition_vector"),
            resolution = resolution
        )
        classes <- deps$classes
        class_patches <- deps$class_patches
        area_patches <- deps$area_patches
        composition_vector <- deps$composition_vector
    }

    # get aggregation index for each class
    ai <- lsm_c_ai_calc(
        landscape_mat = landscape_mat,
        composition_vector = composition_vector
    )

    # get proportional class area
    pland <- lsm_c_pland_calc(
        landscape_mat = landscape_mat,
        directions = 8,
        resolution = resolution,
        classes = classes,
        class_patches = class_patches,
        area_patches = area_patches
    )

    # final AI index
    ai <- sum(ai * (pland / 100), na.rm = TRUE)

    return(as.double(ai))
}
