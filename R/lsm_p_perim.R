#' PERIM (patch level)
#'
#' @description Perimeter (Area and edge metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{PERIM = p_{ij}}
#' where \eqn{p_{ij}} is the perimeter in meters.
#'
#' PERIM is an 'Area and edge metric'. It equals the perimeter of the patch
#' including also the edge to the landscape boundary. The metric describes
#' patch area (larger perimeter for larger patches), but also patch shape
#' (large perimeter for irregular shapes).
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{PERIM > 0}
#' \subsection{Behaviour}{Increases, without limit, as patch size and
#' complexity increases.}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_p_perim(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_p_perim <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         perim <- lsm_p_perim_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             resolution = resolution
                         )

                         lsm_patch_output(metric = "perim",
                                          class = as.integer(names(perim)),
                                          value = unname(perim),
                                          id = seq_along(perim))
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_p_perim_calc <- function(landscape_mat, directions = NULL, resolution = NULL,
                              classes = NULL, class_patches = NULL, perimeter_patch = NULL) {

    # all values NA
    if (all(is.na(landscape_mat))) {
        return(stats::setNames(as.double(NA), NA_character_))
    }

    # lazy dependency resolution
    if (is.null(classes) || is.null(class_patches) || is.null(perimeter_patch)) {
        deps <- resolve_extras(
            landscape_mat = landscape_mat,
            directions = directions,
            required = c("classes", "class_patches", "perimeter_patch"),
            resolution = resolution
        )
        classes <- deps$classes
        class_patches <- deps$class_patches
        perimeter_patch <- deps$perimeter_patch
    }

    # perimeter_patch is a named vector
    # names are class IDs, values are perimeters
    perimeter_vec <- perimeter_patch

    # return named vector (preserve names)
    structure(as.double(perimeter_vec), names = names(perimeter_vec))
}
