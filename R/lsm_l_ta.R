#' TA (landscape level)
#'
#' @description Total area (Area and edge metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{CA = sum(AREA[patch_{ij}])}
#' where \eqn{AREA[patch_{ij}]} is the area of each patch in hectares.
#'
#' TA is an 'Area and edge metric'. The total (class) area sums the area of all patches
#' in the landscape. It is the area of the observation area.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{TA > 0}
#' \subsection{Behaviour}{Approaches TA > 0 if the landscape is small and increases,
#' without limit, as the size of the landscape increases.}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{sum}}, \cr
#' \code{\link{lsm_c_ca}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_ta(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_l_ta <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         ta <- lsm_l_ta_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             resolution = resolution
                         )

                         lsm_landscape_output(metric = "ta", value = ta)
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_ta_calc <- function(landscape_mat, directions = NULL, resolution = NULL,
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

    # all values NA - no classes present
    if (is.null(classes) || length(classes) == 0) {
        return(as.double(NA))
    }

    # calculate total area directly from area_patches
    total_area <- sum(unlist(area_patches))

    # all values NA
    if (is.na(total_area)) {
        return(as.double(NA))
    }

    return(as.double(total_area))
}
