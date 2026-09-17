#' PD (class level)
#'
#' @description Patch density (Aggregation metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{PD = \frac{n_{i}} {A} * 10000 * 100}
#' where \eqn{n_{i}} is the number of patches and \eqn{A} is the total landscape
#' area in square meters.
#'
#' PD is an 'Aggregation metric'. It describes the fragmentation of a class, however, does not
#' necessarily contain information about the configuration or composition of the class. In
#' contrast to \code{\link{lsm_c_np}} it is standardized to the area and comparisons among
#' landscapes with different total area are possible.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Number per 100 hectares}
#' \subsection{Ranges}{0 < PD <= 1e+06}
#' \subsection{Behaviour}{Increases as the landscape gets more patchy. Reaches its maximum
#' if every cell is a different patch.}
#'
#' @seealso
#' \code{\link{lsm_c_np}},
#' \code{\link{lsm_l_ta}}, \cr
#' \code{\link{lsm_l_pd}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_pd(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_c_pd <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         pd <- lsm_c_pd_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             resolution = resolution
                         )

                         lsm_class_output(metric = "pd",
                                          class = as.integer(names(pd)),
                                          value = unname(pd))
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_pd_calc <- function(landscape_mat, directions = NULL, resolution = NULL,
                          classes = NULL, class_patches = NULL, area_patches = NULL) {

    # all cells are NA
    if (all(is.na(landscape_mat))) {
        return(stats::setNames(as.double(NA), NA_character_))
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

    # get patch area
    area_patch <- lsm_p_area_calc(
        landscape_mat = landscape_mat,
        directions = directions,
        resolution = resolution,
        classes = classes,
        class_patches = class_patches,
        area_patches = area_patches
    )

    # summarise to total area
    area_total <- sum(unname(area_patch))

    # get number of patches (returns named vector)
    np_class <- lsm_c_np_calc(
        landscape_mat = landscape_mat,
        directions = directions,
        classes = classes,
        class_patches = class_patches
    )

    # calculate relative patch density
    pd <- (np_class / area_total) * 100

    # return named vector
    stats::setNames(as.double(pd), names(pd))
}
