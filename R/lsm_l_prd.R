#' PRD (landscape level)
#'
#' @description Patch richness density (Diversity metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{PRD = \frac{m} {A} * 10000 * 100 }
#' where \eqn{m} is the number of classes and \eqn{A} is the total landscape area in
#' square meters.
#'
#' PRD is a 'Diversity metric'. It is one of the simplest diversity and composition measures.
#' In contrast to \code{\link{lsm_l_pr}}, it is a relative measure and following, comparable
#' among landscapes with different total landscape areas.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Number per 100 hectares}
#' \subsection{Range}{PR > 0}
#' \subsection{Behaviour}{Approaches PRD > 1 when only one patch is present and the landscape
#' is rather large. Increases, without limit, as the number of classes increases and the
#' total landscape area decreases.}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_prd(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_l_prd <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         prd <- lsm_l_prd_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             resolution = resolution
                         )

                         lsm_landscape_output(metric = "prd", value = prd)
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_prd_calc <- function(landscape_mat, directions = NULL, resolution = NULL,
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

    # get patch area
    area_patch <- lsm_p_area_calc(
        landscape_mat = landscape_mat,
        directions = directions,
        resolution = resolution,
        classes = classes,
        class_patches = class_patches,
        area_patches = area_patches
    )

    # summarise for total landscape
    area_total <- sum(area_patch)

    # all values NA
    if (is.na(area_total)) {
        return(as.double(NA))
    }

    # get number of classes
    pr_landscape <- lsm_l_pr_calc(
        landscape_mat = landscape_mat,
        classes = classes
    )

    # relative number of classes
    prd <- pr_landscape / area_total * 100

    return(as.double(prd))
}
