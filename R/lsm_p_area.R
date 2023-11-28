#' AREA (patch level)
#'
#' @description Patch area (Area and edge metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{AREA = a_{ij} * (\frac{1} {10000})}
#' where \eqn{a_{ij}} is the area in square meters.
#'
#' AREA is an 'Area and edge metric' and equals the area of each patch in hectares.
#' The lower limit of AREA is limited by the resolution of the input raster,
#' i.e. AREA can't be smaller than the resolution squared (in hectares). It is one of
#' the most basic, but also most important metrics, to characterise a landscape. The
#' metric is the simplest measure of composition.
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{AREA > 0}
#' \subsection{Behaviour}{Increases, without limit, as the patch size increases.}
#'
#' @seealso
#' \code{\link{lsm_c_area_mn}},
#' \code{\link{lsm_c_area_sd}},
#' \code{\link{lsm_c_area_cv}},
#' \code{\link{lsm_c_ca}}, \cr
#' \code{\link{lsm_l_area_mn}},
#' \code{\link{lsm_l_area_sd}},
#' \code{\link{lsm_l_area_cv}},
#' \code{\link{lsm_l_ta}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_p_area(landscape)
#'
#' @aliases lsm_p_area
#' @rdname lsm_p_area
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_p_area <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_p_area_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}


lsm_p_area_calc <- function(landscape, directions, resolution, extras = NULL){

    if (missing(resolution)) resolution <- terra::res(landscape)

    if (is.null(extras)){
        metrics <- "lsm_p_area"
        landscape <- terra::as.matrix(landscape, wide = TRUE)
        extras <- prepare_extras(metrics, landscape_mat = landscape,
                                            directions = directions, resolution = resolution)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::new_tibble(list(level = "patch",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "area",
                              value = as.double(NA))))
    }

    # get unique class id
    classes <- extras$classes
    class_patches <- extras$class_patches
    area_patches <- extras$area_patches

    area_patch <- do.call(rbind,
                          lapply(classes, function(patches_class){

        # get connected patches
        landscape_labeled <- class_patches[[as.character(patches_class)]]

        # multiply number of cells within each patch with hectar factor
        area_patch_ij <- area_patches[[as.character(patches_class)]]

        tibble::new_tibble(list(
            class = rep(as.integer(patches_class), length(area_patch_ij)),
            value = area_patch_ij))
        })
    )
    return(tibble::new_tibble(list(
        level = rep("patch", nrow(area_patch)),
        class = as.integer(area_patch$class),
        id = as.integer(seq_len(nrow(area_patch))),
        metric = rep("area", nrow(area_patch)),
        value = as.double(area_patch$value)
        )))
}
