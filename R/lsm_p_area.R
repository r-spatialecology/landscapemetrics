#' AREA (patch level)
#'
#' @description Patch area (Area and edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
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
#' lsm_p_area(landscape)
#'
#' @aliases lsm_p_area
#' @rdname lsm_p_area
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
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


lsm_p_area_calc <- function(landscape, directions, resolution = NULL){

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        resolution <- raster::res(landscape)

        landscape <- raster::as.matrix(landscape)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "patch",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "area",
                              value = as.double(NA)))
    }

    # factor to convert cell to area
    factor_ha <- prod(resolution) / 10000

    # get unique class id
    classes <- get_unique_values_int(landscape, verbose = FALSE)

    area_patch <- do.call(rbind,
                          lapply(classes, function(patches_class){

        # get connected patches
        landscape_labeled <- get_patches_int(landscape,
                                             class = patches_class,
                                             directions = directions)[[1]]

        # multiply number of cells within each patch with hectar factor
        area_patch_ij <- rcpp_get_composition_vector(x = landscape_labeled) * factor_ha

        tibble::tibble(
            class = as.integer(patches_class),
            value = area_patch_ij
        )
        })
    )

    return(tibble::tibble(
        level = "patch",
        class = as.integer(area_patch$class),
        id = as.integer(seq_len(nrow(area_patch))),
        metric = "area",
        value = as.double(area_patch$value)
        )
    )
}
