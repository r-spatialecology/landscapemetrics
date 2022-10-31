#' AREA_CV (class level)
#'
#' @description Coefficient of variation of patch area (Area and edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{AREA_{CV} = cv(AREA[patch_{ij}])}
#' where \eqn{AREA[patch_{ij}]} is the area of each patch in hectares.
#'
#' AREA_CV is an 'Area and Edge metric'. The metric summarises each class
#' as the Coefficient of variation of all patch areas belonging to class i.
#' The metric describes the differences among patches of the same class i in
#' the landscape and is easily comparable because it is scaled to the mean.
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{AREA_CV >= 0}
#' \subsection{Behaviour}{Equals AREA_CV = 0 if all patches are identical in size.
#' Increases, without limit, as the variation of patch areas increases.}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{cv}}, \cr
#' \code{\link{lsm_c_area_mn}},
#' \code{\link{lsm_c_area_sd}}, \cr
#' \code{\link{lsm_l_area_mn}},
#' \code{\link{lsm_l_area_sd}},
#' \code{\link{lsm_l_area_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_area_cv(landscape)
#'
#' @aliases lsm_c_area_cv
#' @rdname lsm_c_area_cv
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: https://www.umass.edu/landeco/
#'
#' @export
lsm_c_area_cv <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_area_cv_calc,
                     directions = directions)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)

}

lsm_c_area_cv_calc <- function(landscape, directions, resolution = NULL){

    # get area of patches
    area <- lsm_p_area_calc(landscape,
                            directions = directions,
                            resolution = resolution)

    # all values NA
    if (all(is.na(area$value))) {
        return(tibble::tibble(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "area_cv",
                              value = as.double(NA)))
    }

    # calculate cv
    area_cv <- stats::aggregate(area[, 5], by = area[, 2], FUN = raster::cv)

    return(tibble::tibble(level = "class",
                          class = as.integer(area_cv$class),
                          id = as.integer(NA),
                          metric = "area_cv",
                          value = as.double(area_cv$value)))
}
