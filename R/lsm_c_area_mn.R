#' AREA_MN (class level)
#'
#' @description Mean of patch area (Area and edge metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{AREA_{MN} = mean(AREA[patch_{ij}])}
#' where \eqn{AREA[patch_{ij}]} is the area of each patch in hectares
#'
#' AREA_MN is an 'Area and Edge metric'. The metric summarises each class
#' as the mean of all patch areas belonging to class i. The metric is a simple way
#' to describe the composition of the landscape. Especially together with the total
#' class area (\code{\link{lsm_c_ca}}), it can also give an an idea of patch structure
#' (e.g. many small patches vs. few larges patches).
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{AREA_MN > 0}
#' \subsection{Behaviour}{Approaches AREA_MN = 0 if all patches are small. Increases, without
#' limit, as the patch areas increase.}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_area_cv}},
#' \code{\link{lsm_c_area_sd}}, \cr
#' \code{\link{lsm_l_area_mn}},
#' \code{\link{lsm_l_area_sd}},
#' \code{\link{lsm_l_area_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_area_mn(landscape)
#'
#' @aliases lsm_c_area_mn
#' @rdname lsm_c_area_mn
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_c_area_mn <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_area_mn_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_area_mn_calc <- function(landscape, directions, resolution, extras = NULL){

    # get area of patches
    area <- lsm_p_area_calc(landscape,
                            directions = directions,
                            resolution = resolution,
                            extras = extras)

    # all values NA
    if (all(is.na(area$value))) {
        return(tibble::tibble(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "area_mn",
                              value = as.double(NA)))
    }

    # calculate mean
    area_mean <- stats::aggregate(area[, 5], by = area[, 2], FUN = mean)

    return(tibble::tibble(level = "class",
                          class = as.integer(area_mean$class),
                          id = as.integer(NA),
                          metric = "area_mn",
                          value = as.double(area_mean$value)))
}
