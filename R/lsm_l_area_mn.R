#' AREA_MN (landscape level)
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
#' AREA_MN is an 'Area and Edge metric'. The metric summarises the landscape
#' as the mean of all patch in the landscape. The metric is a simple way
#' to describe the composition of the landscape. Especially together with the total
#' landscape area (\code{\link{lsm_l_ta}}), it can also give an an idea of patch structure
#' (e.g. many small patches vs. few larges patches). Because the metric is based on
#' distances or areas please make sure your data is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{AREA_MN > 0}
#' \subsection{Behaviour}{Approaches AREA_MN = 0 if all patches are small. Increases, without
#' limit, as the patch areas increase.}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_area_mn}},
#' \code{\link{lsm_c_area_sd}},
#' \code{\link{lsm_c_area_cv}} \cr
#' \code{\link{lsm_l_area_sd}},
#' \code{\link{lsm_l_area_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_area_mn(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_l_area_mn <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_area_mn_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_area_mn_calc <- function(landscape, directions, resolution, extras = NULL){

    # get patch area
    area_patch <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution,
                                  extras = extras)

    # all values NA
    if (all(is.na(area_patch$value))) {
        return(tibble::new_tibble(list(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "area_mn",
                              value = as.double(NA))))
    }

    # calculate mean
    area_mn <- mean(area_patch$value)

    return(tibble::new_tibble(list(level = rep("landscape", length(area_mn)),
                 class = rep(as.integer(NA), length(area_mn)),
                 id = rep(as.integer(NA), length(area_mn)),
                 metric = rep("area_mn", length(area_mn)),
                 value = as.double(area_mn))))
}


