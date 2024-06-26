#' AREA_SD (landscape level)
#'
#' @description Standard deviation of patch area (Area and edge metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{AREA_{SD} = sd(AREA[patch_{ij}])}
#' where \eqn{AREA[patch_{ij}]} is the area of each patch in hectares.
#'
#' AREA_SD is an 'Area and Edge metric'. The metric summarises the landscape
#' as the standard deviation of all patch in the landscape. The metric describes
#' the differences among all patches in the landscape.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{AREA_SD >= 0}
#' \subsection{Behaviour}{Equals AREA_SD = 0 if all patches are identical in size.
#' Increases, without limit, as the variation of patch areas increases.}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link[stats]{sd}} \cr
#' \code{\link{lsm_c_area_mn}},
#' \code{\link{lsm_c_area_sd}},
#' \code{\link{lsm_c_area_cv}} \cr
#' \code{\link{lsm_l_area_mn}},
#' \code{\link{lsm_l_area_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_area_sd(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_l_area_sd <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_area_sd_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

# Not working yet!
lsm_l_area_sd_calc <- function(landscape, directions, resolution, extras = NULL){

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
                              metric = "area_sd",
                              value = as.double(NA))))
    }

    # calculate sd
    area_sd <- stats::sd(area_patch$value)

    return(tibble::new_tibble(list(level = rep("landscape", length(area_sd)),
                 class = rep(as.integer(NA), length(area_sd)),
                 id = rep(as.integer(NA), length(area_sd)),
                 metric = rep("area_sd", length(area_sd)),
                 value = as.double(area_sd))))
}


