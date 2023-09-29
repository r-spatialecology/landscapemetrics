#' PD (landscape level)
#'
#' @description Patch density (Aggregation metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{PD = \frac{N} {A} * 10000 * 100}
#' where \eqn{N} is the number of patches and \eqn{A} is the total landscape
#' area in square meters.
#'
#' PD is an 'Aggregation metric'. It describes the fragmentation the landscape, however,
#' does not necessarily contain information about the configuration or composition of the
#' landscape. In contrast to \code{\link{lsm_l_np}} it is standardized to the area and
#' comparisons among landscapes with different total area are possible.
#'
#' \subsection{Units}{Number per 100 hectares}
#' \subsection{Ranges}{0 < PD <= 1e+06}
#' \subsection{Behaviour}{Increases as the landscape gets more patchy. Reaches its maximum
#' if every cell is a different patch.}
#'
#' @seealso
#' \code{\link{lsm_c_np}},
#' \code{\link{lsm_l_ta}}, \cr
#' \code{\link{lsm_c_pd}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_pd(landscape)
#'
#' @aliases lsm_l_pd
#' @rdname lsm_l_pd
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_l_pd <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_pd_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_pd_calc <- function(landscape, directions, resolution, extras = NULL) {

    if (missing(resolution)) resolution <- terra::res(landscape)

    if (is.null(extras)){
        metrics <- "lsm_l_pd"
        landscape <- terra::as.matrix(landscape, wide = TRUE)
        extras <- prepare_extras_nonspatial(metrics, landscape = landscape,
                                            directions = directions, resolution = resolution)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::new_tibble(list(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "pd",
                              value = as.double(NA))))
    }

    # get patch area
    area_patch <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution,
                                  extras = extras)

    # summarise for total landscape
    area_total <- sum(area_patch$value)

    # number of patches for each class
    number_patches <- lsm_c_np_calc(landscape,
                                    directions = directions,
                                    extras = extras)

    # summarise for total landscape
    number_patches <- sum(number_patches$value)

    # relative patch density
    patch_density <- number_patches / area_total * 100

    return(tibble::new_tibble(list(level = rep("landscape", length(patch_density)),
                          class = rep(as.integer(NA), length(patch_density)),
                          id = rep(as.integer(NA), length(patch_density)),
                          metric = rep("pd", length(patch_density)),
                          value = as.double(patch_density))))
}
