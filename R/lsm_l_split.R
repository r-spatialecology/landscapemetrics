#' SPLIT (landscape level)
#'
#' @description Splitting index (Aggregation metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{SSPLIT = \frac{A^2} {\sum \limits_{i = 1}^{m} \sum \limits_{j = 1}^{n} a_{ij}^2}}
#' where \eqn{a_{ij}} is the patch area in square meters and \eqn{A} is the
#' total landscape area.
#'
#' SPLIT is an 'Aggregation metric'. It describes the number of patches if all patches the
#' landscape would be divided into equally sized patches.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{1 <= SPLIT <= Number of cells squared}
#' \subsection{Behaviour}{Equals SPLIT = 1 if only one patch is present. Increases as
#' the number of patches increases and is limited if all cells are a patch}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{lsm_l_ta}}, \cr
#' \code{\link{lsm_c_split}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_split(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' Jaeger, J. A. 2000. Landscape division, splitting index, and effective mesh
#' size: new measures of landscape fragmentation.
#' Landscape ecology, 15(2), 115-130.
#'
#' @export
lsm_l_split <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_split_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_split_calc <- function(landscape, directions, resolution, extras = NULL) {

    # get patch area
    area_patch <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution,
                                  extras = extras)

    # summarise for total landscape
    area_total <- sum(area_patch$value)

    # all values NA
    if (is.na(area_total)) {
        return(tibble::new_tibble(list(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "split",
                              value = as.double(NA))))
    }

    # total area squared divided by sum of area squared for each patch
    split <- (area_total ^ 2) / sum(area_patch$value ^ 2)

    return(tibble::new_tibble(list(level = rep("landscape", length(split)),
                          class = rep(as.integer(NA), length(split)),
                          id = rep(as.integer(NA), length(split)),
                          metric = rep("split", length(split)),
                          value = as.double(split))))
}
