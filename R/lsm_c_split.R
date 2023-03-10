#' SPLIT (class level)
#'
#' @description Splitting index (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{SPLIT = \frac{A^2} {\sum \limits_{j = 1}^{n} a_{ij}^2}}
#' where \eqn{a_{ij}} is the patch area in square meters and \eqn{A} is the
#' total landscape area.
#'
#' SPLIT is an 'Aggregation metric'. It describes the number of patches if all patches of
#' class i would be divided into equally sized patches.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{1 <= SPLIT <= Number of cells squared}
#' \subsection{Behaviour}{Equals SPLIT = 1 if only one patch is present. Increases as
#' the number of patches of class i increases and is limited if all cells are a patch}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{lsm_l_ta}}, \cr
#' \code{\link{lsm_l_split}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_split(terra::unwrap(landscape))
#'
#' @aliases lsm_c_split
#' @rdname lsm_c_split
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: https://www.umass.edu/landeco/
#'
#' Jaeger, J. A. 2000. Landscape division, splitting index, and effective mesh
#' size: new measures of landscape fragmentation.
#' Landscape ecology, 15(2), 115-130.
#'
#' @export
lsm_c_split <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_split_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_split_calc <- function(landscape, directions, resolution = NULL) {

    # get patch area
    area_patch <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution)

    # summarise to total area
    area_total <- sum(area_patch$value)

    # all values NA
    if (is.na(area_total)) {
        return(tibble::tibble(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "split",
                              value = as.double(NA)))
    }

    # calculate split for each patch
    area_patch$value <- area_patch$value ^ 2

    # summarise for each class
    split <- stats::aggregate(x = area_patch[, 5], by = area_patch[, 2], FUN = sum)

    # calculate split
    split$value <- (area_total ^ 2) / split$value

    return(tibble::tibble(level = "class",
                          class = as.integer(split$class),
                          id = as.integer(NA),
                          metric = "split",
                          value = as.double(split$value)))
}
