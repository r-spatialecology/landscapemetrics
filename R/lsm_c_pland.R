#' PLAND (class level)
#'
#' @description Percentage of landscape of class (Area and Edge metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{PLAND = \frac{\sum \limits_{j = 1}^{n} a_{ij}} {A} * 100}
#' where \eqn{a_{ij}} is the area of each patch and \eqn{A} is the total
#' landscape area.
#'
#' PLAND is an 'Area and edge metric'. It is the percentage of the landscape
#' belonging to class i. It is a measure of composition and because of the relative
#' character directly comparable among landscapes with different total areas.
#'
#' \subsection{Units}{Percentage}
#' \subsection{Range}{0 < PLAND <= 100}
#' \subsection{Behaviour}{Approaches PLAND = 0 when the proportional class area is decreasing.
#' Equals PLAND = 100 when only one patch is present.}
#'
#' @seealso
#' \code{\link{lsm_c_ca}},
#' \code{\link{lsm_l_ta}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_pland(landscape)
#'
#' @aliases lsm_c_pland
#' @rdname lsm_c_pland
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_c_pland <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_pland_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_pland_calc <- function(landscape, directions, resolution, extras = NULL){

    if (missing(resolution)) resolution <- terra::res(landscape)

    if (is.null(extras)){
        metrics <- "lsm_c_pland"
        landscape <- terra::as.matrix(landscape, wide = TRUE)
        extras <- prepare_extras(metrics, landscape_mat = landscape,
                                            directions = directions, resolution = resolution)
    }

    pland <- lsm_p_area_calc(landscape,
                             directions = directions,
                             resolution = resolution,
                             extras = extras)

    # all values NA
    if (all(is.na(pland$value))) {
        return(tibble::new_tibble(list(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "pland",
                              value = as.double(NA))))
    }

    pland <- stats::aggregate(x = pland[, 5], by = pland[, 2], FUN = sum)

    pland$value <- pland$value / sum(pland$value) * 100

    return(tibble::new_tibble(list(level = rep("class", nrow(pland)),
                              class = as.integer(pland$class),
                              id = rep(as.integer(NA), nrow(pland)),
                              metric = rep("pland", nrow(pland)),
                              value = as.double(pland$value))))
}

