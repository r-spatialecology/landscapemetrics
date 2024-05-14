#' ED (class level)
#'
#' @description Edge Density (Area and Edge metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param count_boundary Count landscape boundary as edge.
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{ED = \frac{\sum \limits_{k = 1}^{m} e_{ik}} {A} * 10000}
#' where \eqn{e_{ik}} is the total edge length in meters and \eqn{A} is the total
#' landscape area in square meters.
#'
#' ED is an 'Area and Edge metric'. The edge density equals the sum of all edges of class i
#' in relation to the landscape area. The boundary of the landscape is only included in the
#' corresponding total class edge length if \code{count_boundary = TRUE}.
#' The metric describes the configuration of the landscape, e.g. because  an  aggregation
#' of the same class will result in a low edge density. The metric is standardized to the
#' total landscape area, and therefore comparisons among landscapes with different total
#' areas are possible.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Meters per hectare}
#' \subsection{Range}{ED >= 0}
#' \subsection{Behaviour}{Equals ED = 0 if only one patch is present (and the landscape
#' boundary is not included) and increases, without limit, as the landscapes becomes more
#' patchy}
#'
#' @seealso
#' \code{\link{lsm_c_te}},
#' \code{\link{lsm_l_ta}}, \cr
#' \code{\link{lsm_l_ed}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_ed(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_c_ed <- function(landscape,
                          count_boundary = FALSE,
                          directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_ed_calc,
                     count_boundary = count_boundary,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_ed_calc <- function(landscape, count_boundary, directions, resolution, extras = NULL) {

    if (missing(resolution)) resolution <- terra::res(landscape)

    if (is.null(extras)){
        metrics <- "lsm_c_ed"
        landscape <- terra::as.matrix(landscape, wide = TRUE)
        extras <- prepare_extras(metrics, landscape_mat = landscape,
                                            directions = directions, resolution = resolution)
    }

    # all cells are NA
    if (all(is.na(landscape))) {
        return(tibble::new_tibble(list(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "ed",
                              value = as.double(NA))))
    }

    # get patch area
    area <- lsm_p_area_calc(landscape,
                            directions = directions,
                            resolution = resolution,
                            extras = extras)

    # summarise to total area
    area <- sum(area$value)

    # get total edge length
    edge_class <- lsm_c_te_calc(landscape,
                                count_boundary = count_boundary,
                                directions = directions,
                                resolution = resolution,
                                extras = extras)

    edge_class$value <- edge_class$value / area

    return(tibble::new_tibble(list(level = rep("class", nrow(edge_class)),
                          class = as.integer(edge_class$class),
                          id = rep(as.integer(NA), nrow(edge_class)),
                          metric = rep("ed", nrow(edge_class)),
                          value = as.double(edge_class$value))))
}
