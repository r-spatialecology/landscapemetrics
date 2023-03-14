#' TE (landscape level)
#'
#' @description Total edge (Area and Edge metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param count_boundary Include landscape boundary in edge length
#'
#' @details
#' \deqn{TE = \sum \limits_{k = 1}^{m} e_{ik}}
#' where \eqn{e_{ik}} is the edge lengths in meters.

#' TE is an 'Area and edge metric'. Total edge includes all edges. It measures the
#' configuration of the landscape because a highly fragmented landscape will have many
#' edges. However, total edge is an absolute measure, making comparisons among landscapes
#' with different total areas difficult. If \code{count_boundary = TRUE} also edges to the
#' landscape boundary are included.
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{TE >= 0}
#' \subsection{Behaviour}{Equals TE = 0 if all cells are edge cells. Increases, without limit,
#' as landscape becomes more fragmented}
#'
#' @seealso
#' \code{\link{lsm_p_perim}}
#' \code{\link{lsm_l_te}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_te(terra::unwrap(landscape))
#'
#' @aliases lsm_l_te
#' @rdname lsm_l_te
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: https://www.umass.edu/landeco/
#'
#' @export
lsm_l_te <- function(landscape, count_boundary = FALSE) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_te_calc,
                     count_boundary = count_boundary)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_te_calc <- function(landscape, count_boundary, resolution = NULL){

    # conver raster to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        resolution <- terra::res(landscape)

        landscape <-terra::as.matrix(landscape, wide = TRUE)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "te",
                              value = as.double(NA)))
    }

    # get resolution in x-y directions
    resolution_x <- resolution[[1]]
    resolution_y <- resolution[[2]]

    if (count_boundary) {

        # get background value not present as class
        background_value <- max(landscape, na.rm = TRUE) + 1

        # add row/col around raster
        landscape <- pad_raster_internal(landscape = landscape,
                                         pad_raster_value = background_value,
                                         pad_raster_cells = 1, global = FALSE)

        # set NA to background value
        landscape[is.na(landscape)] <- background_value
    }

    if (resolution_x == resolution_y) {

        neighbor_matrix <- rcpp_get_coocurrence_matrix(landscape,
                                                       directions = as.matrix(4))

        edge_total <- sum(neighbor_matrix[lower.tri(neighbor_matrix)]) * resolution_x

    } else {

        top_bottom_matrix <- matrix(c(NA, NA, NA,
                                      1,  0, 1,
                                      NA, NA, NA), 3, 3, byrow = TRUE)

        left_right_matrix <- matrix(c(NA, 1, NA,
                                      NA, 0, NA,
                                      NA, 1, NA), 3, 3, byrow = TRUE)

        left_right_neighbours <-
            rcpp_get_coocurrence_matrix(landscape,
                                        directions = as.matrix(left_right_matrix))

        edge_left_right <-
            sum(left_right_neighbours[lower.tri(left_right_neighbours)]) * resolution_x

        top_bottom_neighbours <-
            rcpp_get_coocurrence_matrix(terra::as.matrix(landscape, wide = TRUE),
                                        directions = as.matrix(top_bottom_matrix))

        edge_top_bottom <-
            sum(top_bottom_neighbours[lower.tri(top_bottom_neighbours)]) * resolution_y

        edge_total <- edge_left_right + edge_top_bottom
    }

    return(tibble::tibble(level = "landscape",
                          class = as.integer(NA),
                          id = as.integer(NA),
                          metric = "te",
                          value = as.double(edge_total)))
}
