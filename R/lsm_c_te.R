#' TE (class level)
#'
#' @description Total (class) edge (Area and Edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param count_boundary Include landscape boundary in edge length
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{TE = \sum \limits_{k = 1}^{m} e_{ik}}
#' where \eqn{e_{ik}} is the edge lengths in meters.

#' TE is an 'Area and edge metric'. Total (class) edge includes all edges between class i and
#' all other classes k. It measures the configuration of the landscape because a highly
#' fragmented landscape will have many edges. However, total edge is an absolute measure,
#' making comparisons among landscapes with different total areas difficult. If
#' \code{cound_boundary = TRUE} also edges to the landscape boundary are included.
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
#' lsm_c_te(landscape)
#'
#' @aliases lsm_c_te
#' @rdname lsm_c_te
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_te <- function(landscape,
                     count_boundary, directions) UseMethod("lsm_c_te")

#' @name lsm_c_te
#' @export
lsm_c_te.RasterLayer <- function(landscape,
                                 count_boundary = FALSE, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_te_calc,
                     count_boundary = count_boundary,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_c_te
#' @export
lsm_c_te.RasterStack <- function(landscape,
                                 count_boundary = FALSE, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_te_calc,
                     count_boundary = count_boundary,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_c_te
#' @export
lsm_c_te.RasterBrick <- function(landscape,
                                 count_boundary = FALSE, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_te_calc,
                     count_boundary = count_boundary,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_c_te
#' @export
lsm_c_te.stars <- function(landscape,
                           count_boundary = FALSE, directions = 8) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_te_calc,
                     count_boundary = count_boundary,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}


#' @name lsm_c_te
#' @export
lsm_c_te.list <- function(landscape,
                          count_boundary = FALSE, directions = 8) {

    result <- lapply(X = landscape,
                     FUN = lsm_c_te_calc,
                     count_boundary = count_boundary,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_te_calc <- function(landscape, count_boundary, directions, resolution = NULL) {

    # conver raster to matrix
    if (class(landscape) != "matrix") {
        resolution <- raster::res(landscape)
        landscape <- raster::as.matrix(landscape)
    }

    # get resolution in x-y directions
    resolution_x <- resolution[[1]]
    resolution_y <- resolution[[2]]

    # get class id
    classes <- get_unique_values(landscape)[[1]]

    if(length(classes) == 1 && !count_boundary) {

        tibble::tibble(
            level = "class",
            class = as.integer(classes),
            id = as.integer(NA),
            metric = "te",
            value = as.double(0))
    }

    else {

        # resolution not identical in x and y direction
        if(resolution_x != resolution_y){

            top_bottom_matrix <- matrix(c(NA, NA, NA,
                                          1,  0, 1,
                                          NA, NA, NA), 3, 3, byrow = TRUE)

            left_right_matrix <- matrix(c(NA, 1, NA,
                                          NA, 0, NA,
                                          NA, 1, NA), 3, 3, byrow = TRUE)
        }

        do.call(rbind, lapply(X = classes, FUN = function(patches_class) {

            # get connected patches
            landscape_labeled <- get_patches(landscape,
                                             class = patches_class,
                                             directions = directions,
                                             return_raster = FALSE)[[1]]

            # set all non-class patches to -999
            landscape_labeled[is.na(landscape_labeled)] <- -999

            # add one row/coloumn to count landscape boundary
            if(count_boundary){
                landscape_labeled <- pad_raster(landscape = landscape_labeled,
                                                pad_raster_value = -999,
                                                pad_raster_cells = 1,
                                                return_raster = FALSE)[[1]]
            }

            # resolution identical in x and y direction
            if (resolution_x == resolution_y) {

                # get adjacencies
                neighbor_matrix <- rcpp_get_coocurrence_matrix(landscape_labeled,
                                                               directions = as.matrix(4))


                # sum of all adjacencies between patch id and non-class patches (-999) converted to edge length
                edge_ik <- (sum(neighbor_matrix[2:ncol(neighbor_matrix), 1])) * resolution_x
            }

            else {

                # get adjacencies
                left_right_neighbours <- rcpp_get_coocurrence_matrix(landscape_labeled,
                                                                     directions = as.matrix(left_right_matrix))

                # sum of all adjacencies between patch id and non-class patches (-999) converted to edge length
                edge_ik_left_right <- sum(left_right_neighbours[1 ,2:ncol(left_right_neighbours)]) * resolution_x

                # get adjacencies
                top_bottom_neighbours <- rcpp_get_coocurrence_matrix(landscape_labeled,
                                                                     directions = as.matrix(top_bottom_matrix))

                # sum of all adjacencies between patch id and non-class patches (-999) converted to edge length
                edge_ik_top_bottom <- sum(top_bottom_neighbours[1 ,2:ncol(top_bottom_neighbours)]) * resolution_y

                # add edge length in x- and y-direction
                edge_ik <- edge_ik_left_right + edge_ik_top_bottom
            }

            tibble::tibble(
                level = "class",
                class = as.integer(patches_class),
                id = as.integer(NA),
                metric = "te",
                value = as.double(edge_ik))
            })
        )
    }
}
