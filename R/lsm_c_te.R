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
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_te_calc,
                   count_boundary = count_boundary,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_te
#' @export
lsm_c_te.RasterStack <- function(landscape,
                                 count_boundary = FALSE, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_te_calc,
                   count_boundary = count_boundary,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_te
#' @export
lsm_c_te.RasterBrick <- function(landscape,
                                 count_boundary = FALSE, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_te_calc,
                   count_boundary = count_boundary,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_te
#' @export
lsm_c_te.list <- function(landscape,
                          count_boundary = FALSE, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_te_calc,
                   count_boundary = count_boundary,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_te_calc <- function(landscape, count_boundary, directions) {

    number_classes <- lsm_l_pr_calc(landscape) %>% dplyr::pull(value)

    if(number_classes == 1 && isFALSE(count_boundary)) {

        class_name <- raster::unique(landscape)

        tibble::tibble(
            level = "class",
            class = as.integer(class_name),
            id = as.integer(NA),
            metric = "te",
            value = as.double(0))
    }

    else {

        if(isFALSE(raster::res(landscape)[[1]] == raster::res(landscape)[[2]])){
            top_bottom_matrix <- matrix(c(NA, NA, NA,
                                          1,  0, 1,
                                          NA, NA, NA), 3, 3, byrow = TRUE)

            left_right_matrix <- matrix(c(NA, 1, NA,
                                          NA, 0, NA,
                                          NA, 1, NA), 3, 3, byrow = TRUE)
        }

        landscape_labeled <- get_patches(landscape, directions = directions)

        purrr::map_dfr(landscape_labeled, function(patches_class) {

            class_name <- patches_class %>%
                names() %>%
                sub("Class_", "", .)

            raster::values(patches_class)[is.na(raster::values(
                patches_class))] <- -999

            if(isTRUE(count_boundary)){
                patches_class <- pad_raster(landscape = patches_class,
                                            pad_raster_value = -999,
                                            pad_raster_cells = 1)
            }

            if (isTRUE(raster::res(landscape)[[1]] == raster::res(landscape)[[2]])) {

                neighbor_matrix <- rcpp_get_coocurrence_matrix(raster::as.matrix(patches_class),
                                                  directions = as.matrix(4))

                edge_ik <- (sum(neighbor_matrix[2:ncol(neighbor_matrix), 1])) *
                    raster::res(landscape)[[1]]
            }

            else {

                left_right_neighbours <-
                    rcpp_get_coocurrence_matrix(raster::as.matrix(patches_class),
                                                directions = as.matrix(left_right_matrix))

                edge_ik_left_right <-
                    sum(left_right_neighbours[1 ,2:ncol(left_right_neighbours)]) *
                    raster::res(patches_class)[[1]]

                top_bottom_neighbours <-
                    rcpp_get_coocurrence_matrix(raster::as.matrix(patches_class),
                                                directions = as.matrix(top_bottom_matrix))

                edge_ik_top_bottom <-
                    sum(top_bottom_neighbours[1 ,2:ncol(top_bottom_neighbours)]) *
                    raster::res(patches_class)[[2]]

                edge_ik <- edge_ik_left_right + edge_ik_top_bottom
            }

            tibble::tibble(
                level = "class",
                class = as.integer(class_name),
                id = as.integer(NA),
                metric = "te",
                value = as.double(edge_ik))
        })
    }
}
