#' TE (class level)
#'
#' @description Total (class) edge (Area and Edge metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
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
#' \code{count_boundary = TRUE} also edges to the landscape boundary are included.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
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
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_te(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_c_te <- function(landscape,
                          count_boundary = FALSE, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         te <- lsm_c_te_calc(
                             landscape_mat = landscape_mat,
                             count_boundary = count_boundary,
                             directions = directions,
                             resolution = resolution
                         )

                         lsm_class_output(metric = "te",
                                          class = as.integer(names(te)),
                                          value = unname(te))
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_te_calc <- function(landscape_mat, count_boundary = FALSE, directions = NULL, resolution = NULL,
                          classes = NULL, class_patches = NULL) {

    # all values NA
    if (all(is.na(landscape_mat))) {
        return(stats::setNames(as.double(NA), NA_character_))
    }

    # lazy dependency resolution
    if (is.null(classes) || is.null(class_patches)) {
        deps <- resolve_extras(
            landscape_mat = landscape_mat,
            directions = directions,
            required = c("classes", "class_patches"),
            resolution = resolution
        )
        classes <- deps$classes
        class_patches <- deps$class_patches
    }

    # get class id
    resolution_x <- resolution[[1]]
    resolution_y <- resolution[[2]]

    if (length(classes) == 1 && !count_boundary) {

        stats::setNames(as.double(0), as.character(classes))

    } else {

        # resolution not identical in x and y direction
        if (resolution_x != resolution_y) {

            top_bottom_matrix <- matrix(c(NA, NA, NA,
                                          1,  0, 1,
                                          NA, NA, NA), 3, 3, byrow = TRUE)

            left_right_matrix <- matrix(c(NA, 1, NA,
                                          NA, 0, NA,
                                          NA, 1, NA), 3, 3, byrow = TRUE)
        }

        te_class <- do.call(rbind, lapply(classes, function(patches_class) {

            # get connected patches
            patch_mat <- class_patches[[as.character(patches_class)]]

            # set all non-class patches, but not NAs, to -999
            edge_cells <- which(!is.na(landscape_mat) & landscape_mat != patches_class)

            patch_mat[edge_cells] <- -999

            # add one row/column to count landscape boundary
            if (count_boundary) {
                patch_mat <- pad_raster_internal(landscape = patch_mat,
                                                         pad_raster_value = -999,
                                                         pad_raster_cells = 1,
                                                         global = FALSE)

                # set NA to -999
                patch_mat[is.na(patch_mat)] <- -999
            }

            # x-y resolution is identical
            if (resolution_x == resolution_y) {

                # get coocurrence matrix
                neighbour_matrix <- rcpp_get_coocurrence_matrix_single(patch_mat,
                                                                directions = as.matrix(4),
                                                                single_class = -999)

                # get adjacencies between patches and background cells (-999 always first row of matrix) and convert to edge length
                te_ik <- sum(neighbour_matrix[2:nrow(neighbour_matrix), 1]) * resolution_x

            # x-y resolution not identical, count adjacencies separately for x- and y-direction
            } else {

                # get coocurrence matrix in x-direction
                left_right_neighbours <- rcpp_get_coocurrence_matrix_single(patch_mat,
                                                                     directions = as.matrix(left_right_matrix),
                                                                     single_class = -999)

                # get adjacencies between patches and background cells (-999 always first row of matrix) and convert to edge length
                te_ik_left_right <- sum(left_right_neighbours[2:nrow(left_right_neighbours), 1]) * resolution_x

                # get coocurrennce matrix in y-direction
                top_bottom_neighbours <- rcpp_get_coocurrence_matrix_single(patch_mat,
                                                                     directions = as.matrix(top_bottom_matrix),
                                                                     single_class = -999)

                # get adjacencies between patches and background cells (-999 always first row of matrix) and convert to edge length
                te_ik_top_bottom <- sum(top_bottom_neighbours[2:nrow(top_bottom_neighbours), 1]) * resolution_y

                # add te of both directions for each patch
                te_ik <- te_ik_top_bottom + te_ik_left_right
            }

            tibble::new_tibble(list(class = patches_class,
                           value = te_ik))
            })
        )

        # return named vector
        stats::setNames(as.double(te_class$value), as.character(te_class$class))
    }
}
