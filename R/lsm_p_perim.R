#' PERIM (patch level)
#'
#' @description Perimeter (Area and edge metric))
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{PERIM = p_{ij}}
#' where \eqn{p_{ij}} is the perimeter in meters.
#'
#' PERIM is an 'Area and edge metric'. It equals the perimeter of the patch
#' including also the edge to the landscape boundary. The metric describes
#' patch area (larger perimeter for larger patches), but also patch shape
#' (large perimeter for irregular shapes).
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{PERIM > 0}
#' \subsection{Behaviour}{Increases, without limit, as patch size and
#' complexity increases.}
#'
#' @return tibble
#'
#' @examples
#' lsm_p_perim(landscape)
#'
#' @aliases lsm_p_perim
#' @rdname lsm_p_perim
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_p_perim <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_p_perim_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_p_perim_calc <- function(landscape, directions, resolution = NULL) {

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        resolution <- raster::res(landscape)
        landscape <- raster::as.matrix(landscape)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "patch",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "perim",
                              value = as.double(NA)))
    }

    # get dimensions of raster
    resolution_x <- resolution[[1]]
    resolution_y <- resolution[[2]]

    # get unique classes
    classes <- get_unique_values_int(landscape, verbose = FALSE)

    # raster resolution not identical in x-y directions
    if (!resolution_x == resolution_y) {

        top_bottom_matrix <- matrix(c(NA, NA, NA,
                                      1,  0, 1,
                                      NA, NA, NA), 3, 3, byrow = TRUE)

        left_right_matrix <- matrix(c(NA, 1, NA,
                                      NA, 0, NA,
                                      NA, 1, NA), 3, 3, byrow = TRUE)
    }

    perimeter_patch <- do.call(rbind,
                               lapply(classes, function(patches_class) {

        # get connected patches
        landscape_labeled <- get_patches_int(landscape,
                                             class = patches_class,
                                             directions = directions)[[1]]

        # cells at the boundary of the landscape need neighbours to calculate perim
        landscape_labeled <- pad_raster_internal(landscape_labeled,
                                                 pad_raster_value = NA,
                                                 pad_raster_cells = 1,
                                                 global = FALSE)

        # which cells are NA (i.e. background)
        target_na <- which(is.na(landscape_labeled))

        # set all NA to -999 to get adjacencies between patches and all background
        landscape_labeled[target_na] <- -999

        # x-y resolution is identical
        if (resolution_x == resolution_y) {

            # get coocurrence matrix
            neighbour_matrix <- rcpp_get_coocurrence_matrix(landscape_labeled,
                                                            directions = as.matrix(4))

            # get adjacencies between patches and background cells (-999 always first row of matrix) and convert to perimeter
            perimeter_patch_ij <- neighbour_matrix[1, 2:ncol(neighbour_matrix)] * resolution_x
        }

        # x-y resolution not identical, count adjacencies seperatly for x- and y-direction
        else {

            # get coocurrence matrix in x-direction
            left_right_neighbours <- rcpp_get_coocurrence_matrix(landscape_labeled,
                                                                 directions = as.matrix(left_right_matrix))

            # get adjacencies between patches and background cells (-999 always first row of matrix) and convert to perimeter
            perimeter_patch_ij_left_right <- left_right_neighbours[1, 2:ncol(left_right_neighbours)] * resolution_x

            # get coocurrennce matrix in y-direction
            top_bottom_neighbours <- rcpp_get_coocurrence_matrix(landscape_labeled,
                                                                 directions = as.matrix(top_bottom_matrix))

            # get adjacencies between patches and background cells (-999 always first row of matrix) and convert to perimeter
            perimeter_patch_ij_top_bottom <- top_bottom_neighbours[1, 2:ncol(top_bottom_neighbours)] * resolution_y

            # add perim of both directions for each patch
            perimeter_patch_ij <- perimeter_patch_ij_top_bottom + perimeter_patch_ij_left_right
        }

        tibble::tibble(class = patches_class,
                       value = perimeter_patch_ij)
        })
    )

    tibble::tibble(
        level = "patch",
        class = as.integer(perimeter_patch$class),
        id = as.integer(seq_len(nrow(perimeter_patch))),
        metric = "perim",
        value = as.double(perimeter_patch$value)
    )
}
