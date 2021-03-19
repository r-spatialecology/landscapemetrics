#' get_boundaries
#'
#' @description Get boundary cells of patches
#'
#' @param landscape RasterLayer or matrix.
#' @param consider_boundary Logical if cells that only neighbour the landscape
#' boundary should be considered as edge.
#' @param edge_depth Distance (in cells) a cell has the be away from the patch
#' edge to be considered as core cell.
#' @param as_NA If true, non-boundary cells area labeld NA.
#' @param patch_id If true, boundary/edge cells are labeled with the original patch id.
#' @param return_raster If false, matrix is returned.
#'
#' @details
#' All boundary/edge cells are labeled 1, all non-boundary cells 0. NA values are
#' not changed. Boundary cells are defined as cells that neighbour either a NA
#' cell or a cell with a different value than itself. Non-boundary cells only
#' neighbour cells with the same value than themself.
#'
#' @return List with RasterLayer or matrix
#'
#' @examples
#' class_1 <- get_patches(landscape, class = 1)[[1]]
#'
#' get_boundaries(class_1)
#' get_boundaries(class_1, return_raster = FALSE)
#'
#' @aliases get_boundaries
#' @rdname get_boundaries
#'
#' @export
get_boundaries <- function(landscape,
                           consider_boundary = FALSE, edge_depth = 1,
                           as_NA = FALSE, patch_id = FALSE, return_raster = TRUE) {

    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape, function(x) {

        result_temp <- get_boundaries_calc(raster::as.matrix(x),
                                           consider_boundary = consider_boundary,
                                           edge_depth = edge_depth,
                                           as_NA = as_NA,
                                           patch_id = patch_id)

        # convert back to raster
        if (return_raster && !inherits(x = x, what = "matrix")) {

            result_temp <- matrix_to_raster(matrix = result_temp,
                                            landscape = x)
        } else if (return_raster && inherits(x = x, what = "matrix")) {

            warning("'return_raster = TRUE' not able for matrix input.",
                    call. = FALSE)

        }

        return(result_temp)
    })

    names(result) <- paste0("layer_", 1:length(result))

    return(result)

}

get_boundaries_calc <- function(landscape,
                                consider_boundary,
                                edge_depth,
                                as_NA,
                                patch_id) {

    # add padding for landscape boundary
    if (!consider_boundary) {

        landscape <- pad_raster(landscape,
                                pad_raster_value = NA,
                                pad_raster_cells = 1,
                                global = FALSE,
                                return_raster = FALSE)[[1]]
    }

    # get boundaries
    landscape_boundaries <- rcpp_get_boundaries(landscape,
                                                directions = 4)

    # loop if edge_depth > 1
    if (edge_depth > 1) {

        # save original landscape
        landscape_boundaries_temp <- landscape_boundaries

        # first edge depth already labels
        for (i in seq_len(edge_depth - 1)) {

            # set all already edge to NA
            landscape_boundaries_temp[landscape_boundaries_temp == 1] <- NA

            # set current_edge + 1 to new edge
            landscape_boundaries_temp <- rcpp_get_boundaries(landscape_boundaries_temp,
                                                             directions = 4)

            landscape_boundaries[which(landscape_boundaries_temp[] == 1)] <- 1
        }
    }

    # remove padded rows/cols
    if (!consider_boundary) {

        landscape_boundaries <- unpad_raster(landscape_boundaries,
                                             unpad_raster_cells = 1,
                                             return_raster = FALSE,
                                             to_disk = FALSE)[[1]]
    }

    # use original patch id
    if (patch_id) {

        # issue if class 0 is present because used for non-edge cells
        present_classes <- rcpp_get_unique_values(landscape)

        if (any(present_classes == 0)) {
           warning("Not able to use original patch id because at least one id equals zero.",
                   call. = FALSE)
        }

        # relabel edge cells (value = 1) with original patch id
        else {

            # remove padded rows/cols
            if (!consider_boundary) {

                landscape <- unpad_raster(landscape,
                                          unpad_raster_cells = 1,
                                          return_raster = FALSE,
                                          to_disk = FALSE)[[1]]
            }

            landscape_boundaries[landscape_boundaries == 1 &
                                     !is.na(landscape_boundaries)] <- landscape[landscape_boundaries == 1 &
                                                                                    !is.na(landscape_boundaries)]
        }
    }

    # convert all 0 as NA
    if (as_NA) {

        landscape_boundaries[which(landscape_boundaries == 0)] <- NA
    }

    return(landscape_boundaries)
}
