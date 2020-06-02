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
                           consider_boundary,
                           edge_depth,
                           as_NA,
                           return_raster) UseMethod("get_boundaries")

#' @name get_boundaries
#' @export
get_boundaries.RasterLayer <- function(landscape,
                                       consider_boundary = FALSE,
                                       edge_depth = 1,
                                       as_NA = FALSE,
                                       return_raster = TRUE) {

    # get boundaries
    result <- lapply(raster::as.list(landscape), function(x) {

        result_temp <- get_boundaries.matrix(raster::as.matrix(x),
                                             consider_boundary = consider_boundary,
                                             edge_depth = edge_depth,
                                             as_NA = as_NA)[[1]]

        # convert back to raster
        if (return_raster) {

            result_temp <- matrix_to_raster(matrix = result_temp,
                                            landscape = landscape)
        }

        return(result_temp)
    })

    return(result)
}

#' @name get_boundaries
#' @export
get_boundaries.RasterStack <- function(landscape,
                                       consider_boundary = FALSE,
                                       edge_depth = 1,
                                       as_NA = FALSE,
                                       return_raster = TRUE) {

    # get boundaries
    result <- lapply(X = raster::as.list(landscape), function(x) {

        result_temp <- get_boundaries.matrix(raster::as.matrix(x),
                                             consider_boundary = consider_boundary,
                                             edge_depth = edge_depth,
                                             as_NA = as_NA)[[1]]

        # convert back to raster
        if (return_raster) {

            result_temp <- matrix_to_raster(matrix = result_temp,
                                            landscape = x)
        }

        return(result_temp)
    })

    return(result)
}

#' @name get_boundaries
#' @export
get_boundaries.RasterBrick <- function(landscape,
                                       consider_boundary = FALSE,
                                       edge_depth = 1,
                                       as_NA = FALSE,
                                       return_raster = TRUE) {

    # get boundaries
    result <- lapply(X = raster::as.list(landscape), function(x) {

        result_temp <- get_boundaries.matrix(raster::as.matrix(x),
                                             consider_boundary = consider_boundary,
                                             edge_depth = edge_depth,
                                             as_NA = as_NA)[[1]]

        # convert back to raster
        if (return_raster) {

            result_temp <- matrix_to_raster(matrix = result_temp,
                                            landscape = x)
        }

        return(result_temp)
    })

    return(result)
}

#' @name get_boundaries
#' @export
get_boundaries.stars <- function(landscape,
                                 consider_boundary = FALSE,
                                 edge_depth = 1,
                                 as_NA = FALSE,
                                 return_raster = TRUE) {

    # convert as raster
    landscape <- methods::as(landscape, "Raster")

    # get boundaries
    result <- lapply(X = raster::as.list(landscape), function(x) {

        result_temp <- get_boundaries.matrix(raster::as.matrix(x),
                                             consider_boundary = consider_boundary,
                                             edge_depth = edge_depth,
                                             as_NA = as_NA)[[1]]

        # convert back to raster
        if (return_raster) {

            result_temp <- matrix_to_raster(matrix = result_temp,
                                            landscape = x)
        }

        return(result_temp)
    })

    return(result)
}

#' @name get_boundaries
#' @export
get_boundaries.list <- function(landscape,
                                consider_boundary = FALSE,
                                edge_depth = 1,
                                as_NA = FALSE,
                                return_raster = TRUE) {

    # get boundaries
    result <- lapply(X = landscape, function(x) {

        result_temp <- get_boundaries.matrix(raster::as.matrix(x),
                                             consider_boundary = consider_boundary,
                                             edge_depth = edge_depth,
                                             as_NA = as_NA)[[1]]

        # convert back to raster
        if (return_raster) {

            result_temp <- matrix_to_raster(matrix = result_temp,
                                            landscape = x)
        }

        return(result_temp)
    })

    return(result)
}

#' @name get_boundaries
#' @export
get_boundaries.matrix <- function(landscape,
                                  consider_boundary = FALSE,
                                  edge_depth = 1,
                                  as_NA = FALSE,
                                  return_raster = FALSE) {

    if (return_raster) {

        warning("'return_raster = TRUE' not able for matrix input.",
                call. = FALSE)
    }

    # add padding for landscape boundary
    if (!consider_boundary) {

        landscape <- pad_raster(landscape,
                                pad_raster_value = NA,
                                pad_raster_cells = 1,
                                global = FALSE,
                                return_raster = FALSE)[[1]]
    }

    # get boundaries
    landscape <- rcpp_get_boundaries(landscape,
                                     directions = 4)

    # loop if edge_depth > 1
    if (edge_depth > 1) {

        # save original landscape
        landscape_temp <- landscape

        # first edge depth already labels
        for (i in seq_len(edge_depth - 1)) {

            # set all already edge to NA
            landscape_temp[landscape_temp == 1] <- NA

            # set current_edge + 1 to new edge
            landscape_temp <- rcpp_get_boundaries(landscape_temp,
                                                  directions = 4)

            landscape[which(landscape_temp[] == 1)] <- 1
        }
    }

    # remove padded rows/cols
    if (!consider_boundary) {

        landscape <- unpad_raster(landscape,
                                  unpad_raster_cells = 1,
                                  return_raster = FALSE,
                                  to_disk = FALSE)[[1]]
    }

    # convert all 0 as NA
    if (as_NA) {

        landscape[which(landscape == 0)] <- NA
    }

    return(list(landscape))
}
