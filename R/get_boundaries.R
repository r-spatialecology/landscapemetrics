#' get_boundaries
#'
#' @description Get boundary cells of patches
#'
#' @param landscape RasterLayer or matrix.
#' @param directions Rook's case (4 neighbours) or queen's case (8 neighbours) should be used as neighbourhood rule
#' @param consider_boundary Logical if cells that only neighbour the landscape
#' boundary should be considered as edge
#' @param as_NA If true, non-boundary cells area labeld NA
#' @param return_raster If false, matrix is returned
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
#' class_1_matrix <- raster::as.matrix(class_1)
#' get_boundaries(class_1_matrix, return_raster = FALSE)
#'
#' @aliases get_boundaries
#' @rdname get_boundaries
#'
#' @export
get_boundaries <- function(landscape,
                           directions,
                           as_NA,
                           consider_boundary,
                           return_raster) UseMethod("get_boundaries")

#' @name get_boundaries
#' @export
get_boundaries.RasterLayer <- function(landscape,
                                       directions = 4,
                                       as_NA = FALSE,
                                       consider_boundary = FALSE,
                                       return_raster = TRUE) {

    # check if either directions are possible
    if (directions != 4 && directions != 8) {

        stop("Please specify 'directions = 4' or 'directions = 8'.", call. = FALSE)
    }

    # get boundaries
    result <- lapply(raster::as.list(landscape), function(x) {

        result_temp <- get_boundaries.matrix(raster::as.matrix(x),
                                             directions = directions,
                                             consider_boundary = consider_boundary,
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
                                       directions = 4,
                                       consider_boundary = FALSE,
                                       as_NA = FALSE,
                                       return_raster = TRUE) {

    # check if either directions are possible
    if (directions != 4 && directions != 8) {
        stop("Please specify 'directions = 4' or 'directions = 8'.", call. = FALSE)
    }

    # get boundaries
    result <- lapply(X = raster::as.list(landscape), function(x) {

        result_temp <- get_boundaries.matrix(raster::as.matrix(x),
                                             directions = directions,
                                             consider_boundary = consider_boundary,
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
get_boundaries.RasterBrick <- function(landscape,
                                       directions = 4,
                                       consider_boundary = FALSE,
                                       as_NA = FALSE,
                                       return_raster = TRUE) {

    # check if either directions are possible
    if (directions != 4 && directions != 8) {
        stop("Please specify 'directions = 4' or 'directions = 8'.", call. = FALSE)
    }

    # get boundaries
    result <- lapply(X = raster::as.list(landscape), function(x) {

        result_temp <- get_boundaries.matrix(raster::as.matrix(x),
                                             directions = directions,
                                             consider_boundary = consider_boundary,
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
get_boundaries.stars <- function(landscape,
                                 directions = 4,
                                 consider_boundary = FALSE,
                                 as_NA = FALSE,
                                 return_raster = TRUE) {

    # check if either directions are possible
    if (directions != 4 && directions != 8) {
        stop("Please specify 'directions = 4' or 'directions = 8'.", call. = FALSE)
    }

    # convert as raster
    landscape <- methods::as(landscape, "Raster")

    # get boundaries
    result <- lapply(X = raster::as.list(landscape), function(x) {

        result_temp <- get_boundaries.matrix(raster::as.matrix(x),
                                             directions = directions,
                                             consider_boundary = consider_boundary,
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
get_boundaries.list <- function(landscape,
                                directions = 4,
                                consider_boundary = FALSE,
                                as_NA = FALSE,
                                return_raster = TRUE) {

    # check if either directions are possible
    if (directions != 4 && directions != 8) {
        stop("Please specify 'directions = 4' or 'directions = 8'.", call. = FALSE)
    }

    # get boundaries
    result <- lapply(X = landscape, function(x) {

        result_temp <- get_boundaries.matrix(raster::as.matrix(x),
                                             directions = directions,
                                             consider_boundary = consider_boundary,
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
get_boundaries.matrix <- function(landscape,
                                  directions = 4,
                                  consider_boundary = FALSE,
                                  as_NA = FALSE) {

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
                                     directions = directions)

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
