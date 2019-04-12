#' get_boundaries
#'
#' @description Get boundary cells of patches
#'
#' @param landscape RasterLayer or matrix.
#' @param directions Rook's case (4 neighbours) or queen's case (8 neighbours) should be used as neighbourhood rule
#' @param as_NA If true, non-boundary cells area labeld NA
#' @param return_raster If false, matrix is returned
#'
#' @details
#' All boundary/edge cells are labeled 1, all non-boundary cells 0. NA values are
#' not changed. Boundary cells are defined as cells that neighbour either a NA
#' cell or a cell with a different value than itself. Non-boundary cells only
#' neighbour cells with the same value than themself.
#'
#' @return RasterLayer or matrix
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
                           return_raster) UseMethod("get_boundaries")

#' @name get_boundaries
#' @export
get_boundaries.RasterLayer <- function(landscape,
                                       directions = 4,
                                       as_NA = FALSE,
                                       return_raster = TRUE) {

    # check if either directions are possible
    if (directions != 4 && directions != 8) {
        stop("Please specify 'directions = 4' or 'directions = 8'.", call. = FALSE)
    }

    # get boundaries
    result <- rcpp_get_boundaries(raster::as.matrix(landscape),
                                  directions = directions)

    if (as_NA) {
        result[which(result == 0)] <- NA
    }

    # convert back to raster
    if (return_raster) {
        result <- matrix_to_raster(matrix = result,
                                   landscape = landscape)
    }

    return(result)
}

#' @name get_boundaries
#' @export
get_boundaries.RasterStack <- function(landscape,
                                       directions = 4,
                                       as_NA = FALSE,
                                       return_raster = TRUE) {

    # check if either directions are possible
    if (directions != 4 && directions != 8) {
        stop("Please specify 'directions = 4' or 'directions = 8'.", call. = FALSE)
    }

    # get boundaries
    result <- lapply(X = raster::as.list(landscape), function(x)
        rcpp_get_boundaries(raster::as.matrix(x),
                            directions = directions))

    if (as_NA) {
        result[which(result == 0)] <- NA
    }

    # convert back to raster
    if (return_raster) {

        result <- lapply(seq_along(result),
                         function(x) matrix_to_raster(matrix = result[[x]],
                                                      landscape = landscape[[x]]))

        result <- raster::stack(result)
    }

    return(result)
}

#' @name get_boundaries
#' @export
get_boundaries.RasterBrick <- function(landscape,
                                       directions = 4,
                                       as_NA = FALSE,
                                       return_raster = TRUE) {

    # check if either directions are possible
    if (directions != 4 && directions != 8) {
        stop("Please specify 'directions = 4' or 'directions = 8'.", call. = FALSE)
    }

    # get boundaries
    result <- lapply(X = raster::as.list(landscape), function(x)
        rcpp_get_boundaries(raster::as.matrix(x),
                            directions = directions))

    if (as_NA) {
        result[which(result == 0)] <- NA
    }

    # convert back to raster
    if (return_raster) {

        result <- lapply(seq_along(result),
                         function(x) matrix_to_raster(matrix = result[[x]],
                                                      landscape = landscape[[x]]))

        result <- raster::stack(result)
    }

    return(result)
}

#' @name get_boundaries
#' @export
get_boundaries.stars <- function(landscape,
                                 directions = 4,
                                 as_NA = FALSE,
                                 return_raster = TRUE) {

    # check if either directions are possible
    if (directions != 4 && directions != 8) {
        stop("Please specify 'directions = 4' or 'directions = 8'.", call. = FALSE)
    }

    landscape <- methods::as(landscape, "Raster")

    # get boundaries
    result <- lapply(X = raster::as.list(landscape), function(x)
        rcpp_get_boundaries(raster::as.matrix(x),
                            directions = directions))

    if (as_NA) {
        result[which(result == 0)] <- NA
    }

    # convert back to raster
    if (return_raster) {

        result <- lapply(seq_along(result),
                         function(x) matrix_to_raster(matrix = result[[x]],
                                                      landscape = landscape[[x]]))

        result <- raster::stack(result)
    }

    return(result)
}

#' @name get_boundaries
#' @export
get_boundaries.list <- function(landscape,
                                directions = 4,
                                as_NA = FALSE,
                                return_raster = TRUE) {

    # check if either directions are possible
    if (directions != 4 && directions != 8) {
        stop("Please specify 'directions = 4' or 'directions = 8'.", call. = FALSE)
    }

    # get boundaries
    result <- lapply(X = landscape, function(x)
        rcpp_get_boundaries(raster::as.matrix(x),
                            directions = directions))

    if (as_NA) {
        result[which(result == 0)] <- NA
    }

    # convert back to raster
    if (return_raster) {

        result <- lapply(seq_along(result),
                         function(x) matrix_to_raster(matrix = result[[x]],
                                                      landscape = landscape[[x]]))

        result <- raster::stack(result)
    }

    return(result)
}


#' @name get_boundaries
#' @export
get_boundaries.matrix <- function(landscape,
                                  directions = 4,
                                  as_NA = FALSE,
                                  return_raster = TRUE) {

    # get boundaries
    result <- rcpp_get_boundaries(landscape,
                                  directions = directions)

    if (as_NA) {
        result[which(result == 0)] <- NA
    }

    if (return_raster) {
        warning("return_raster = TRUE not able for matrix input")
    }

    return(result)
}
