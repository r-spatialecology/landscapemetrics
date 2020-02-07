#' get_circumscribingcircle
#'
#' @description Diameter of the circumscribing circle around patches
#'
#' @param landscape RasterLayer or matrix (with x, y, id columns)
#' @param resolution_x Resolution of the landscape (only needed if matrix as input is used)
#' @param resolution_y Resolution of the landscape (only needed if matrix as input is used)
#'
#' @details
#' The diameter of the smallest circumscribing circle around a patch in the landscape
#' is based on the maximum distance between the corners of each cell. This ensures that all
#' cells of the patch are included in the patch. All patches need an unique
#' ID (see \code{\link{get_patches}}). If one uses this functions with a matrix the
#' resolution of the underlying data must be provided.
#'
#' @examples
#' # get patches for class 1 from testdata as raster
#' class_1 <- get_patches(landscape, class = 1)[[1]]
#'
#' # calculate the minimum circumscribing circle of each patch in class 1
#' get_circumscribingcircle(class_1)
#'
#' # do the same with a 3 column matrix (x, y, id)
#' class_1_matrix <- raster::rasterToPoints(class_1)
#' get_circumscribingcircle(class_1_matrix, resolution_x = 1, resolution_y = 1)
#'
#' @aliases get_circumscribingcircle
#' @rdname get_circumscribingcircle
#'
#' @export
get_circumscribingcircle <- function(landscape,
                                     resolution_x,
                                     resolution_y) UseMethod("get_circumscribingcircle")

#' @name get_circumscribingcircle
#' @export
get_circumscribingcircle.RasterLayer <- function(landscape,
                                                 resolution_x = NULL,
                                                 resolution_y = NULL) {

    result <- lapply(seq_along(raster::as.list(landscape)), function(x) {

        resolution_xy <- raster::res(landscape[[x]])
        resolution_x <- resolution_xy[[1]]
        resolution_y <- resolution_xy[[2]]

        mat <- raster::as.matrix(landscape[[x]])
        circle <- rcpp_get_circle(mat, resolution_xy = resolution_x)

        tibble::tibble(layer = x,
                       id = circle$patch_id,
                       dist = circle$circle_diameter)
    })

    result <- do.call(rbind, result)

    return(result)
}

#' @name get_circumscribingcircle
#' @export
get_circumscribingcircle.RasterStack <- function(landscape,
                                                 resolution_x = NULL,
                                                 resolution_y = NULL) {

    result <- lapply(seq_along(raster::as.list(landscape)), function(x) {

        resolution_xy <- raster::res(landscape[[x]])
        resolution_x <- resolution_xy[[1]]
        resolution_y <- resolution_xy[[2]]

        mat <- raster::as.matrix(landscape[[x]])
        circle <- rcpp_get_circle(mat, resolution_xy = resolution_x)

        tibble::tibble(layer = x,
                       id = circle$patch_id,
                       dist = circle$circle_diameter)
    })

    result <- do.call(rbind, result)

    return(result)
}

#' @name get_circumscribingcircle
#' @export
get_circumscribingcircle.RasterBrick <- function(landscape,
                                                 resolution_x = NULL,
                                                 resolution_y = NULL) {

    result <- lapply(seq_along(raster::as.list(landscape)), function(x) {

        resolution_xy <- raster::res(landscape[[x]])
        resolution_x <- resolution_xy[[1]]
        resolution_y <- resolution_xy[[2]]

        mat <- raster::as.matrix(landscape[[x]])
        circle <- rcpp_get_circle(mat, resolution_xy = resolution_x)

        tibble::tibble(layer = x,
                       id = circle$patch_id,
                       dist = circle$circle_diameter)
    })

    result <- do.call(rbind, result)

    return(result)
}

#' @name get_circumscribingcircle
#' @export
get_circumscribingcircle.stars <- function(landscape,
                                           resolution_x = NULL,
                                           resolution_y = NULL) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(seq_along(raster::as.list(landscape)), function(x) {

        resolution_xy <- raster::res(landscape[[x]])
        resolution_x <- resolution_xy[[1]]
        resolution_y <- resolution_xy[[2]]

        mat <- raster::as.matrix(landscape[[x]])
        circle <- rcpp_get_circle(mat, resolution_xy = resolution_x)

        tibble::tibble(layer = x,
                       id = circle$patch_id,
                       dist = circle$circle_diameter)
    })

    result <- do.call(rbind, result)

    return(result)
}

#' @name get_circumscribingcircle
#' @export
get_circumscribingcircle.list <- function(landscape,
                                          resolution_x = NULL,
                                          resolution_y = NULL) {

    result <- lapply(seq_along(landscape), function(x) {

        resolution_xy <- raster::res(landscape[[x]])
        resolution_x <- resolution_xy[[1]]
        resolution_y <- resolution_xy[[2]]

        mat <- raster::as.matrix(landscape[[x]])
        circle <- rcpp_get_circle(mat, resolution_xy = resolution_x)

        tibble::tibble(layer = x,
                       id = circle$patch_id,
                       dist = circle$circle_diameter)
    })

    result <- do.call(rbind, result)

    return(result)
}

#' @name get_circumscribingcircle
#' @export
get_circumscribingcircle.matrix <- function(landscape,
                                            resolution_x = NULL,
                                            resolution_y = NULL) {

    if (ncol(landscape) != 3) {
        stop("Coordinate matrix must have 3 (x, y, id) columns.",
             call. = FALSE)
    }

    if (is.null(resolution_x) || is.null(resolution_y)) {
        stop("Resolution must be provided to correctly calculate the edges. ",
             call. = FALSE)
    }
    mat <- raster::rasterFromXYZ(landscape)
    mat <- raster::as.matrix(mat)

    circle <- rcpp_get_circle(mat, resolution_xy = resolution_x)

    tibble::add_column(tibble::tibble(id = circle$patch_id,
                   dist = circle$circle_diameter),
                   layer = 1, .before = TRUE)
}
