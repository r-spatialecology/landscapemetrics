#' unpad_raster
#'
#' @description Adding padding to raster
#'
#' @param landscape Raster* Layer, Stack, Brick, stars, or a list of rasterLayers.
#' @param unpad_raster_cells Number of rows and columns added
#' @param return_raster If false, matrix is returned
#' @param to_disk Logical argument, if FALSE results of get_patches are hold
#' in memory. If true, unpad_raster writes temporary files and hence, does not hold
#' everything in memory. Can be set with a global option, e.g. `option(to_disk = TRUE)`.
#'
#' @details
#' Removes equally (in all four directions) additional cells around the raster
#'
#' @return raster
#'
#' @examples
#' lsm_padded <- pad_raster(landscape, pad_raster_cells = 2, pad_raster_value = -1)
#' unpad_raster(lsm_padded[[1]], unpad_raster_cells = 2)
#'
#' @aliases unpad_raster
#' @rdname unpad_raster
#'
#' @keywords internal
#'
#' @export
unpad_raster <- function(landscape,
                         unpad_raster_cells,
                         return_raster,
                         to_disk) UseMethod("unpad_raster")

#' @name unpad_raster
#' @export
unpad_raster.RasterLayer <- function(landscape,
                                     unpad_raster_cells = 1,
                                     return_raster = TRUE,
                                     to_disk = getOption("to_disk", default = FALSE)) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = unpad_raster_internal,
                     unpad_raster_cells = unpad_raster_cells)

    # convert back to raster
    if (return_raster) {

        extent <- raster::extent(landscape)

        resolution <- raster::res(landscape)

        crs <- raster::crs(landscape)

        result <- lapply(result,
                         FUN = matrix_to_raster,
                         extent = extent - resolution * unpad_raster_cells * 2,
                         resolution = resolution,
                         crs = crs,
                         to_disk = to_disk)
    }

    return(result)
}

#' @name unpad_raster
#' @export
unpad_raster.RasterStack <- function(landscape,
                                     unpad_raster_cells = 1,
                                     return_raster = TRUE,
                                     to_disk = getOption("to_disk", default = FALSE)) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = unpad_raster_internal,
                     unpad_raster_cells = unpad_raster_cells)

    # convert back to raster
    if (return_raster) {

        extent <- raster::extent(landscape)

        resolution <- raster::res(landscape)

        crs <- raster::crs(landscape)

        result <- lapply(result,
                         FUN = matrix_to_raster,
                         extent = extent - resolution * unpad_raster_cells * 2,
                         resolution = resolution,
                         crs = crs,
                         to_disk = to_disk)
    }

    return(result)
}

#' @name unpad_raster
#' @export
unpad_raster.RasterBrick <- function(landscape,
                                     unpad_raster_cells = 1,
                                     return_raster = TRUE,
                                     to_disk = getOption("to_disk", default = FALSE)) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = unpad_raster_internal,
                     unpad_raster_cells = unpad_raster_cells)

    # convert back to raster
    if (return_raster) {

        extent <- raster::extent(landscape)

        resolution <- raster::res(landscape)

        crs <- raster::crs(landscape)

        result <- lapply(result,
                         FUN = matrix_to_raster,
                         extent = extent - resolution * unpad_raster_cells * 2,
                         resolution = resolution,
                         crs = crs,
                         to_disk = to_disk)
    }

    return(result)
}

#' @name unpad_raster
#' @export
unpad_raster.stars <- function(landscape,
                               unpad_raster_cells = 1,
                               return_raster = TRUE,
                               to_disk = getOption("to_disk", default = FALSE)) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = unpad_raster_internal,
                     unpad_raster_cells = unpad_raster_cells)

    # convert back to raster
    if (return_raster) {

        extent <- raster::extent(landscape)

        resolution <- raster::res(landscape)

        crs <- raster::crs(landscape)

        result <- lapply(result,
                         FUN = matrix_to_raster,
                         extent = extent - resolution * unpad_raster_cells * 2,
                         resolution = resolution,
                         crs = crs,
                         to_disk = to_disk)
    }

    return(result)
}

#' @name unpad_raster
#' @export
unpad_raster.list <- function(landscape,
                              unpad_raster_cells = 1,
                              return_raster = TRUE,
                              to_disk = getOption("to_disk", default = FALSE)) {

    result <- lapply(X = landscape,
                     FUN = unpad_raster_internal,
                     unpad_raster_cells = unpad_raster_cells)

    # convert back to raster
    if (return_raster) {

        result <- lapply(seq_along(result),
                         FUN = function(x) {

                             extent <- raster::extent(landscape[[x]])

                             resolution <- raster::res(landscape[[x]])

                             crs <- raster::crs(landscape[[x]])

                             matrix_to_raster(result[[x]],
                                              extent = extent - resolution * unpad_raster_cells * 2,
                                              resolution = resolution,
                                              crs = crs,
                                              to_disk = to_disk)})
    }

    return(result)
}

#' @name unpad_raster
#' @export
unpad_raster.matrix <- function(landscape,
                                unpad_raster_cells = 1,
                                return_raster = FALSE,
                                to_disk = getOption("to_disk", default = FALSE)) {

    result <- lapply(X = list(landscape),
                     FUN = unpad_raster_internal,
                     unpad_raster_cells = unpad_raster_cells)

    if (return_raster || to_disk) {
        warning("'return_raster = TRUE' or 'to_disk = TRUE' not able for matrix input.",
                call. = FALSE)
    }

    return(result)
}

unpad_raster_internal <- function(landscape,
                                  unpad_raster_cells){

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {

        landscape <- raster::as.matrix(landscape)
    }

    # remove first row and col
    landscape <- landscape[-c(1:unpad_raster_cells), -c(1:unpad_raster_cells)]

    # get dimensions of matrix
    n_row_pad <- nrow(landscape)
    n_col_pad <- ncol(landscape)

    # get max row/cols to remove
    n_row_keep <- (n_row_pad - unpad_raster_cells) + 1
    n_col_keep <- (n_col_pad - unpad_raster_cells) + 1

    # remove rows/cols
    landscape <- landscape[-c(n_row_keep:n_row_pad),
                           -c(n_col_keep:n_col_pad)]

    return(landscape)
}
