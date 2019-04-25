#' pad_raster
#'
#' @description Adding padding to raster
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param pad_raster_value Value of cells added
#' @param pad_raster_cells Number of rows and columns added
#' @param global Only pad around the raster extent or also NA holes "inside"
#' @param return_raster If false, matrix is returned
#' @param to_disk Logical argument, if FALSE results of get_patches are hold
#' in memory. If true, pad_raster writes temporary files and hence, does not hold
#' everything in memory. Can be set with a global option, e.g. `option(to_disk = TRUE)`.
#'
#' @details
#' Adds equally (in all four directions) additional cells around the raster
#'
#' @return raster
#'
#' @examples
#' pad_raster(landscape, pad_raster_value = -1, pad_raster_cells = 2)
#'
#' @aliases pad_raster
#' @rdname pad_raster
#'
#' @keywords internal
#'
#' @export
pad_raster <- function(landscape,
                       pad_raster_value,
                       pad_raster_cells,
                       global,
                       return_raster,
                       to_disk) UseMethod("pad_raster")

#' @name pad_raster
#' @export
pad_raster.RasterLayer <- function(landscape,
                                   pad_raster_value = -999,
                                   pad_raster_cells = 1,
                                   global = FALSE,
                                   return_raster = TRUE,
                                   to_disk = getOption("to_disk", default = FALSE)) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = pad_raster_internal,
                     pad_raster_value = pad_raster_value,
                     pad_raster_cells = pad_raster_cells,
                     global = global)

    # convert back to raster
    if (return_raster) {

        extent <- raster::extent(landscape)

        resolution <- raster::res(landscape)

        crs <- raster::crs(landscape)

        result <- lapply(result,
                         FUN = matrix_to_raster,
                         extent = extent + resolution * pad_raster_cells * 2,
                         resolution = resolution,
                         crs = crs,
                         to_disk = to_disk)
    }

    return(result)
}

#' @name pad_raster
#' @export
pad_raster.RasterStack <- function(landscape,
                                   pad_raster_value = -999,
                                   pad_raster_cells = 1,
                                   global = FALSE,
                                   return_raster = TRUE,
                                   to_disk = getOption("to_disk", default = FALSE)) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = pad_raster_internal,
                     pad_raster_value = pad_raster_value,
                     pad_raster_cells = pad_raster_cells,
                     global = global)

    # convert back to raster
    if (return_raster) {

        extent <- raster::extent(landscape)

        resolution <- raster::res(landscape)

        crs <- raster::crs(landscape)

        result <- lapply(result,
                         FUN = matrix_to_raster,
                         extent = extent + resolution * pad_raster_cells * 2,
                         resolution = resolution,
                         crs = crs,
                         to_disk = to_disk)
    }

    return(result)
}

#' @name pad_raster
#' @export
pad_raster.RasterBrick <- function(landscape,
                                   pad_raster_value = -999,
                                   pad_raster_cells = 1,
                                   global = FALSE,
                                   return_raster = TRUE,
                                   to_disk = getOption("to_disk", default = FALSE)) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = pad_raster_internal,
                     pad_raster_value = pad_raster_value,
                     pad_raster_cells = pad_raster_cells,
                     global = global)

    # convert back to raster
    if (return_raster) {

        extent <- raster::extent(landscape)

        resolution <- raster::res(landscape)

        crs <- raster::crs(landscape)

        result <- lapply(result,
                         FUN = matrix_to_raster,
                         extent = extent + resolution * pad_raster_cells * 2,
                         resolution = resolution,
                         crs = crs,
                         to_disk = to_disk)
    }

    return(result)
}

#' @name pad_raster
#' @export
pad_raster.stars <- function(landscape,
                             pad_raster_value = -999,
                             pad_raster_cells = 1,
                             global = FALSE,
                             return_raster = TRUE,
                             to_disk = getOption("to_disk", default = FALSE)) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = pad_raster_internal,
                     pad_raster_value = pad_raster_value,
                     pad_raster_cells = pad_raster_cells,
                     global = global)

    # convert back to raster
    if (return_raster) {

        extent <- raster::extent(landscape)

        resolution <- raster::res(landscape)

        crs <- raster::crs(landscape)

        result <- lapply(result,
                         FUN = matrix_to_raster,
                         extent = extent + resolution * pad_raster_cells * 2,
                         resolution = resolution,
                         crs = crs,
                         to_disk = to_disk)
    }

    return(result)
}

#' @name pad_raster
#' @export
pad_raster.list <- function(landscape,
                            pad_raster_value = -999,
                            pad_raster_cells = 1,
                            global = FALSE,
                            return_raster = TRUE,
                            to_disk = getOption("to_disk", default = FALSE)) {

    result <- lapply(X = landscape,
                     FUN = pad_raster_internal,
                     pad_raster_value = pad_raster_value,
                     pad_raster_cells = pad_raster_cells,
                     global = global)

    # convert back to raster
    if (return_raster) {

        result <- lapply(seq_along(result),
                         FUN = function(x) {

                             extent <- raster::extent(landscape[[x]])

                             resolution <- raster::res(landscape[[x]])

                             crs <- raster::crs(landscape[[x]])

                             matrix_to_raster(result[[x]],
                                              extent = extent + resolution * pad_raster_cells * 2,
                                              resolution = resolution,
                                              crs = crs,
                                              to_disk = to_disk)})
    }

    return(result)
}

#' @name pad_raster
#' @export
pad_raster.matrix <- function(landscape,
                              pad_raster_value = -999,
                              pad_raster_cells = 1,
                              global = FALSE,
                              return_raster = TRUE,
                              to_disk = getOption("to_disk", default = FALSE)) {

    result <- lapply(X = list(landscape),
                     FUN = pad_raster_internal,
                     pad_raster_value = pad_raster_value,
                     pad_raster_cells = pad_raster_cells,
                     global = global)

    if (return_raster || to_disk) {
        warning("return_raster = TRUE or to_disk = TRUE not able for matrix input")
    }

    return(result)
}

pad_raster_internal <- function(landscape,
                                pad_raster_value,
                                pad_raster_cells,
                                global){

    # convert to matrix
    if (class(landscape) != "matrix") {
        landscape <- raster::as.matrix(landscape)
    }

    # get pad_raster_values as often as columns times pad_raster_cells add in y direction
    y_direction <- matrix(rep(x = pad_raster_value,
                              times = ncol(landscape) * pad_raster_cells),
                          nrow = pad_raster_cells)

    # add columns on both sides
    landscape_padded <- rbind(y_direction,
                              landscape,
                              y_direction,
                              deparse.level = 0)
    # get pad_raster_values as often as rows time spad_raster_cells to add in x direction
    x_direction <- matrix(rep(x = pad_raster_value,
                              times = nrow(landscape_padded) * pad_raster_cells),
                          ncol = pad_raster_cells)

    # add rows on both sides
    landscape_padded <- cbind(x_direction,
                              landscape_padded,
                              x_direction,
                              deparse.level = 0)

    if (global) {
        landscape_padded[is.na(landscape_padded)] <- pad_raster_value
    }

    return(landscape_padded)
}
