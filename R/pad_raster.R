#' pad_raster
#'
#' @description Adding padding to raster
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param pad_raster_value Value of cells added
#' @param pad_raster_cells Number of rows and columns added
#' @param global Only pad around the raster extent or also NA holes "inside"
#'
#' @details
#' Adds equally (in all four directions) additional cells around the raster
#'
#' @return raster
#'
#' @examples
#' pad_raster(landscape, -999, 2)
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
                       global) UseMethod("pad_raster")

#' @name pad_raster
#' @export
pad_raster.RasterLayer <- function(landscape,
                                   pad_raster_value = -999,
                                   pad_raster_cells = 1,
                                   global = FALSE) {

    pad_raster_internal(raster::as.matrix(landscape),
                        pad_raster_value = pad_raster_value,
                        pad_raster_cells = pad_raster_cells,
                        global = global)
}

#' @name pad_raster
#' @export
pad_raster.RasterStack <- function(landscape,
                                   pad_raster_value = -999,
                                   pad_raster_cells = 1,
                                   global = FALSE) {

    lapply(X = raster::as.list(landscape),
           FUN = function(x, pad_raster_value, pad_raster_cells, global) {
               x <- raster::as.matrix(x)
               pad_raster_internal(x, pad_raster_value, pad_raster_cells, global)
               },
           pad_raster_value = pad_raster_value,
           pad_raster_cells = pad_raster_cells,
           global = global)
}

#' @name pad_raster
#' @export
pad_raster.RasterBrick <- function(landscape,
                                   pad_raster_value = -999,
                                   pad_raster_cells = 1,
                                   global = FALSE) {

    lapply(X = raster::as.list(landscape),
           FUN = function(x, pad_raster_value, pad_raster_cells, global) {
               x <- raster::as.matrix(x)
               pad_raster_internal(x, pad_raster_value, pad_raster_cells, global)
           },
           pad_raster_value = pad_raster_value,
           pad_raster_cells = pad_raster_cells,
           global = global)
}

#' @name pad_raster
#' @export
pad_raster.list <- function(landscape,
                            pad_raster_value = -999,
                            pad_raster_cells = 1,
                            global = FALSE) {

    lapply(X = landscape,
           FUN = pad_raster_internal,
           pad_raster_value = pad_raster_value,
           pad_raster_cells = pad_raster_cells,
           global = global)
}

#' @name pad_raster
#' @export
pad_raster.matrix <- function(landscape,
                              pad_raster_value = -999,
                              pad_raster_cells = 1,
                              global = FALSE) {

    pad_raster_internal(landscape,
                        pad_raster_value = pad_raster_value,
                        pad_raster_cells = pad_raster_cells,
                        global = global)
}

pad_raster_internal <- function(landscape,
                                pad_raster_value,
                                pad_raster_cells,
                                global){

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

    if(isTRUE(global)){
        landscape_padded[is.na(landscape_padded)] <- pad_raster_value
    }

    return(landscape_padded)
}
