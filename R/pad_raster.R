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
#' pad_raster(landscape)
#'
#' @aliases pad_raster
#' @rdname pad_raster
#'
#' @keywords internal
#'
#' @export
pad_raster <- function(landscape,
                       pad_raster_value, pad_raster_cells,
                       global) UseMethod("pad_raster")

#' @name pad_raster
#' @export
pad_raster.RasterLayer <- function(landscape,
                                   pad_raster_value = -999,
                                   pad_raster_cells = 1,
                                   global = FALSE) {
    pad_raster_internal(landscape, pad_raster_value, pad_raster_cells, global)
}

#' @name pad_raster
#' @export
pad_raster.RasterStack <- function(landscape,
                                   pad_raster_value = -999,
                                   pad_raster_cells = 1,
                                   global = FALSE) {
    purrr::map(raster::as.list(landscape), pad_raster_internal,
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
    purrr::map(raster::as.list(landscape), pad_raster_internal,
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
    purrr::map(landscape, pad_raster_internal,
               pad_raster_value = pad_raster_value,
               pad_raster_cells = pad_raster_cells,
               global = global)
}

pad_raster_internal <- function(landscape,
                                pad_raster_value,
                                pad_raster_cells,
                                global){

    landscape_matrix <- raster::as.matrix(landscape)

    for(i in seq_len(pad_raster_cells)){
        landscape_matrix <- rbind(pad_raster_value,
                                  landscape_matrix,
                                  pad_raster_value,
                                  deparse.level = 0)
        landscape_matrix <- cbind(pad_raster_value,
                                  landscape_matrix,
                                  pad_raster_value,
                                  deparse.level = 0)
    }

    if(isTRUE(global)){
     landscape_matrix[is.na(landscape_matrix)] <- pad_raster_value
    }

    landscape_padded_extent <- raster::extent(landscape) +
        (pad_raster_cells * 2 * raster::res(landscape))

    landscape_padded <- raster::raster(x = landscape_padded_extent,
                                       resolution = raster::res(landscape),
                                       crs = raster::crs(landscape))

    landscape_padded <- raster::setValues(landscape_padded, landscape_matrix)

    names(landscape_padded) <- names(landscape)


    return(landscape_padded)

}
