#' pad_raster
#'
#' @description Adding padding to raster
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param pad_raster_value Value of cells added
#' @param pad_raster_cells Number of rows and columns added
#' @param global If TRUE, NAs within the landscape are replace with the value; if FALSE only
#' padding around the raster is added.
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
#' landscape <- terra::rast(landscapemetrics::landscape)
#' pad_raster(landscape, pad_raster_value = -1, pad_raster_cells = 2)
#'
#' @keywords internal
#'
#' @export
pad_raster <- function(landscape, pad_raster_value = -999, pad_raster_cells = 1,
                       global = FALSE, return_raster = TRUE,
                       to_disk = getOption("to_disk", default = FALSE)) {

    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape, FUN = function(x) {

        result_temp <- pad_raster_internal(landscape = x,
                                           pad_raster_value = pad_raster_value,
                                           pad_raster_cells = pad_raster_cells,
                                           global = global)

        if (return_raster && inherits(x = x, what = "SpatRaster")) {

            result_temp <- matrix_to_raster(matrix = result_temp,
                                            extent = terra::ext(x) + terra::res(x) * pad_raster_cells,
                                            resolution = terra::res(x), crs = terra::crs(x),
                                            to_disk = to_disk)

        } else if (return_raster || to_disk && !inherits(x = x, what = "SpatRaster")) {

            warning("'return_raster = TRUE' or 'to_disk = TRUE' not able for matrix input.",
                    call. = FALSE)

        }

        return(result_temp)
    })

    names(result) <- paste0("layer_", 1:length(result))

    return(result)
}

pad_raster_internal <- function(landscape,
                                pad_raster_value,
                                pad_raster_cells,
                                global){

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {

        landscape <-terra::as.matrix(landscape, wide = TRUE)

    }

    # get pad_raster_values as often as columns times pad_raster_cells add in y direction
    y_direction <- matrix(rep(x = pad_raster_value,
                              times = ncol(landscape) * pad_raster_cells),
                          nrow = pad_raster_cells)

    # add columns on both sides
    landscape_padded <- rbind(y_direction, landscape, y_direction,
                              deparse.level = 0)

    # get pad_raster_values as often as rows time spad_raster_cells to add in x direction
    x_direction <- matrix(rep(x = pad_raster_value,
                              times = nrow(landscape_padded) * pad_raster_cells),
                          ncol = pad_raster_cells)

    # add rows on both sides
    landscape_padded <- cbind(x_direction, landscape_padded, x_direction,
                              deparse.level = 0)

    if (global) {

        landscape_padded[is.na(landscape_padded)] <- pad_raster_value

    }

    return(landscape_padded)
}
