#' unpad_raster
#'
#' @description Adding padding to raster
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
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
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_padded <- pad_raster(landscape, pad_raster_cells = 2, pad_raster_value = -1)
#' unpad_raster(lsm_padded[[1]], unpad_raster_cells = 2)
#'
#' @keywords internal
#'
#' @export
unpad_raster <- function(landscape,
                         unpad_raster_cells = 1,
                         return_raster = TRUE,
                         to_disk = getOption("to_disk", default = FALSE)) {

    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape, function(x) {

        result_temp <- unpad_raster_internal(landscape = x,
                                             unpad_raster_cells = unpad_raster_cells)

        if (return_raster && inherits(x = x, what = "SpatRaster")) {

            result_temp <- matrix_to_raster(matrix = result_temp,
                                            extent = terra::ext(x) -
                                                terra::res(x) * unpad_raster_cells,
                                            resolution = terra::res(x), crs =  terra::crs(x),
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

unpad_raster_internal <- function(landscape, unpad_raster_cells){

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {

        landscape <-terra::as.matrix(landscape, wide = TRUE)

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
