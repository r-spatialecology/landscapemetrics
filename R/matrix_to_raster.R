#' matrix_to_raster
#'
#' @description Convert matrix to raster
#'
#' @param matrix matrix with values.
#' @param landscape SpatRaster
#' @param landscape_empty If true, SpatRaster is landscape_empty
#' @param extent Extent of SpatRaster
#' @param resolution Resolution of SpatRaster
#' @param crs CRS of raster layer.
#' @param to_disk If TRUE raster will be saved to disk.
#'
#' @details
#' Converts `matrix` to a raster with same characteristics as `landscape`. Either
#' `landscape` or `extent`, `resolution` and `crs` must be specified.
#'
#' @return raster
#'
#' @examples
#' augusta_nlcd <- terra::rast(landscapemetrics::augusta_nlcd)
#'
#' test_matrix <- terra::as.matrix(augusta_nlcd, wide = TRUE)
#' matrix_to_raster(matrix = test_matrix, landscape = augusta_nlcd)
#'
#' @keywords internal
#'
#' @export
matrix_to_raster <- function(matrix,
                             landscape = NULL,
                             landscape_empty = FALSE,
                             extent = NULL,
                             resolution = NULL,
                             crs = NULL,
                             to_disk = getOption("to_disk", default = FALSE)) {

  # create empty raster with same characteristics as reference raster
  if (!is.null(landscape)) {

    if (landscape_empty) {
        out <- landscape

    } else {
        out <- terra::rast(x = terra::ext(landscape), resolution = terra::res(landscape),
                           crs = terra::crs(landscape))
    }

  } else if (!all(c(is.null(extent), is.null(resolution), is.null(crs)))) {

      out <- terra::rast(x = extent, resolution = resolution, crs = crs)
  }

  else{
    stop("Either 'landscape' or 'extent' & 'resolution' & 'crs' must be specified")
  }

  # create raster on disk
  if (to_disk) {

    # transpose matrix to get correct ordering
    matrix <- t(matrix)

    # starting to write values in raster
    blks <- terra::writeStart(x = out, filename = paste0(tempfile(), ".tif"),
                              overwrite = TRUE)

    # loop through all block sizes
    for (i in 1:blks$n) {

      # start and end row of current block
      start_row <- blks$row[i]
      end_row <- blks$row[i] + (blks$nrows[i] - 1)

      # get values from matrix (row and col exchanged due to transposing)
      values_temp <- c(matrix[, start_row:end_row])

      # write current block
      terra::writeValues(out, values_temp, blks$row[i], blks$nrows[i])

    }

    # close writing connections
    terra::writeStop(out)

  # create raster in memory
  } else {

    # set values of empty raster according to matrix
      out <- terra::setValues(x = out, values = matrix)

  }

  return(out)
}
