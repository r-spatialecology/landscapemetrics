#' matrix_to_raster
#'
#' @description Convert matrix to raster
#'
#' @param matrix matrix with values.
#' @param landscape RasterLayer.
#' @param landscape_empty If true, RasterLayer is landscape_empty
#' @param to_disk If TRUE raster will be saved to disk.
#' @param extent Extent of RasterLayer.
#' @param resolution Resolution of RasterLayer.
#' @param crs CRS of raster layer.
#'
#' @details
#' Converts `matrix` to a raster with same characteristics as `landscape`. Either
#' `landscape` or `extent`, `resolution` and `crs` must be specified.
#'
#' @return raster
#'
#' @examples
#' test_matrix <- raster::as.matrix(augusta_nlcd)
#' matrix_to_raster(matrix = test_matrix, landscape = augusta_nlcd)
#'
#' @aliases matrix_to_raster
#' @rdname matrix_to_raster
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
  if(!is.null(landscape)){

    if(landscape_empty){
      landscape_empty <- landscape
    }

    else {
      landscape_empty <- raster::raster(x = raster::extent(landscape),
                                        resolution = raster::res(landscape),
                                        crs = raster::crs(landscape))
    }
  }

  else if(!all(c(is.null(extent), is.null(resolution), is.null(crs)))){
    landscape_empty <- raster::raster(x = extent,
                                      resolution = resolution,
                                      crs = crs)
  }

  else{
    stop("Either 'landscape' or 'extent' & 'resolution' & 'crs' must be specified")
  }

  # create raster on disk
  if(to_disk) {

    # get block size
    block_size <- raster::blockSize(landscape_empty)

    # transpose matrix to get correct ordering
    matrix <- t(matrix)

    # starting to write values in raster
    result <- writeStart(x = landscape_empty,
                         filename = raster::rasterTmpFile(),
                         overwrite = TRUE)

    # loop through all block sizes
    for (i in 1:block_size$n) {

      # start and end row of current block
      start_row <- block_size$row[i]
      end_row <- block_size$row[i] + (block_size$nrow[i] - 1)

      # get values from matrix (row and col exchanged due to transposing)
      values_temp <- c(matrix[, start_row:end_row])

      # write current block
      result <- raster::writeValues(x = result,
                                    v = values_temp,
                                    start = block_size$row[i])
    }

    # close writing connections
    result <- raster::writeStop(result)
  }

  # create raster in memory
  else {

    # set values of empty raster according to matrix
    result <- raster::setValues(x = landscape_empty,
                                values = matrix)
  }


  return(result)
}
