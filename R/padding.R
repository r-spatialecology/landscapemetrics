#' pad_raster
#'
#' @description Adding padding to raster
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param pad_raster_value Value of cells added
#' @param pad_raster_cells Number of rows and columns added
#'
#' @return raster
#'
#' @examples
#' pad_raster(landscape)
#'
#' @aliases pad_raster
#' @rdname pad_raster
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
pad_raster <- function(landscape, pad_raster_value, pad_raster_cells) UseMethod("pad_raster")

#' @name pad_raster
#' @export
pad_raster.RasterLayer <- function(landscape, pad_raster_value = -999, pad_raster_cells = 1) {
    pad_raster_internal(landscape, pad_raster_value, pad_raster_cells)
}

#' @name pad_raster
#' @export
pad_raster.RasterStack <- function(landscape, pad_raster_value = -999, pad_raster_cells = 1) {
    purrr::map(raster::as.list(landscape), pad_raster_internal,
               pad_raster_value = pad_raster_value, pad_raster_cells = pad_raster_cells)
}

#' @name pad_raster
#' @export
pad_raster.RasterBrick <- function(landscape, pad_raster_value = -999, pad_raster_cells = 1) {
    purrr::map(raster::as.list(landscape), pad_raster_internal,
               pad_raster_value = pad_raster_value, pad_raster_cells = pad_raster_cells)
}

#' @name pad_raster
#' @export
pad_raster.list <- function(landscape, pad_raster_value = -999, pad_raster_cells = 1) {
    purrr::map(landscape, pad_raster_internal,
               pad_raster_value = pad_raster_value, pad_raster_cells = pad_raster_cells)
}

pad_raster_internal <- function(landscape, pad_raster_value, pad_raster_cells){
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

    landscape_padded <- raster::raster(landscape_matrix)

    raster::extent(landscape_padded) <- c(
        raster::xmin(landscape),
        (raster::xmax(landscape) + 2 * pad_raster_cells) * raster::res(landscape)[1],
        raster::xmin(landscape),
        (raster::xmax(landscape) + 2 * pad_raster_cells) * raster::res(landscape)[2]
    )

    return(landscape_padded)

}
