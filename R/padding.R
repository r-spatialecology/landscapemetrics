#' Padding
#'
#' @description Adding padding to raster
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param padding_value value of cells added
#' @param padding_cells Number of rows and columns added
#'
#' @return raster
#'
#' @examples
#' padding(landscape)
#'
#' @aliases padding
#' @rdname padding
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
padding <- function(landscape, padding_value, padding_cells) UseMethod("padding")

#' @name padding
#' @export
padding.RasterLayer <- function(landscape, padding_value = -999, padding_cells = 1) {
    padding_internal(landscape, padding_value, padding_cells)
}

#' @name padding
#' @export
padding.RasterStack <- function(landscape, padding_value = -999, padding_cells = 1) {
    purrr::map(raster::as.list(landscape), padding_internal,
               padding_value = padding_value, padding_cells = padding_cells)
}

#' @name padding
#' @export
padding.RasterBrick <- function(landscape, padding_value = -999, padding_cells = 1) {
    purrr::map(raster::as.list(landscape), padding_internal,
               padding_value = padding_value, padding_cells = padding_cells)
}

#' @name padding
#' @export
padding.list <- function(landscape, padding_value = -999, padding_cells = 1) {
    purrr::map(landscape, padding_internal,
               padding_value = padding_value, padding_cells = padding_cells)
}

padding_internal <- function(landscape, padding_value, padding_cells){
    landscape_matrix <- raster::as.matrix(landscape)

    for(i in seq_len(padding_cells)){
        landscape_matrix <- rbind(padding_value,
                                  landscape_matrix,
                                  padding_value,
                                  deparse.level = 0)
        landscape_matrix <- cbind(padding_value,
                                  landscape_matrix,
                                  padding_value,
                                  deparse.level = 0)
    }

    landscape_padded <- raster::raster(landscape_matrix)

    raster::extent(landscape_padded) <- c(
        raster::xmin(landscape),
        (raster::xmax(landscape) + 2 * padding_cells) * raster::res(landscape)[1],
        raster::xmin(landscape),
        (raster::xmax(landscape) + 2 * padding_cells) * raster::res(landscape)[2]
    )

    return(landscape_padded)

}
