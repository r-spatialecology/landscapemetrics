#' Landscape as list
#'
#' @description Convert raster input to list
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters
#'
#' @details Mainly for internal use
#'
#' @return list
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' landscape_as_list(c(landscape, landscape))
#'
#' @export
landscape_as_list <- function(landscape) UseMethod("landscape_as_list")

#' @name landscape_as_list
#' @export
landscape_as_list.SpatRaster <- function(landscape) {
    landscape <- terra::as.list(landscape)

    return(landscape)
}

#' @name landscape_as_list
#' @export
landscape_as_list.RasterLayer <- function(landscape) {

    landscape <- terra::rast(landscape)
    landscape <- terra::as.list(landscape)

    return(landscape)
}

#' @name landscape_as_list
#' @export
landscape_as_list.RasterBrick <- function(landscape) {

    landscape <- terra::rast(landscape)
    landscape <- terra::as.list(landscape)

    return(landscape)
}

#' @name landscape_as_list
#' @export
landscape_as_list.RasterStack <- function(landscape) {

    landscape <- terra::rast(landscape)
    landscape <- terra::as.list(landscape)

    return(landscape)
}

#' @name landscape_as_list
#' @export
landscape_as_list.stars <- function(landscape) {

    landscape <- terra::rast(methods::as(landscape, "Raster"))
    landscape <- terra::as.list(landscape)

    return(landscape)
}

#' @name landscape_as_list
#' @export
landscape_as_list.list <- function(landscape) {

    return(landscape)
}

#' @name landscape_as_list
#' @export
landscape_as_list.matrix <- function(landscape) {

    landscape <- list(landscape)

    return(landscape)
}

#' @name landscape_as_list
#' @export
landscape_as_list.numeric <- function(landscape) {

    landscape <- list(landscape)

    return(landscape)
}
