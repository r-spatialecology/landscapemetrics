#' Landscape as list
#'
#' @description Convert raster input to list
#'
#' @param landscape SpatRaster, Raster* Layer, Stack, Brick, Stars or a list of rasterLayers
#'
#' @details Mainly for internal use
#'
#' @return list
#'
#' @examples
#' landscape_as_list(raster::stack(landscape, landscape))
#'
#' @aliases landscape_as_list
#' @rdname landscape_as_list
#'
#' @export
landscape_as_list <- function(landscape) UseMethod("landscape_as_list")

#' @name landscape_as_list
#' @export
landscape_as_list.RasterLayer <- function(landscape) {

    landscape <- raster::as.list(landscape)

    return(landscape)
}

#' @name landscape_as_list
#' @export
landscape_as_list.RasterStack <- function(landscape) {

    landscape <- raster::as.list(landscape)

    return(landscape)
}

#' @name landscape_as_list
#' @export
landscape_as_list.RasterBrick <- function(landscape) {

    landscape <- raster::as.list(landscape)

    return(landscape)
}

#' @name landscape_as_list
#' @export
landscape_as_list.stars <- function(landscape) {

    landscape <- methods::as(landscape, "Raster")

    landscape <- raster::as.list(landscape)

    return(landscape)
}

#' @name landscape_as_list
#' @export
landscape_as_list.SpatRaster <- function(landscape) {

    landscape <- methods::as(landscape, "SpatRaster")

    landscape <- raster::as.list(landscape)

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
