landscape_as_list <- function(landscape) UseMethod("landscape_as_list")

landscape_as_list.RasterLayer <- function(landscape){
    landscape <- raster::as.list(landscape)
    return(landscape)
}
landscape_as_list.RasterStack <- function(landscape){
    landscape <- raster::as.list(landscape)
    return(landscape)
}
landscape_as_list.RasterBrick <- function(landscape){
    landscape <- raster::as.list(landscape)
    return(landscape)
}
landscape_as_list.stars <- function(landscape){
    landscape <- methods::as(landscape, "Raster")
    landscape <- raster::as.list(landscape)
    return(landscape)
}
landscape_as_list.list <- function(landscape){
    return(landscape)
}
