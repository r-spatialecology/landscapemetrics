prepare_input <- function(landscape) UseMethod("prepare_input")

prepare_input.RasterLayer <- function(landscape){
    landscape <- raster::as.list(landscape)
    return(landscape)
}
prepare_input.RasterStack <- function(landscape){
    landscape <- raster::as.list(landscape)
    return(landscape)
}
prepare_input.RasterBrick <- function(landscape){
    landscape <- raster::as.list(landscape)
    return(landscape)
}
prepare_input.stars <- function(landscape){
    landscape <- methods::as(landscape, "Raster")
    landscape <- raster::as.list(landscape)
    return(landscape)
}
prepare_input.list <- function(landscape){
    return(landscape)
}
