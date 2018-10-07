#' Show info
#'
#' @description Show info
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers
#'
#' @details This function extracts basic information about
#' the input landscape.
#' It includes a type of coordinate reference system (crs) -
#' either "geographic", "projected", or NA,
#' units of the coordinate reference system,
#' and a class of the input landscape's values.
#'
#' @return tibble
#'
#' @examples
#' show_info(augusta_nlcd)
#' show_info(podlasie_ccilc)
#' show_info(landscape)
#' landscape_stack = raster::stack(landscape, landscape)
#' show_info(landscape_stack)
#'
#' @aliases show_info
#' @rdname show_info
#'
#' @export
show_info <- function(landscape) UseMethod("show_info")

#' @name show_info
#' @export
show_info.RasterLayer <- function(landscape) {

    result <- lapply(X = raster::as.list(landscape),
           FUN = show_info_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name show_info
#' @export
show_info.RasterStack <- function(landscape) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = show_info_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name show_info
#' @export
show_info.RasterBrick <- function(landscape) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = show_info_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name show_info
#' @export
show_info.stars <- function(landscape) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = show_info_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name show_info
#' @export
show_info.list <- function(landscape) {

    result <- lapply(X = landscape,
                     FUN = show_info_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

proj_info = function(landscape){
    landscape_proj <- raster::projection(landscape)
    if (!is.na(landscape_proj)){
        if(raster::isLonLat(landscape)){
            data.frame(crs = "geographic", units = "degrees")
        } else{
            proj_units <- strsplit(sub(".*units=", "", landscape_proj), " ", fixed = TRUE)[[1]][[1]]
            data.frame(crs = "projected", units = proj_units)
        }
    } else {
        data.frame(crs = NA, units = NA)
    }
}

data_info <- function(landscape){
    data.frame(class = class(raster::getValues(landscape)))
}

show_info_calc <- function(landscape){
    cbind(proj_info(landscape), data_info(landscape))
}
