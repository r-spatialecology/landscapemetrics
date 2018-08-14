
#' sample_metrics
#'
#' @description Connected components labeling to derive patches in a landscape.
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#'
#' @references
#'
#' @return List
#'
#' @examples
#'
#' @aliases sample_metrics
#' @rdname sample_metrics
#'
#' @export
sample_metrics <- function(landscape, what,
                           points, shape, size) UseMethod("sample_metrics")


#' @name sample_metrics
#' @export
sample_metrics.RasterLayer <- function(landscape,
                                       what = "all",
                                       points, shape = "square", size) {
    sample_metrics_int(landscape,
                    what = what,
                    points = points, shape = "square", size = size) %>%
        raster::as.list()
}

#' @name sample_metrics
#' @export
sample_metrics.RasterStack <- function(landscape,
                                       what = "all",
                                       points, shape = "square", size) {
    purrr::map(raster::as.list(landscape_stack),
               sample_metrics_int,
               what = what,
               points = points, shape = "square", size = size)
}

#' @name sample_metrics
#' @export
sample_metrics.RasterBrick <- function(landscape,
                                       what = "all",
                                       points, shape = "square", size) {
    purrr::map(raster::as.list(landscape),
               sample_metrics_int,
               what = what,
               points = points, shape = "square", size = size)
}

#' @name sample_metrics
#' @export
sample_metrics.list <- function(landscape,
                                what = "all",
                                points, shape = "square", size) {
    purrr::map(landscape,
               sample_metrics_int,
               what = what,
               points = points, shape = "square", size = size)
}

sample_metrics_int <- function(landscape, what,
                               points, shape, size) {



}

# x = 10 # center x
# y = 10 # center y
# n = 100 # nr of pts
# r = 5 # radius
# pts = seq(0, 2 * pi, length.out = n)
# plot(sin(pts), cos(pts), type = 'l', asp = 1) # test
# require(sp)
# xy = cbind(x + r * sin(pts), y + r * cos(pts))
# sl= SpatialPolygons(list(Polygons(list(Polygon(xy)), "Polygon")))
# plot(sl, add=FALSE, col = 'red', axes=T )

points
