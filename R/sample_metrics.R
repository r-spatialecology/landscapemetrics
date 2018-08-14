
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

    if (shape == "circle") {

        circle_points <- seq(0, 2 * pi, length.out = 100)
        circle_points_x <- sin(circle_points) * size
        circle_points_y <- cos(circle_points) * size

        x_circle <- matrix(outer(circle_points_x, points[, 1], `+`),
                           ncol = 1)

        y_circle <- matrix(outer(circle_points_y, points[, 2], `+`),
                           ncol = 1)

        sample_plots_coords <- cbind(x_circle, y_circle,
                                     rep(1:nrow(points), each = 100))

        # Polygons(sample_plots_coords[, 1:2], ID = sample_plots_coords[, 3])

        # sample_plots <- SpatialPolygons(list(Polygons(list(Polygon(xy)), "Polygon")))


    }

    else if (shape == "square") {

    }

    else{
        stop(paste0("Shape=", shape, " unknown"))
    }



}

# x = 10 # center x
# y = 10 # center y
# n = 100 # nr of pts
# r = 5 # radius
#
# pts = seq(0, 2 * pi, length.out = n)
# plot(sin(pts), cos(pts), type = 'l', asp = 1) # test
#
# require(sp)
# xy = cbind(x + 1 * sin(pts), y + 1 * cos(pts))
# sl= SpatialPolygons(list(Polygons(list(Polygon(xy)), "Polygon")))
# plot(sl, add=FALSE, col = 'red', axes=T )
#
# points <- matrix(c(1, 5,
#                    2, 0,
#                    3, 4), ncol = 2, byrow = TRUE)
#
# plot(x = points[,1], y = points[, 2], asp = 1)
#
# points(sample_plots[, 1], sample_plots[, 2])
