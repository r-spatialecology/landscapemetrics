#' sample_lsm
#'
#' @description Sample metrics
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param points SpatialPoints, sf or 2-column matrix with coordinates of sample points
#' @param shape String specifying plot shape. Either "circle" or "square"
#' @param size Approximated size of sample plot. Equals the radius for circles or the
#' side-length for squares in mapunits
#' @param sample_plots SpatialPolygons in which should be sampled.
#' @param ... Arguments passed on to \code{calculate_lsm()}.
#' @param return_raster Logical if the clipped raster of the sample plot should
#' be returned
#'
#' @details
#' This function samples the selected metrics in a buffer area (sample plot)
#' around sample points or within provided SpatialPolygons. The size of the actual
#' sampled landscape can be different to the provided size due to two reasons.
#' Firstly, because clipping raster cells using a circle or a sample plot not directly
#' at a cell center lead to inaccuracies. Secondly, sample plots can exceed the
#' landscape boundary. Therefore, we report the actual clipped sample plot area relative
#' in relation to the theoretical, maximum sample plot area e.g. a sample plot only half
#' within the landscape will have a `percentage_inside = 50`. Please be aware that the
#' output is sligthly different to all other `lsm`-function of `landscapemetrics`.
#'
#' The metrics can be specified by the arguments `what`, `level`, `metric`, `name`
#' and/or `type` (combinations of different arguments are possible (e.g.
#' `level = "class", type = "aggregation metric"`). If an argument is not provided,
#' automatically all possibilities are selected. Therefore, to get **all**
#' available metrics, don't specify any of the above arguments.
#'
#' @seealso
#' \code{\link{list_lsm}} \cr
#' \code{\link{calculate_lsm}}
#'
#' @return tibble
#'
#' @examples
#' # use a matrix
#' points <- matrix(c(10, 5, 25, 15, 5, 25), ncol = 2, byrow = TRUE)
#' sample_lsm(landscape, points = points, size = 15, what = "lsm_l_np")
#'
#' # use sp points
#' points_sp <- sp::SpatialPoints(points)
#' sample_lsm(landscape, points = points_sp, size = 15, what = "lsm_l_np", return_raster = TRUE)
#'
#' # use polygons
#' poly_1 <-  sp::Polygon(cbind(c(2.5, 2.5, 17.5, 17.5),
#'                            c(-2.5, 12.5, 12.5, -2.5)))
#' poly_2 <-  sp::Polygon(cbind(c(7.5, 7.5, 23.5, 23.5),
#'                            c(-7.5, 23.5, 23.5, -7.5)))
#' poly_1 <- sp::Polygons(list(poly_1), "p1")
#' poly_2 <- sp::Polygons(list(poly_2), "p2")
#' sample_plots <- sp::SpatialPolygons(list(poly_1, poly_2))
#' sample_lsm(landscape, sample_plots = sample_plots, what = "lsm_l_np")
#'
#' @aliases sample_lsm
#' @rdname sample_lsm
#'
#' @export
sample_lsm <- function(landscape,
                       points, shape, size,
                       sample_plots,
                       ...,
                       return_raster) UseMethod("sample_lsm")

#' @name sample_lsm
#' @export
sample_lsm.RasterLayer <- function(landscape,
                                   points = NULL, shape = "square", size,
                                   sample_plots = NULL,
                                   ...,
                                   return_raster = FALSE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = sample_lsm_int,
                     points = points, shape = shape, size = size,
                     sample_plots = sample_plots,
                     ...)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    if (!isTRUE(return_raster)) {
        result  <- result[, -9]
    }

    result[with(result, order(layer, plot_id, level, metric, class, id)), ]
}

#' @name sample_lsm
#' @export
sample_lsm.RasterStack <- function(landscape,
                                   points = NULL, shape = "square", size,
                                   sample_plots = NULL,
                                   ...,
                                   return_raster = FALSE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = sample_lsm_int,
                     points = points, shape = shape, size = size,
                     sample_plots = sample_plots,
                     ...)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    if (!isTRUE(return_raster)) {
        result  <- result[, -9]
    }

    result[with(result, order(layer, plot_id, level, metric, class, id)), ]
}

#' @name sample_lsm
#' @export
sample_lsm.RasterBrick <- function(landscape,
                                   points = NULL, shape = "square", size,
                                   sample_plots = NULL,
                                   ...,
                                   return_raster = FALSE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = sample_lsm_int,
                     points = points, shape = shape, size = size,
                     sample_plots = sample_plots,
                     ...)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    if (!isTRUE(return_raster)) {
        result  <- result[, -9]
    }

    result[with(result, order(layer, plot_id, level, metric, class, id)), ]
}

#' @name sample_lsm
#' @export
sample_lsm.list <- function(landscape,
                            points = NULL, shape = "square", size,
                            sample_plots = NULL,
                            ...,
                            return_raster = FALSE) {

    result <- lapply(X = landscape,
                     FUN = sample_lsm_int,
                     points = points, shape = shape, size = size,
                     sample_plots = sample_plots,
                     ...)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    if (!isTRUE(return_raster)) {
        result  <- result[, -9]
    }

    result[with(result, order(layer, plot_id, level, metric, class, id)), ]
}

sample_lsm_int <- function(landscape,
                           points, shape, size,
                           sample_plots, ...) {

    # neither points nor polygons are provided
    if (is.null(points) & is.null(sample_plots)) {

        stop("Please provide either sample points locations or sample polygons.",
             call. = FALSE)
    }

    # both points and polygon are provided
    if (!is.null(points) & !is.null(sample_plots)) {

        stop("Please provide only sample points locations or only sample polygons.",
             call. = FALSE)
    }

    # use polygon
    if (!is.null(sample_plots)) {

        if (!methods::is(sample_plots, "SpatialPolygons")) {
            stop("sample_plots must be SpatialPolygons.", call. = FALSE)
        }

        # get area of all polygons
        maximum_area <- vapply(sample_plots@polygons, function(x) x@area / 10000,
                               FUN.VALUE = numeric(1))
    }

    # check if points are provided
    if (!is.null(points)) {

        # how many sample points are present for SP
        if (methods::is(points, "SpatialPoints")) {
            n <- length(points)
        }

        # how many sample points are present for matrix
        else if (methods::is(points, "matrix")) {
            n <- nrow(points)
        }

        # wrong class of points
        else {
            stop("Points must be a matrix or SpatialPoints.", call. = FALSE)
        }

        # calculate theoretical, maximum area n times
        if (shape == "circle") {
            maximum_area <- rep((pi * size ^ 2) / 10000, times = n)
        }

        # calculate theoretical, maximum area
        else if (shape == "square") {
            maximum_area <- rep((size ^ 2) / 10000, times = n)
        }

        # Unkown shape argument
        else{
            stop(paste0("Shape = ", shape, " unknown."), call. = FALSE)
        }

        # construct plot area around sample points
        sample_plots <- construct_buffer(points = points,
                                         shape = shape,
                                         size = size)
    }

    # loop through each sample point and calculate metrics
    result <- do.call(rbind, lapply(X = seq_along(sample_plots), FUN = function(current_plot) {

        # crop sample plot
        landscape_crop <- raster::crop(x = landscape,
                                       y = sample_plots[current_plot])

        # mask sample plot
        landscape_mask <- raster::mask(x = landscape_crop,
                                       mask = sample_plots[current_plot])

        # calculate actual area of sample plot
        area <- lsm_l_ta_calc(landscape_mask,
                              directions = 8)

        # calculate lsm
        result_current_plot <- calculate_lsm(landscape = landscape_mask,
                                             ...)

        # add plot id
        result_current_plot$plot_id <- current_plot

        # calculate ratio between actual area and theoretical area
        result_current_plot$percentage_inside <- area$value / maximum_area[[current_plot]] * 100

        # add sample plot raster
        result_current_plot$raster_sample_plots <- raster::as.list(landscape_mask)

        return(result_current_plot)
        })
    )

    return(result)
}
