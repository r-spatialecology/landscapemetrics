#' scale_sample
#'
#' @description Metrics on changing sample scale
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param y 2-column matrix with coordinates or SpatialPoints.
#' @param shape String specifying plot shape. Either "circle" or "square"
#' @param size Approximated size of sample plot. Equals the radius for circles or half of
#' the side-length for squares in mapunits. For lines size equals the width of the buffer.
#' @param max_size Maximum size to which sample plot size is summed up.
#' @param verbose Print warning messages.
#' @param progress Print progress report.
#' @param ... Arguments passed on to \code{calculate_lsm()}.

#' @details
#' This function calculates the selected metrics in subsequential buffers around
#' a/multiple point(s) of interest.
#'
#' The size of the actual sampled landscape can be different to the provided size
#' due to two reasons. Firstly, because clipping raster cells using a circle or a
#' sample plot not directly at a cell center lead to inaccuracies. Secondly,
#' sample plots can exceed the landscape boundary. Therefore, we report the actual
#' clipped sample plot area relative in relation to the theoretical, maximum sample
#' plot area e.g. a sample plot only half within the landscape will have a
#' `percentage_inside = 50`. Please be aware that the output is sligthly different
#' to all other `lsm`-function of `landscapemetrics`.
#'
#' The metrics can be specified by the arguments `what`, `level`, `metric`, `name`
#' and/or `type` (combinations of different arguments are possible (e.g.
#' `level = "class", type = "aggregation metric"`). If an argument is not provided,
#' automatically all possibilities are selected. Therefore, to get **all**
#' available metrics, don't specify any of the above arguments.
#'
#' @seealso
#' \code{\link{list_lsm}} \cr
#' \code{\link{calculate_lsm}} \cr
#' \code{\link{sample_lsm}} \cr
#' \code{\link{construct_buffer}}
#'
#' @return tibble
#'
#' @examples
#' my_points = matrix(c(1265000, 1250000, 1255000, 1257000),
#'                   ncol = 2, byrow = TRUE)
#'
#' scale_sample(landscape = augusta_nlcd, y = my_points,
#' size = 500, max_size = 5000, what = c("lsm_l_ent", "lsm_l_mutinf"))
#'
#' @aliases scale_sample
#' @rdname scale_sample
#'
#' @export
scale_sample <- function(landscape,
                         y,
                         shape,
                         size, max_size,
                         verbose,
                         progress,
                         ...) UseMethod("scale_sample")

#' @name scale_sample
#' @export
scale_sample.RasterLayer <- function(landscape,
                                     y,
                                     shape = "square",
                                     size, max_size,
                                     verbose = TRUE,
                                     progress = FALSE,
                                     ...) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = scale_sample_int_multibuffer,
                     y = y,
                     shape = shape,
                     size = size,
                     max_size = max_size,
                     verbose = verbose,
                     progress = progress,
                     ...)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    # return warning of only 3/4 of sample plot are in landscape
    if (verbose) {
        if (any(result$percentage_inside < 90)) {
            warning("The 'perecentage_inside' is below 90% for at least one buffer.",
                    call. = FALSE)
        }
    }

    result[with(result, order(layer, plot_id, level, metric, class, id, size)), ]
}

#' @name scale_sample
#' @export
scale_sample.RasterStack <- function(landscape,
                                     y,
                                     shape = "square",
                                     size, max_size,
                                     verbose = TRUE,
                                     progress = FALSE,
                                     ...) {

    landscape <- raster::as.list(landscape)

    result <- lapply(X = seq_along(landscape), FUN = function(x) {

        if (progress) {

            cat("\r> Progress nlayers: ", x , "/", length(landscape))
        }

        scale_sample_int_multibuffer(landscape = landscape[[x]],
                                     y = y,
                                     shape = shape,
                                     size = size,
                                     max_size = max_size,
                                     verbose = verbose,
                                     progress = FALSE,
                                     ...)
    })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    if (progress) {cat("\n")}

    # return warning of only 3/4 of sample plot are in landscape
    if (verbose) {
        if (any(result$percentage_inside < 90)) {
            warning("The 'perecentage_inside' is below 90% for at least one buffer.",
                    call. = FALSE)
        }
    }

    result[with(result, order(layer, plot_id, level, metric, class, id, size)), ]
}

#' @name scale_sample
#' @export
scale_sample.RasterBrick <- function(landscape,
                                     y,
                                     shape = "square",
                                     size, max_size,
                                     verbose = TRUE,
                                     progress = FALSE,
                                     ...) {

    landscape <- raster::as.list(landscape)

    result <- lapply(X = seq_along(landscape), FUN = function(x) {

        if (progress) {

            cat("\r> Progress nlayers: ", x , "/", length(landscape))
        }

        scale_sample_int_multibuffer(landscape = landscape[[x]],
                                     y = y,
                                     shape = shape,
                                     size = size,
                                     max_size = max_size,
                                     verbose = verbose,
                                     progress = FALSE,
                                     ...)
    })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    if (progress) {cat("\n")}

    # return warning of only 3/4 of sample plot are in landscape
    if (verbose) {
        if (any(result$percentage_inside < 90)) {
            warning("The 'perecentage_inside' is below 90% for at least one buffer.",
                    call. = FALSE)
        }
    }

    result[with(result, order(layer, plot_id, level, metric, class, id, size)), ]
}

#' @name scale_sample
#' @export
scale_sample.stars <- function(landscape,
                               y,
                               shape = "square",
                               size, max_size,
                               verbose = TRUE,
                               progress = FALSE,
                               ...) {

    landscape <-  raster::as.list(methods::as(landscape, "Raster"))

    result <- lapply(X = seq_along(landscape), FUN = function(x) {

        if (progress) {

            cat("\r> Progress nlayers: ", x , "/", length(landscape))
        }

        scale_sample_int_multibuffer(landscape = landscape[[x]],
                                     y = y,
                                     shape = shape,
                                     size = size,
                                     max_size = max_size,
                                     verbose = verbose,
                                     progress = FALSE,
                                     ...)
    })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    if (progress) {cat("\n")}

    # return warning of only 3/4 of sample plot are in landscape
    if (verbose) {
        if (any(result$percentage_inside < 90)) {
            warning("The 'perecentage_inside' is below 90% for at least one buffer.",
                    call. = FALSE)
        }
    }

    result[with(result, order(layer, plot_id, level, metric, class, id, size)), ]
}

#' @name scale_sample
#' @export
scale_sample.list <- function(landscape,
                              y,
                              shape = "square",
                              size, max_size,
                              verbose = TRUE,
                              progress = FALSE,
                              ...) {

    result <- lapply(X = seq_along(landscape), FUN = function(x) {

        if (progress) {

            cat("\r> Progress nlayers: ", x , "/", length(landscape))
        }

        scale_sample_int_multibuffer(landscape = landscape[[x]],
                                     y = y,
                                     shape = shape,
                                     size = size,
                                     max_size = max_size,
                                     verbose = verbose,
                                     progress = FALSE,
                                     ...)
    })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    if (progress) {cat("\n")}

    # return warning of only 3/4 of sample plot are in landscape
    if (verbose) {
        if (any(result$percentage_inside < 90)) {
            warning("The 'perecentage_inside' is below 90% for at least one buffer.",
                    call. = FALSE)
        }
    }

    result[with(result, order(layer, plot_id, level, metric, class, id, size)), ]
}

scale_sample_int_multibuffer <- function(landscape,
                                         y,
                                         shape, size, max_size,
                                         verbose,
                                         progress,
                                         ...) {

    # create buffer sequence
    size <- seq(from = size, to = max_size, by = size)

    # loop through buffers
    result <- do.call(rbind, lapply(X = seq_along(size), FUN = function(x) {

        # print progess using the non-internal name
        if (progress) {

            cat("\r> Progress scales: ", x, "/", length(size))
        }

        scale_sample_int(landscape = landscape, y = y,
                         shape = shape, size = size[[x]],
                         verbose = verbose, ...)}))

    if (progress) {

        cat("\n")

    }
    return(result)
}

scale_sample_int <- function(landscape,
                             y,
                             shape, size,
                             verbose,
                             progress,
                             ...) {

    # use points
    if (methods::is(y, "SpatialPoints") | methods::is(y, "SpatialPointsDataFrame") | methods::is(y, "matrix")) {

        # points are matrix
        if (methods::is(y, "matrix")) {

            if (ncol(y) != 2 & verbose) {
                warning("'y' should be a two column matrix including x- and y-coordinates.",
                        call. = FALSE)
            }
        }

        # construct plot area around sample sample_points
        y <- construct_buffer(coords = y,
                              shape = shape,
                              size = size,
                              verbose = verbose)

        # how many plots are present
        number_plots <- length(y)
    }

    else {

        stop("'y' must be a matrix or SpatialPoints.",
             call. = FALSE)
    }

    # get area of all polygons
    maximum_area <- vapply(y@polygons, function(x) x@area / 10000,
                           FUN.VALUE = numeric(1))

    # loop through each sample point and calculate metrics
    result <- do.call(rbind, lapply(X = seq_along(y), FUN = function(current_plot) {

        # crop sample plot
        landscape_crop <- raster::crop(x = landscape,
                                       y = y[current_plot])

        # mask sample plot
        landscape_mask <- raster::mask(x = landscape_crop,
                                       mask = y[current_plot])

        # calculate actual area of sample plot
        area <- lsm_l_ta_calc(landscape_mask,
                              directions = 8)

        # calculate lsm
        result_current_plot <- calculate_lsm(landscape = landscape_mask,
                                             verbose = verbose,
                                             progress = FALSE,
                                             ...)
        # add buffer size
        result_current_plot$size <- size

        # add plot id
        result_current_plot$plot_id <- current_plot

        # calculate ratio between actual area and theoretical area
        result_current_plot$percentage_inside <- area$value / maximum_area[[current_plot]] * 100

        return(result_current_plot)
    }))

    return(result)
}
