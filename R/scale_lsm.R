#' scale_lsm
#'
#' @description Metrics on changing scale
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param y 2-column matrix with coordinates or SpatialPoints.
#' @param buffer_width Buffer width in which a landscape metric is measured
#' @param max_width Max distance to which buffer_width is summed up
#' @param verbose Print warning messages.
#' @param progress Print progress report.
#' @param ... Arguments passed on to \code{calculate_lsm()}.

#' @details
#' This function calculates the selected metrics in subsequential buffers around
#' a/multiple point(s) of interest.
#'
#' The size of the actual
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
#' scale_lsm(augusta_nlcd, my_points, buffer_width = 500, max_width = 5000,
#'  what = c("lsm_l_ent", "lsm_l_mutinf"))
#'
#' @aliases scale_lsm
#' @rdname scale_lsm
#'
#' @export
scale_lsm <- function(landscape,
                       y,
                       buffer_width, max_width,
                       verbose,
                       progress,
                       ...) UseMethod("scale_lsm")

#' @name scale_lsm
#' @export
scale_lsm.RasterLayer <- function(landscape,
                                   y,
                                   buffer_width, max_width,
                                   verbose = TRUE,
                                   progress = FALSE,
                                   ...) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = scale_lsm_int_multibuffer,
                     y = y,
                     buffer_width = buffer_width, max_width = max_width,
                     verbose = verbose,
                     progress = progress,
                     ...)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    if(any(result < 99)){
        warning(c("Some of buffers extend over the landscape border. ",
                  "Consider decreasing of the max_width argument value."),
                call. = FALSE)
    }

    result[with(result, order(layer, plot_id, level, metric, class, id, value, size, percentage_inside)), ]
}

#' @name scale_lsm
#' @export
scale_lsm.RasterStack <- function(landscape,
                                   y,
                                   buffer_width, max_width,
                                   verbose = TRUE,
                                   progress = FALSE,
                                   ...) {

    landscape <- raster::as.list(landscape)

    result <- lapply(X = seq_along(landscape), FUN = function(x) {

        if (progress) {

            message("\r> Progress nlayers: ", x , "/", length(landscape),
                    appendLF = FALSE)
        }

        scale_lsm_int_multibuffer(landscape = landscape[[x]],
                       y = y,
                       buffer_width = buffer_width,
                       max_width = max_width,
                       verbose = verbose,
                       progress = FALSE,
                       ...)
    })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    if (progress) {message("")}

    if(any(result < 99)){
        warning(c("Some of buffers extend over the landscape border. ",
                  "Consider decreasing of the max_width argument value."),
                call. = FALSE)
    }

    result[with(result, order(layer, plot_id, level, metric, class, id, value, size, percentage_inside)), ]
}

#' @name scale_lsm
#' @export
scale_lsm.RasterBrick <- function(landscape,
                                   y,
                                   buffer_width, max_width,
                                   verbose = TRUE,
                                   progress = FALSE,
                                   ...) {

    landscape <- raster::as.list(landscape)

    result <- lapply(X = seq_along(landscape), FUN = function(x) {

        if (progress) {

            message("\r> Progress nlayers: ", x , "/", length(landscape),
                    appendLF = FALSE)
        }

        scale_lsm_int_multibuffer(landscape = landscape[[x]],
                       y = y,
                       buffer_width = buffer_width,
                       max_width = max_width,
                       verbose = verbose,
                       progress = FALSE,
                       ...)
    })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    if (progress) {message("")}

    if(any(result < 99)){
        warning(c("Some of buffers extend over the landscape border. ",
                  "Consider decreasing of the max_width argument value."),
                call. = FALSE)
    }

    result[with(result, order(layer, plot_id, level, metric, class, id, value, size, percentage_inside)), ]
}

#' @name scale_lsm
#' @export
scale_lsm.stars <- function(landscape,
                             y,
                             buffer_width, max_width,
                             verbose = TRUE,
                             progress = FALSE,
                             ...) {

    landscape <-  raster::as.list(methods::as(landscape, "Raster"))

    result <- lapply(X = seq_along(landscape), FUN = function(x) {

        if (progress) {

            message("\r> Progress nlayers: ", x , "/", length(landscape),
                    appendLF = FALSE)
        }

        scale_lsm_int_multibuffer(landscape = landscape[[x]],
                       y = y,
                       buffer_width = buffer_width,
                       max_width = max_width,
                       verbose = verbose,
                       progress = FALSE,
                       ...)
    })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    if (progress) {message("")}

    if(any(result < 99)){
        warning(c("Some of buffers extend over the landscape border. ",
                  "Consider decreasing of the max_width argument value."),
                call. = FALSE)
    }

    result[with(result, order(layer, plot_id, level, metric, class, id, value, size, percentage_inside)), ]
}

#' @name scale_lsm
#' @export
scale_lsm.list <- function(landscape,
                            y,
                            buffer_width, max_width,
                            verbose = TRUE,
                            progress = FALSE,
                            ...) {

    result <- lapply(X = seq_along(landscape), FUN = function(x) {

        if (progress) {

            message("\r> Progress nlayers: ", x , "/", length(landscape),
                    appendLF = FALSE)
        }

        scale_lsm_int_multibuffer(landscape = landscape[[x]],
                       y = y,
                       buffer_width = buffer_width,
                       max_width = max_width,
                       verbose = verbose,
                       progress = FALSE,
                       ...)
    })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    if (progress) {message("")}

    if(any(result < 99)){
        warning(c("Some of buffers extend over the landscape border. ",
                  "Consider decreasing of the max_width argument value."),
                call. = FALSE)
    }

    result[with(result, order(layer, plot_id, level, metric, class, id, value, size, percentage_inside)), ]
}

scale_lsm_int <- function(size,
                           landscape,
                           y,
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
                              shape = "circle",
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

        # print progess using the non-internal name
        if (progress) {

            message("\r> Progress sample plots: ", current_plot, "/",
                    number_plots, appendLF = FALSE)
        }

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
    })
    )

    if (progress) {

        message("")
    }

    return(result)
}

scale_lsm_int_multibuffer <- function(landscape,
                           y,
                           buffer_width, max_width,
                           verbose,
                           progress,
                           ...) {
    buffers <- seq(buffer_width, max_width, buffer_width)

    result <- do.call(rbind, lapply(X = buffers, FUN = scale_lsm_int, landscape, y, verbose = verbose, progress = progress, ...))

    return(result)
}
