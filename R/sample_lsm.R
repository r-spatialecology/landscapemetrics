#' sample_lsm
#'
#' @description Sample metrics
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param points SpatialPoints, sf or 2-column matrix with coordinates of sample points
#' @param shape String specifying plot shape. Either "circle" or "square"
#' @param size Approximated size of sample plot. Equals the radius for circles or the
#' side-length for squares in mapunits
#' @param what Selected level of metrics: either "patch", "class" or "landscape".
#' It is also possible to specify functions as a vector of strings, e.g. `what = c("lsm_c_ca", "lsm_l_ta")`.
#' @param level Level of metrics to calculate (e.g. 'landscape').
#' @param metric Abbreviation of metrics to calculate (e.g. 'area').
#' @param name Full name of metrics to calculate (e.g. 'core area').
#' @param type Metric types to calculate according to FRAGSTATS grouping (e.g. 'aggregation metric').
#' @param return_raster Logical if the clipped raster of the sample plot should
#' be returned
#'
#' @details
#' This function samples the selected metrics in a buffer area (sample plot)
#' around sample points. The size of the actual sampled landscape can be different
#' to the provided size due to two reasons. Firstly, because clipping raster
#' cells using a circle or a sample plot not directly at a cell center lead
#' to inaccuracies. Secondly, sample plots can exceed the landscape boundary.
#' Therefore, we report the actual clipped sample plot area relative in relation
#' to the theoretical, maximum sample plot area e.g. a sample plot only half within
#' the landscape will have a `percentage_inside = 50`. Please be aware that the
#' output is sligthly different to all other `lsm`-function of `landscapemetrics`.
#'
#' The metrics can be specified by the arguments `what`, `level`, `metric`, `name`
#' and/or `type` (combinations of different arguments are possible (e.g.
#' `level = "class", type = "aggregation metric"`). If an argument is not provided,
#' automatically all possibilities are selected. Therefore, to get **all**
#' available metrics, don't specify any of the above arguments.
#'
#' @seealso
#' \code{\link{list_lsm}}
#'
#' @return tibble
#'
#' @examples
#' points <- matrix(c(10, 5, 25, 15, 5, 25), ncol = 2, byrow = TRUE)
#' sample_lsm(landscape, points = points, size = 15, what = "lsm_l_np")
#'
#' points_sp <- sp::SpatialPoints(points)
#' sample_lsm(landscape, points = points_sp, size = 15, what = "lsm_l_np", return_raster = TRUE)
#'
#' @aliases sample_lsm
#' @rdname sample_lsm
#'
#' @export
sample_lsm <- function(landscape,
                       points, shape, size,
                       what, level, metric, name, type,
                       return_raster) UseMethod("sample_lsm")


#' @name sample_lsm
#' @export
sample_lsm.RasterLayer <- function(landscape,
                                   points, shape = "square", size,
                                   what = NULL,
                                   level = NULL,
                                   metric = NULL,
                                   name = NULL,
                                   type = NULL,
                                   return_raster = FALSE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = sample_lsm_int,
                     points = points, shape = shape, size = size,
                     what = what,
                     level = level,
                     metric = metric,
                     name = name,
                     type = type)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    if(!isTRUE(return_raster)) {
        result  <- result[, -9]
    }

    result[with(result, order(layer, plot_id, level, metric, class, id)), ]
}

#' @name sample_lsm
#' @export
sample_lsm.RasterStack <- function(landscape,
                                   points, shape = "square", size,
                                   what = NULL,
                                   level = NULL,
                                   metric = NULL,
                                   name = NULL,
                                   type = NULL,
                                   return_raster = FALSE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = sample_lsm_int,
                     points = points, shape = shape, size = size,
                     what = what,
                     level = level,
                     metric = metric,
                     name = name,
                     type = type)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    if(!isTRUE(return_raster)) {
        result  <- result[, -9]
    }

    result[with(result, order(layer, plot_id, level, metric, class, id)), ]
}

#' @name sample_lsm
#' @export
sample_lsm.RasterBrick <- function(landscape,
                                   points, shape = "square", size,
                                   what = NULL,
                                   level = NULL,
                                   metric = NULL,
                                   name = NULL,
                                   type = NULL,
                                   return_raster = FALSE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = sample_lsm_int,
                     points = points, shape = shape, size = size,
                     what = what,
                     level = level,
                     metric = metric,
                     name = name,
                     type = type)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    if(!isTRUE(return_raster)) {
        result  <- result[, -9]
    }

    result[with(result, order(layer, plot_id, level, metric, class, id)), ]
}

#' @name sample_lsm
#' @export
sample_lsm.list <- function(landscape,
                            points, shape = "square", size,
                            what = NULL,
                            level = NULL,
                            metric = NULL,
                            name = NULL,
                            type = NULL,
                            return_raster = FALSE) {

    result <- lapply(X = landscape,
                     FUN = sample_lsm_int,
                     points = points, shape = shape, size = size,
                     what = what,
                     level = level,
                     metric = metric,
                     name = name,
                     type = type)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    if(!isTRUE(return_raster)) {
        result  <- result[, -9]
    }

    result[with(result, order(layer, plot_id, level, metric, class, id)), ]
}

sample_lsm_int <- function(landscape,
                           points, shape, size,
                           what, level, metric, name, type) {

    # calculate theoretical, maximum area
    if (shape == "circle") {
        maximum_area <- (pi * size ^ 2) / 10000
    }

    # calculate theoretical, maximum area
    else if (shape == "square") {
        maximum_area <- (size ^ 2) / 10000
    }

    # Unkown shape argument
    else{
        stop(paste0("Shape = ", shape, " unknown"))
    }

    # construct plot area around sample points
    sample_plots <- construct_buffer(points = points,
                                     shape = shape,
                                     size = size)

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
                                             what = what,
                                             level = level,
                                             metric = metric,
                                             name = name,
                                             type = type)

        # add plot id
        result_current_plot$plot_id <- current_plot

        # calculate ratio between actual area and theoretical area
        result_current_plot$percentage_inside <- area$value / maximum_area * 100

        # add sample plot raster
        result_current_plot$raster_sample_plots <- raster::as.list(landscape_mask)

        return(result_current_plot)
        })
    )

    return(result)
}
