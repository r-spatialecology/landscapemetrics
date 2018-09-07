#' sample_lsm
#'
#' @description Sample metrics
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param what Selected level of metrics: either "all", "patch", "class", "landscape".
#' The default is "all". It is also possible to specify functions as a vector of strings,
#' e.g. what = c("lsm_c_ca", "lsm_l_ta").
#' @param points SpatialPoints, sf or 2-column matrix with coordinates of sample points
#' @param shape String specifying plot shape. Either "circle" or "square"
#' @param size Approximated size of sample plot. Equals the radius for circles or the
#' side-length for squares in mapunits
#' @param return_plots Logical if the clipped raster of the sample plot should
#' be returned
#' @param ... Options for calculate_lsm()
#'
#' @details
#' This function samples the selected metrics in a buffer area (sample plot)
#' around sample points. The size of the acutal sampled landscape can be different
#' to the provided size. This can be due to two reasons. Firstly, because clipping raster
#' cells using a circle leads to inaccuracies. Secondly, sample plots can exceed the
#' landscape boundary. Therefore, we report the relative area of each sample in
#' relation to the area of the sample plot, e.g. a sample plot only half within the
#' landscape will have a `percentage_inside = 50`. Please be aware that the
#' output is sligthly different to all other `lsm`-function
#' of `landscapemetrics` because the plot_id and the is included in the result.
#'
#' @return tibble
#'
#' @examples
#' \dontrun{
#' points <- matrix(c(10, 5, 25, 15, 5, 25), ncol = 2, byrow = TRUE)
#' sample_lsm(landscape, points = points, size = 15, what = "lsm_l_np")
#'
#' points_sp <- sp::SpatialPoints(points)
#' sample_lsm(landscape, points = points_sp, size = 15, what = "lsm_l_np")
#'
#' }
#'
#' @aliases sample_lsm
#' @rdname sample_lsm
#'
#' @export
sample_lsm <- function(landscape, what,
                           shape, points, size, return_plots, ...) UseMethod("sample_lsm")


#' @name sample_lsm
#' @export
sample_lsm.RasterLayer <- function(landscape,
                                       what = "all",
                                       shape = "square", points, size,
                                       return_plots = FALSE,
                                       ...) {

    result <- sample_lsm_int(landscape,
                             what = what,
                             shape = shape, points = points, size = size,
                             ...)

    if(return_plots == FALSE) {
        result  <- dplyr::bind_rows(result$metrics)
    }

    return(result)
}

#' @name sample_lsm
#' @export
sample_lsm.RasterStack <- function(landscape,
                                   what = "all",
                                   shape = "square", points, size,
                                   return_plots = FALSE,
                                   ...) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = sample_lsm_int,
                     what = what,
                     shape = shape,
                     points = points,
                     size = size,
                     ...)

    result <- dplyr::bind_rows(result)

    layer_id <- rep(x = 1:raster::nlayers(landscape), each = nrow(points))

    metrics_long <- dplyr::bind_rows(result$metrics)
    metrics_long[,1] <- layer_id
    metrics_long_list <- split(metrics_long, metrics_long[, c(1,7)])

    result$metrics <- metrics_long_list

    if(return_plots == FALSE) {
        result  <- dplyr::bind_rows(result$metrics)
    }

    return(result)
}

#' @name sample_lsm
#' @export
sample_lsm.RasterBrick <- function(landscape,
                                       what = "all",
                                       shape = "square", points, size,
                                       return_plots = FALSE,
                                       ...) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = sample_lsm_int,
                     what = what,
                     shape = shape,
                     points = points,
                     size = size,
                     ...)

    result <- dplyr::bind_rows(result)

    layer_id <- rep(x = 1:raster::nlayers(landscape), each = nrow(points))

    metrics_long <- dplyr::bind_rows(result$metrics)
    metrics_long[,1] <- layer_id
    metrics_long_list <- split(metrics_long, metrics_long[, c(1,7)])

    result$metrics <- metrics_long_list

    if(return_plots == FALSE) {
        result  <- dplyr::bind_rows(result$metrics)
    }

    return(result)
}

#' @name sample_lsm
#' @export
sample_lsm.list <- function(landscape,
                                what = "all",
                                shape = "square", points, size,
                                return_plots = FALSE,
                                ...) {

    result <- lapply(X = landscape,
                     FUN = sample_lsm_int,
                     what = what,
                     shape = shape,
                     points = points,
                     size = size,
                     ...)

    result <- dplyr::bind_rows(result)

    layer_id <- rep(x = 1:raster::nlayers(landscape), each = nrow(points))

    metrics_long <- dplyr::bind_rows(result$metrics)
    metrics_long[,1] <- layer_id
    metrics_long_list <- split(metrics_long, metrics_long[, c(1,7)])

    result$metrics <- metrics_long_list

    if(return_plots == FALSE) {
        result  <- dplyr::bind_rows(result$metrics)
    }

    return(result)
}

sample_lsm_int <- function(landscape, what, shape, points, size, ...) {

    if (shape == "circle") {
        maximum_area <- (pi * size ^ 2) / 10000
    }

    else if (shape == "square") {
        maximum_area <- (size ^ 2) / 10000
    }

    else{
        stop(paste0("Shape=", shape, " unknown"))
    }

    sample_plots <- construct_buffer(points = points,
                                     shape = shape,
                                     size = size)

    landscape_plots <- lapply(X = seq_along(sample_plots),
                              FUN = function(current_plot) {
                                  landscape_crop <- raster::crop(x = landscape, y = sample_plots[current_plot])
                                  landscape_mask <- raster::mask(x = landscape_crop, mask = sample_plots[current_plot])
                                  }
                              )

    results_landscapes <- lapply(X = seq_along(landscape_plots),
                                 FUN = function(current_plot) {
                                     area <- dplyr::pull(lsm_l_ta(landscape_plots[[current_plot]]), value)

                                     dplyr::mutate(
                                         calculate_lsm(landscape = landscape_plots[[current_plot]], what = what, ...),
                                         plot_id = current_plot, percentage_inside = (area / maximum_area) * 100)
                                     }
                                 )

    results <- tibble::enframe(results_landscapes, name = "plot_id", value = "metrics")

    results_total <- dplyr::mutate(results, raster_sample_plots = landscape_plots)

    return(results_total)
}
