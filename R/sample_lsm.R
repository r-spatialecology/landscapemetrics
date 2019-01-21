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

    result <- sample_lsm_int(landscape = landscape,
                             points = points, shape = shape, size = size,
                             what = what,
                             level = level,
                             metric = metric,
                             name = name,
                             type = type)

    # if(return_raster == FALSE) {
    #     result  <- dplyr::bind_rows(result$metrics)
    # }
    # else {
    #     result <- dplyr::mutate(result, layer = as.integer(1))
    #     result <- result[, c(4, 1, 2, 3)]
    # }

    if(!isTRUE(return_raster)) {
        result  <- dplyr::select(result, -raster_sample_plots)
    }

    return(result)
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

    result <- dplyr::bind_rows(result)

    layer_id <- rep(x = seq_len(raster::nlayers(landscape)),
                    each = nrow(as.data.frame(points)))

    # for(current_layer in seq_len(nrow(result))) {
    #     result$metrics[[current_layer]]$layer <- layer_id[current_layer]
    # }
    #
    # if(return_raster == FALSE) {
    #     result  <- dplyr::bind_rows(result$metrics)
    # }
    #
    # else {
    #     result <- dplyr::mutate(result, layer = as.integer(layer_id))
    #     result <- result[, c(4, 1, 2, 3)]
    # }

    result$layer <- layer_id

    if(!isTRUE(return_raster)) {
        result  <- dplyr::select(result, -raster_sample_plots)
    }

    return(result)
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

    result <- dplyr::bind_rows(result)

    layer_id <- rep(x = seq_len(raster::nlayers(landscape)),
                    each = nrow(as.data.frame(points)))

    # for(current_layer in seq_len(nrow(result))) {
    #     result$metrics[[current_layer]]$layer <- layer_id[current_layer]
    # }
    #
    # if(return_raster == FALSE) {
    #     result  <- dplyr::bind_rows(result$metrics)
    # }
    #
    # else {
    #     result <- dplyr::mutate(result, layer = as.integer(layer_id))
    #     result <- result[, c(4, 1, 2, 3)]
    # }

    result$layer <- layer_id

    if(!isTRUE(return_raster)) {
        result  <- dplyr::select(result, -raster_sample_plots)
    }

    return(result)
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

    result <- dplyr::bind_rows(result)

    layer_id <- rep(x = seq_along(landscape), each = nrow(points))

    # for(current_layer in seq_len(nrow(result))) {
    #     result$metrics[[current_layer]]$layer <- layer_id[current_layer]
    # }
    #
    # if(return_raster == FALSE) {
    #     result  <- dplyr::bind_rows(result$metrics)
    # }
    #
    # else {
    #     result <- dplyr::mutate(result, layer = as.integer(layer_id))
    #     result <- result[, c(4, 1, 2, 3)]
    # }

    result$layer <- layer_id

    if(!isTRUE(return_raster)) {
        result  <- dplyr::select(result, -raster_sample_plots)
    }

    return(result)
}

sample_lsm_int <- function(landscape,
                           points, shape, size,
                           what, level, metric, name, type) {

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
                                  landscape_crop <- raster::crop(x = landscape,
                                                                 y = sample_plots[current_plot])
                                  landscape_mask <- raster::mask(x = landscape_crop,
                                                                 mask = sample_plots[current_plot])
                                  names(landscape_mask) <- paste0("plot_", current_plot)
                                  return(landscape_mask)
                                  }
                              )

    results_landscapes <- lapply(X = seq_along(landscape_plots),
                                 FUN = function(current_plot) {

                                     area <- lsm_l_ta_calc(landscape_plots[[current_plot]],
                                                           directions = 8)

                                    result <- calculate_lsm(landscape = landscape_plots[[current_plot]],
                                                            what = what,
                                                            level = level,
                                                            metric = metric,
                                                            name = name,
                                                            type = type)

                                     result_plot <- dplyr::mutate(result,
                                                                  plot_id = current_plot,
                                                                  percentage_inside = (area$value / maximum_area) * 100)

                                     result_plot <- result_plot[, c(1, 7, 2, 3, 4, 5, 6, 8)]

                                     return(result_plot)
                                     }
                                 )

    # results <- tibble::enframe(results_landscapes, name = "plot_id", value = "metrics")
    #
    # results_total <- dplyr::mutate(results, raster_sample_plots = landscape_plots)
    #
    # return(results_total)

    results <- dplyr::bind_rows(results_landscapes)

    results$raster_sample_plots <- landscape_plots[results$plot_id]

    return(results)
}
