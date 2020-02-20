#' window_lsm_old
#'
#' @description Moving window
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param window matrix
#' @param metric Abbreviation of metrics (e.g. 'area').
#' @param name Full name of metrics (e.g. 'core area')
#' @param type Type according to FRAGSTATS grouping (e.g. 'aggregation metrics').
#' @param what Selected level of metrics: either "patch", "class" or "landscape".
#' It is also possible to specify functions as a vector of strings, e.g. `what = c("lsm_c_ca", "lsm_l_ta")`.
#' @param progress Print progress report.
#' @param ... Arguments passed on to \code{calculate_lsm()}.
#'
#' @details
#' The function calculates for each focal cell the selected landscape metrics (currently only landscape level
#' metrics are allowed) for a local neighbourhood. The neighbourhood can be specified using a matrix. For more
#' details, see \code{?raster::focal()}. The result will be a \code{RasterLayer} in which each focal cell includes
#' the value of its neighbourhood and thereby allows to show gradients and variability in the landscape (Hagen-Zanker 2016).
#' To be type stable, the acutally result is always a nested list (first level for \code{RasterStack} layers, second level
#' for selected landscape metrics).
#'
#' @seealso
#' \code{\link{list_lsm}} \cr
#' \code{\link{calculate_lsm}} \cr
#' \code{\link{focal}}
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' window <- matrix(1, nrow = 5,ncol = 5)
#' window_lsm_old(landscape, window = window, what = c("lsm_l_pr", "lsm_l_joinent"))
#' window_lsm_old(landscape_stack, window = window, what = c("lsm_l_pr", "lsm_l_joinent"))
#' }
#'
#' @aliases window_lsm_old
#' @rdname window_lsm_old
#'
#' @references
#' Fletcher, R., Fortin, M.-J. 2018. Spatial Ecology and Conservation Modeling: Applications
#' with R. Springer International Publishing. 523 pages
#'
#' Hagen-Zanker, A. (2016). A computational framework for generalized moving windows
#' and its application to landscape pattern analysis. International journal of applied
#' earth observation and geoinformation, 44, 205-216.
#'
#' McGarigal, K., Cushman, S.A., and Ene E. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' website: <http://www.umass.edu/landeco/research/fragstats/fragstats.html>
#'
#' @export
window_lsm_old <- function(landscape, window,
                       metric, name, type, what,
                       progress,
                       ...) UseMethod("window_lsm_old")


#' @name window_lsm_old
#' @export
window_lsm_old.RasterLayer <- function(landscape,
                                   window,
                                   metric = NULL,
                                   name = NULL,
                                   type = NULL,
                                   what = NULL,
                                   progress = FALSE,
                                   ...) {

    # get list of metrics to calculate
    metrics_list <- list_lsm(level = "landscape",
                             metric = metric,
                             name = name,
                             type = type,
                             what = what,
                             simplify = TRUE,
                             verbose = FALSE)

    number_metrics <- length(metrics_list)

    # check if non-landscape-level metrics are selected
    if (!all(metrics_list %in% list_lsm(level = "landscape", simplify = TRUE))) {

        stop("'window_lsm_old()' is only able to calculate landscape level metrics.",
             call. = FALSE)
    }

    # print warnings immediately to capture
    options(warn = 1)

    # open text connection for warnings
    text_connection <- textConnection(object = "warn_messages",
                                      open = "w", local = TRUE)
    sink(file = text_connection, type = "message", append = TRUE)

    result <- lapply(raster::as.list(landscape), function(current_landscape) {

        # get coordinates of cells
        points <- raster_to_points(current_landscape)[, 2:4]

        # resolution of original raster
        resolution <- raster::res(current_landscape)

        # get dimensions of window
        n_row = nrow(window)
        n_col = ncol(window)

        result_layer <- lapply(seq_along(metrics_list), function(current_metric) {

            # print progess using the non-internal name
            if (progress) {

                cat("\r> Progress metrics: ", current_metric, "/", number_metrics)
            }

            raster::focal(x = current_landscape, w = window, fun = function(x) {

                calculate_lsm_focal_old(landscape = x,
                                    n_row = n_row,
                                    n_col = n_col,
                                    resolution = resolution,
                                    points = points,
                                    what = metrics_list[[current_metric]],
                                    ...)},
                pad = TRUE, padValue = NA)
        })

        names(result_layer) <- metrics_list

        return(result_layer)
    })

    if (progress) {cat("\n")}

    # reset warning options
    options(warn = 0)

    # close text connection
    sink(NULL, type = "message")
    close(text_connection)

    # only unique warnings
    warn_messages <- unique(warn_messages)

    # removing "Warning:" -> will be added by warning()
    warn_messages <- gsub(pattern = "Warning: ", replacement = "",
                          x = warn_messages)

    # print warnings
    lapply(warn_messages, function(x){ warning(x, call. = FALSE)})

    return(result)
}

#' @name window_lsm_old
#' @export
window_lsm_old.RasterStack <- function(landscape,
                                   window,
                                   metric = NULL,
                                   name = NULL,
                                   type = NULL,
                                   what = NULL,
                                   progress = FALSE,
                                   ...) {

    # get list of metrics to calculate
    metrics_list <- list_lsm(level = "landscape",
                             metric = metric,
                             name = name,
                             type = type,
                             what = what,
                             simplify = TRUE,
                             verbose = FALSE)

    # check if non-landscape-level metrics are selected
    if (!all(metrics_list %in% list_lsm(level = "landscape", simplify = TRUE))) {

        stop("'window_lsm_old()' is only able to calculate landscape level metrics.",
             call. = FALSE)
    }

    # print warnings immediately to capture
    options(warn = 1)

    # open text connection for warnings
    text_connection <- textConnection(object = "warn_messages",
                                      open = "w", local = TRUE)
    sink(file = text_connection, type = "message", append = TRUE)

    # convert to list
    landscape <- raster::as.list(landscape)

    # how many landscapes are present for progress
    number_layers <- length(landscape)

    result <- lapply(seq_along(landscape), function(current_landscape) {

        # print progess using the non-internal name
        if (progress) {

            cat("\r> Progress nlayers: ", current_landscape, "/", number_layers)
        }

        # get coordinates of cells
        points <- raster_to_points(landscape[[current_landscape]])[, 2:4]

        # resolution of original raster
        resolution <- raster::res(landscape[[current_landscape]])

        # get dimensions of window
        n_row = nrow(window)
        n_col = ncol(window)

        result_layer <- lapply(metrics_list, function(current_metric) {

            raster::focal(x = landscape[[current_landscape]], w = window, fun = function(x) {

                calculate_lsm_focal_old(landscape = x,
                                    n_row = n_row,
                                    n_col = n_col,
                                    resolution = resolution,
                                    points = points,
                                    what = current_metric,
                                    ...)},
                pad = TRUE, padValue = NA)
        })

        names(result_layer) <- metrics_list

        return(result_layer)
    })

    if (progress) {cat("\n")}

    # reset warning options
    options(warn = 0)

    # close text connection
    sink(NULL, type = "message")
    close(text_connection)

    # only unique warnings
    warn_messages <- unique(warn_messages)

    # removing "Warning:" -> will be added by warning()
    warn_messages <- gsub(pattern = "Warning: ", replacement = "",
                          x = warn_messages)

    # print warnings
    lapply(warn_messages, function(x){ warning(x, call. = FALSE)})

    return(result)
}

#' @name window_lsm_old
#' @export
window_lsm_old.RasterBrick <- function(landscape,
                                   window,
                                   metric = NULL,
                                   name = NULL,
                                   type = NULL,
                                   what = NULL,
                                   progress = FALSE,
                                   ...) {

    # get list of metrics to calculate
    metrics_list <- list_lsm(level = "landscape",
                             metric = metric,
                             name = name,
                             type = type,
                             what = what,
                             simplify = TRUE,
                             verbose = FALSE)

    # check if non-landscape-level metrics are selected
    if (!all(metrics_list %in% list_lsm(level = "landscape", simplify = TRUE))) {

        stop("'window_lsm_old()' is only able to calculate landscape level metrics.",
             call. = FALSE)
    }

    # print warnings immediately to capture
    options(warn = 1)

    # open text connection for warnings
    text_connection <- textConnection(object = "warn_messages",
                                      open = "w", local = TRUE)
    sink(file = text_connection, type = "message", append = TRUE)

    # convert to list
    landscape <- raster::as.list(landscape)

    # how many landscapes are present for progress
    number_layers <- length(landscape)

    result <- lapply(seq_along(landscape), function(current_landscape) {

        # print progess using the non-internal name
        if (progress) {

            cat("\r> Progress nlayers: ", current_landscape, "/", number_layers)
        }

        # get coordinates of cells
        points <- raster_to_points(landscape[[current_landscape]])[, 2:4]

        # resolution of original raster
        resolution <- raster::res(landscape[[current_landscape]])

        # get dimensions of window
        n_row = nrow(window)
        n_col = ncol(window)

        result_layer <- lapply(metrics_list, function(current_metric) {

            raster::focal(x = landscape[[current_landscape]], w = window, fun = function(x) {

                calculate_lsm_focal_old(landscape = x,
                                    n_row = n_row,
                                    n_col = n_col,
                                    resolution = resolution,
                                    points = points,
                                    what = current_metric,
                                    ...)},
                pad = TRUE, padValue = NA)
        })

        names(result_layer) <- metrics_list

        return(result_layer)
    })

    if (progress) {cat("\n")}

    # reset warning options
    options(warn = 0)

    # close text connection
    sink(NULL, type = "message")
    close(text_connection)

    # only unique warnings
    warn_messages <- unique(warn_messages)

    # removing "Warning:" -> will be added by warning()
    warn_messages <- gsub(pattern = "Warning: ", replacement = "",
                          x = warn_messages)

    # print warnings
    lapply(warn_messages, function(x){ warning(x, call. = FALSE)})

    return(result)
}

#' @name window_lsm_old
#' @export
window_lsm_old.stars <- function(landscape,
                             window,
                             metric = NULL,
                             name = NULL,
                             type = NULL,
                             what = NULL,
                             progress = FALSE,
                             ...) {

    # get list of metrics to calculate
    metrics_list <- list_lsm(level = "landscape",
                             metric = metric,
                             name = name,
                             type = type,
                             what = what,
                             simplify = TRUE,
                             verbose = FALSE)

    # check if non-landscape-level metrics are selected
    if (!all(metrics_list %in% list_lsm(level = "landscape", simplify = TRUE))) {

        stop("'window_lsm_old()' is only able to calculate landscape level metrics.",
             call. = FALSE)
    }

    # print warnings immediately to capture
    options(warn = 1)

    # open text connection for warnings
    text_connection <- textConnection(object = "warn_messages",
                                      open = "w", local = TRUE)
    sink(file = text_connection, type = "message", append = TRUE)

    # convert to list
    landscape <- raster::as.list(methods::as(landscape, "Raster"))

    # how many landscapes are present for progress
    number_layers <- length(landscape)

    result <- lapply(seq_along(landscape), function(current_landscape) {

        # print progess using the non-internal name
        if (progress) {

            cat("\r> Progress nlayers: ", current_landscape, "/", number_layers)
        }

        # get coordinates of cells
        points <- raster_to_points(landscape[[current_landscape]])[, 2:4]

        # resolution of original raster
        resolution <- raster::res(landscape[[current_landscape]])

        # get dimensions of window
        n_row = nrow(window)
        n_col = ncol(window)

        result_layer <- lapply(metrics_list, function(current_metric) {

            raster::focal(x = landscape[[current_landscape]], w = window, fun = function(x) {

                calculate_lsm_focal_old(landscape = x,
                                    n_row = n_row,
                                    n_col = n_col,
                                    resolution = resolution,
                                    points = points,
                                    what = current_metric,
                                    ...)},
                pad = TRUE, padValue = NA)
        })

        names(result_layer) <- metrics_list

        return(result_layer)
    })

    if (progress) {cat("\n")}

    # reset warning options
    options(warn = 0)

    # close text connection
    sink(NULL, type = "message")
    close(text_connection)

    # only unique warnings
    warn_messages <- unique(warn_messages)

    # removing "Warning:" -> will be added by warning()
    warn_messages <- gsub(pattern = "Warning: ", replacement = "",
                          x = warn_messages)

    # print warnings
    lapply(warn_messages, function(x){ warning(x, call. = FALSE)})

    return(result)
}

#' @name window_lsm_old
#' @export
window_lsm_old.list <- function(landscape,
                            window,
                            metric = NULL,
                            name = NULL,
                            type = NULL,
                            what = NULL,
                            progress = FALSE,
                            ...) {

    # get list of metrics to calculate
    metrics_list <- list_lsm(level = "landscape",
                             metric = metric,
                             name = name,
                             type = type,
                             what = what,
                             simplify = TRUE,
                             verbose = FALSE)

    # check if non-landscape-level metrics are selected
    if (!all(metrics_list %in% list_lsm(level = "landscape", simplify = TRUE))) {

        stop("'window_lsm_old()' is only able to calculate landscape level metrics.",
             call. = FALSE)
    }

    # print warnings immediately to capture
    options(warn = 1)

    # open text connection for warnings
    text_connection <- textConnection(object = "warn_messages",
                                      open = "w", local = TRUE)
    sink(file = text_connection, type = "message", append = TRUE)

    # how many landscapes are present for progress
    number_layers <- length(landscape)

    result <- lapply(seq_along(landscape), function(current_landscape) {

        # print progess using the non-internal name
        if (progress) {

            cat("\r> Progress nlayers: ", current_landscape, "/", number_layers)
        }

        # get coordinates of cells
        points <- raster_to_points(landscape[[current_landscape]])[, 2:4]

        # resolution of original raster
        resolution <- raster::res(landscape[[current_landscape]])

        # get dimensions of window
        n_row = nrow(window)
        n_col = ncol(window)

        result_layer <- lapply(metrics_list, function(current_metric) {

            raster::focal(x = landscape[[current_landscape]], w = window, fun = function(x) {

                calculate_lsm_focal_old(landscape = x,
                                    n_row = n_row,
                                    n_col = n_col,
                                    resolution = resolution,
                                    points = points,
                                    what = current_metric,
                                    ...)},
                pad = TRUE, padValue = NA)
        })

        names(result_layer) <- metrics_list

        return(result_layer)
    })

    if (progress) {cat("\n")}

    # reset warning options
    options(warn = 0)

    # close text connection
    sink(NULL, type = "message")
    close(text_connection)

    # only unique warnings
    warn_messages <- unique(warn_messages)

    # removing "Warning:" -> will be added by warning()
    warn_messages <- gsub(pattern = "Warning: ", replacement = "",
                          x = warn_messages)

    # print warnings
    lapply(warn_messages, function(x){ warning(x, call. = FALSE)})

    return(result)
}

calculate_lsm_focal_old <- function(landscape,
                                n_row,
                                n_col,
                                resolution,
                                points,
                                what,
                                ...) {

    # convert focal window to matrix
    raster_window <- matrix(landscape, n_row, n_col)

    # get internal calculation function
    what <- paste0(what, "_calc")

    # match function name
    foo <- get(what, mode = "function")

    # get argument
    arguments <- names(formals(foo))[-1]

    arguments_values <- list(resolution = resolution,
                             points = points,
                             directions = 8,
                             count_boundary = FALSE,
                             consider_boundary = FALSE,
                             edge_depth = 1,
                             classes_max = NULL,
                             neighbourhood = 4,
                             ordered = TRUE,
                             base = "log2",
                             verbose = TRUE)

    # which arguments are needed
    arguments_values <- arguments_values[names(arguments_values) %in% arguments]

    # sort alphabetically to match later with provided
    arguments_values <- arguments_values[order(names(arguments_values))]

    # get provided arguments
    arguments_provided <- substitute(...())

    # sort alphabetically to match later with defaults
    if (!is.null(arguments_provided)) {
        arguments_provided <- arguments_provided[order(names(arguments_provided))]

        # exchange arguments
        arguments_values[names(arguments_values) %in% names(arguments_provided)] <- arguments_provided
    }

    # landscape argument
    arguments_values$landscape <- raster_window

    # run function
    result <- do.call(what = foo,
                      args = arguments_values)

    return(result$value)
}

