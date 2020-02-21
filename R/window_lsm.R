#' window_lsm
#'
#' @description Moving window
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param window Moving window matrix.
#' @param level Level of metrics. Either 'patch', 'class' or 'landscape' (or vector with combination).
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
#' window_lsm(landscape, window = window, what = c("lsm_l_pr", "lsm_l_joinent"))
#' window_lsm(landscape_stack, window = window, what = c("lsm_l_pr", "lsm_l_joinent"))
#' }
#'
#' @aliases window_lsm
#' @rdname window_lsm
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
window_lsm <- function(landscape, window,
                       level, metric, name, type, what,
                       progress,
                       ...) UseMethod("window_lsm")


#' @name window_lsm
#' @export
window_lsm.RasterLayer <- function(landscape,
                                   window,
                                   level = "landscape",
                                   metric = NULL,
                                   name = NULL,
                                   type = NULL,
                                   what = NULL,
                                   progress = FALSE,
                                   ...) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = window_lsm_int,
                     window = window,
                     level = level,
                     metric = metric,
                     name = name,
                     type = type,
                     what = what,
                     progress = progress,
                     ...)

    return(result)
}

#' @name window_lsm
#' @export
window_lsm.RasterStack <- function(landscape,
                                   window,
                                   level = "landscape",
                                   metric = NULL,
                                   name = NULL,
                                   type = NULL,
                                   what = NULL,
                                   progress = FALSE,
                                   ...) {

    landscape <- raster::as.list(landscape)

    result <- lapply(X = seq_along(landscape), FUN = function(x) {

        if (progress) {

            cat("\r> Progress nlayers: ", x , "/", length(landscape))
        }

        window_lsm_int(landscape = landscape[[x]],
                       window = window,
                       level = level,
                       metric = metric,
                       name = name,
                       type = type,
                       what = what,
                       progress = FALSE,
                       ...)
    })

    if (progress) {cat("\n")}

    return(result)
}

#' @name window_lsm
#' @export
window_lsm.RasterBrick <- function(landscape,
                                   window,
                                   level = "landscape",
                                   metric = NULL,
                                   name = NULL,
                                   type = NULL,
                                   what = NULL,
                                   progress = FALSE,
                                   ...) {

    landscape <- raster::as.list(landscape)

    result <- lapply(X = seq_along(landscape), FUN = function(x) {

        if (progress) {

            cat("\r> Progress nlayers: ", x , "/", length(landscape))
        }

        window_lsm_int(landscape = landscape[[x]],
                       window = window,
                       level = level,
                       metric = metric,
                       name = name,
                       type = type,
                       what = what,
                       progress = FALSE,
                       ...)
    })

    if (progress) {cat("\n")}

    return(result)
}

#' @name window_lsm
#' @export
window_lsm.stars <- function(landscape,
                             window,
                             level = "landscape",
                             metric = NULL,
                             name = NULL,
                             type = NULL,
                             what = NULL,
                             progress = FALSE,
                             ...) {

    landscape <-  raster::as.list(methods::as(landscape, "Raster"))

    result <- lapply(X = seq_along(landscape), FUN = function(x) {

        if (progress) {

            cat("\r> Progress nlayers: ", x , "/", length(landscape))
        }

        window_lsm_int(landscape = landscape[[x]],
                       window = window,
                       level = level,
                       metric = metric,
                       name = name,
                       type = type,
                       what = what,
                       progress = FALSE,
                       ...)
    })

    if (progress) {cat("\n")}

    return(result)
}

#' @name window_lsm
#' @export
window_lsm.list <- function(landscape,
                            window,
                            level = "landscape",
                            metric = NULL,
                            name = NULL,
                            type = NULL,
                            what = NULL,
                            progress = FALSE,
                            ...) {

    result <- lapply(X = seq_along(landscape), FUN = function(x) {

        if (progress) {

            cat("\r> Progress nlayers: ", x , "/", length(landscape))
        }

        window_lsm_int(landscape = landscape[[x]],
                       window = window,
                       level = level,
                       metric = metric,
                       name = name,
                       type = type,
                       what = what,
                       progress = FALSE,
                       ...)
    })

    if (progress) {cat("\n")}

    return(result)
}

window_lsm_int <- function(landscape,
                           window,
                           level,
                           metric,
                           name,
                           type,
                           what,
                           progress,
                           ...) {

    # check if window has uneven sides
    if (any(dim(window) %% 2 == 0)) {

        stop("The window must have uneven sides.", call. = FALSE)
    }

    # get list of metrics to calculate
    metrics_list <- list_lsm(level = level,
                             metric = metric,
                             name = name,
                             type = type,
                             what = what,
                             simplify = TRUE,
                             verbose = FALSE)

    number_metrics <- length(metrics_list)

    # check if non-landscape-level metrics are selected
    if (!all(metrics_list %in% list_lsm(level = "landscape", simplify = TRUE))) {

        stop("'window_lsm()' is only able to calculate landscape level metrics.",
             call. = FALSE)
    }

    # get coordinates of cells
    points <- raster_to_points(landscape)[, 2:4]

    # resolution of original raster
    resolution <- raster::res(landscape)

    # get dimensions of window
    n_row = nrow(window)
    n_col = ncol(window)

    # create object for warning messages
    warning_messages <- character(0)

    result <- withCallingHandlers(expr = {lapply(seq_along(metrics_list), function(current_metric) {

        # print progess using the non-internal name
        if (progress) {

            cat("\r> Progress metrics: ", current_metric, "/", number_metrics)
        }

        raster::focal(x = landscape, w = window, fun = function(x) {

            calculate_lsm_focal(landscape = x,
                                n_row = n_row,
                                n_col = n_col,
                                resolution = resolution,
                                points = points,
                                what = metrics_list[[current_metric]],
                                ...)},
            pad = TRUE, padValue = NA)
        })},
        warning = function(cond) {

            warning_messages <<- c(warning_messages, conditionMessage(cond))

            invokeRestart("muffleWarning")})

    names(result) <- metrics_list

    if (progress) {cat("\n")}

    # warnings present
    if (length(warning_messages)) {

        # only unique warnings
        warning_messages <- unique(warning_messages)

        # remove warning from creating raster
        remove_id <- which(warning_messages %in% c("no non-missing arguments to min; returning Inf",
                                                   "no non-missing arguments to max; returning -Inf"))

        if (length(remove_id)) {
            warning_messages <- warning_messages[-remove_id]
        }

        # still warnings present
        if (length(warning_messages)) {

            # print warnings
            lapply(warning_messages, function(x){ warning(x, call. = FALSE)})
        }
    }

    return(result)
}

calculate_lsm_focal <- function(landscape,
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

