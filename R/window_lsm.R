#' window_lsm
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
#' @param verbose Print warning messages.
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
                       metric, name, type, what,
                       verbose,
                       progress,
                       ...) UseMethod("window_lsm")


#' @name window_lsm
#' @export
window_lsm.RasterLayer <- function(landscape,
                                   window,
                                   metric = NULL,
                                   name = NULL,
                                   type = NULL,
                                   what = NULL,
                                   verbose = TRUE,
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

        stop("'window_lsm()' is only able to calculate landscape level metrics.",
             call. = FALSE)
    }

    result <- lapply(raster::as.list(landscape), function(current_landscape) {

        # get coordinates of cells
        points <- raster_to_points(current_landscape)[, 2:4]

        # resolution of original raster
        resolution <- raster::res(current_landscape)

        # get dimensions of window
        n_row = nrow(window)
        n_col = ncol(window)

        result_layer <- lapply(metrics_list, function(current_metric) {

            raster::focal(x = current_landscape, w = window, fun = function(x) {

                calculate_lsm_focal(landscape = x,
                                    n_row = n_row,
                                    n_col = n_col,
                                    resolution = resolution,
                                    points = points,
                                    what = current_metric,
                                    directions = directions,
                                    count_boundary = count_boundary,
                                    consider_boundary = consider_boundary,
                                    edge_depth = edge_depth,
                                    classes_max = classes_max,
                                    neighbourhood = neighbourhood,
                                    ordered = ordered,
                                    base = base,
                                    verbose = verbose)},
                pad = TRUE, padValue = NA)
        })

        names(result_layer) <- metrics_list

        return(result_layer)
    })

    return(result)
}

#' @name window_lsm
#' @export
window_lsm.RasterStack <- function(landscape,
                                   window,
                                   metric = NULL,
                                   name = NULL,
                                   type = NULL,
                                   what = NULL,
                                   verbose = TRUE,
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

        stop("'window_lsm()' is only able to calculate landscape level metrics.",
             call. = FALSE)
    }

    result <- lapply(raster::as.list(landscape), function(current_landscape) {

        # get coordinates of cells
        points <- raster_to_points(current_landscape)[, 2:4]

        # resolution of original raster
        resolution <- raster::res(current_landscape)

        # get dimensions of window
        n_row = nrow(window)
        n_col = ncol(window)

        result_layer <- lapply(metrics_list, function(current_metric) {

            raster::focal(x = current_landscape, w = window, fun = function(x) {

                calculate_lsm_focal(landscape = x,
                                    n_row = n_row,
                                    n_col = n_col,
                                    resolution = resolution,
                                    points = points,
                                    what = current_metric,
                                    directions = directions,
                                    count_boundary = count_boundary,
                                    consider_boundary = consider_boundary,
                                    edge_depth = edge_depth,
                                    classes_max = classes_max,
                                    neighbourhood = neighbourhood,
                                    ordered = ordered,
                                    base = base,
                                    verbose = verbose)},
                pad = TRUE, padValue = NA)
        })

        names(result_layer) <- metrics_list

        return(result_layer)
    })

    return(result)
}

#' @name window_lsm
#' @export
window_lsm.RasterBrick <- function(landscape,
                                   window,
                                   metric = NULL,
                                   name = NULL,
                                   type = NULL,
                                   what = NULL,
                                   verbose = TRUE,
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

        stop("'window_lsm()' is only able to calculate landscape level metrics.",
             call. = FALSE)
    }

    result <- lapply(raster::as.list(landscape), function(current_landscape) {

        # get coordinates of cells
        points <- raster_to_points(current_landscape)[, 2:4]

        # resolution of original raster
        resolution <- raster::res(current_landscape)

        # get dimensions of window
        n_row = nrow(window)
        n_col = ncol(window)

        result_layer <- lapply(metrics_list, function(current_metric) {

            raster::focal(x = current_landscape, w = window, fun = function(x) {

                calculate_lsm_focal(landscape = x,
                                    n_row = n_row,
                                    n_col = n_col,
                                    what = current_metric,
                                    resolution = resolution,
                                    points = points,
                                    directions = directions,
                                    count_boundary = count_boundary,
                                    consider_boundary = consider_boundary,
                                    edge_depth = edge_depth,
                                    classes_max = classes_max,
                                    neighbourhood = neighbourhood,
                                    ordered = ordered,
                                    base = base,
                                    verbose = verbose)},
                pad = TRUE, padValue = NA)
        })

        names(result_layer) <- metrics_list

        return(result_layer)
    })

    return(result)
}

#' @name window_lsm
#' @export
window_lsm.stars <- function(landscape,
                             window,
                             metric = NULL,
                             name = NULL,
                             type = NULL,
                             what = NULL,
                             verbose = TRUE,
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

        stop("'window_lsm()' is only able to calculate landscape level metrics.",
             call. = FALSE)
    }

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(raster::as.list(landscape), function(current_landscape) {

        # get coordinates of cells
        points <- raster_to_points(current_landscape)[, 2:4]

        # resolution of original raster
        resolution <- raster::res(current_landscape)

        # get dimensions of window
        n_row = nrow(window)
        n_col = ncol(window)

        result_layer <- lapply(metrics_list, function(current_metric) {

            raster::focal(x = current_landscape, w = window, fun = function(x) {

                calculate_lsm_focal(landscape = x,
                                    n_row = n_row,
                                    n_col = n_col,
                                    resolution = resolution,
                                    points = points,
                                    what = current_metric,
                                    directions = directions,
                                    count_boundary = count_boundary,
                                    consider_boundary = consider_boundary,
                                    edge_depth = edge_depth,
                                    classes_max = classes_max,
                                    neighbourhood = neighbourhood,
                                    ordered = ordered,
                                    base = base,
                                    verbose = verbose)},
                pad = TRUE, padValue = NA)
        })

        names(result_layer) <- metrics_list

        return(result_layer)
    })

    return(result)
}

#' @name window_lsm
#' @export
window_lsm.list <- function(landscape,
                            window,
                            metric = NULL,
                            name = NULL,
                            type = NULL,
                            what = NULL,
                            verbose = TRUE,
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

        stop("'window_lsm()' is only able to calculate landscape level metrics.",
             call. = FALSE)
    }

    result <- lapply(landscape, function(current_landscape) {

        # get coordinates of cells
        points <- raster_to_points(current_landscape)[, 2:4]

        # resolution of original raster
        resolution <- raster::res(current_landscape)

        # get dimensions of window
        n_row = nrow(window)
        n_col = ncol(window)

        result_layer <- lapply(metrics_list, function(current_metric) {

            raster::focal(x = current_landscape, w = window, fun = function(x) {

                calculate_lsm_focal(landscape = x,
                                    n_row = n_row,
                                    n_col = n_col,
                                    what = current_metric,
                                    resolution = resolution,
                                    points = points,
                                    directions = directions,
                                    count_boundary = count_boundary,
                                    consider_boundary = consider_boundary,
                                    edge_depth = edge_depth,
                                    classes_max = classes_max,
                                    neighbourhood = neighbourhood,
                                    ordered = ordered,
                                    base = base,
                                    verbose = verbose)},
                pad = TRUE, padValue = NA)
        })

        names(result_layer) <- metrics_list

        return(result_layer)
    })

    return(result)
}

calculate_lsm_focal <- function(landscape,
                                window,
                                metric,
                                name,
                                type,
                                what,
                                verbose,
                                progress,
                                ...) {

    # convert focal window to matrix
    raster_window <- matrix(landscape, n_row, n_col)

    # get internal calculation function
    what <- paste0(what, "_calc")

    # match function name
    foo <- get(what, mode = "function")

    # get argument
    arguments <- names(formals(foo))[-1]

    # get all values in environment
    arguments_values <- lapply(arguments, function(x)
        get0(x, envir = parent.env(environment())))

    names(arguments_values) <- arguments

    # give all arguments default if NULL
    # directions = 8,
    # count_boundary = FALSE,
    # consider_boundary = FALSE,
    # edge_depth = 1,
    # classes_max = NULL,
    # neighbourhood = 4,
    # ordered = TRUE,
    # base = "log2",

    arguments$landscape <- raster_window

    # run function
    result <- do.call(what = foo,
                      args = arguments)

    return(result$value)
}

