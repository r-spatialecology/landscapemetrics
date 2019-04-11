#' window_lsm
#'
#' @description Moving window
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param window matrix
#' @param what Selected level of metrics: either "patch", "class" or "landscape".
#' It is also possible to specify functions as a vector of strings, e.g. `what = c("lsm_c_ca", "lsm_l_ta")`.
#' @param level Level of metrics to calculate (e.g. 'landscape').
#' @param metric Abbreviation of metrics to calculate (e.g. 'area').
#' @param name Full name of metrics to calculate (e.g. 'core area').
#' @param type Metric types to calculate according to FRAGSTATS grouping (e.g. 'aggregation metric').
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param count_boundary Include landscape boundary in edge length
#' @param consider_boundary Logical if cells that only neighbour the landscape
#' boundary should be considered as core
#' @param edge_depth Distance (in cells) a cell has the be away from the patch
#' edge to be considered as core cell
#' @param classes_max Potential maximum number of present classes
#' @param neighbourhood The number of directions in which cell adjacencies are considered as neighbours:
#' 4 (rook's case) or 8 (queen's case). The default is 4.
#' @param ordered The type of pairs considered. Either ordered (TRUE) or unordered (FALSE).
#' The default is TRUE.
#' @param base The unit in which entropy is measured. The default is "log2",
#' which compute entropy in "bits". "log" and "log10" can be also used.
#' @param verbose Print warning messages
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
                       what, level, metric, name, type,
                       directions,
                       count_boundary,
                       consider_boundary,
                       edge_depth,
                       classes_max,
                       neighbourhood,
                       ordered,
                       base,
                       verbose) UseMethod("window_lsm")


#' @name window_lsm
#' @export
window_lsm.RasterLayer <- function(landscape,
                                   window,
                                   what = NULL,
                                   level = NULL,
                                   metric = NULL,
                                   name = NULL,
                                   type = NULL,
                                   directions = 8,
                                   count_boundary = FALSE,
                                   consider_boundary = FALSE,
                                   edge_depth = 1,
                                   classes_max = NULL,
                                   neighbourhood = 4,
                                   ordered = TRUE,
                                   base = "log2",
                                   verbose = TRUE) {

    # get list of metrics to calculate
    metrics_list <- landscapemetrics::list_lsm(level = level,
                                               metric = metric,
                                               name = name,
                                               type = type,
                                               what = what,
                                               simplify = TRUE,
                                               verbose = FALSE)

    # check if non-landscape-level metrics are selected
    if (!all(metrics_list %in% landscapemetrics::list_lsm(level = "landscape",
                                                          simplify = TRUE))) {

        stop("'window_lsm()' is only able to calculate landscape level metrics.",
             call. = FALSE)
    }

    result <- lapply(raster::as.list(landscape), function(current_landscape) {

        # get coordinates of cells
        points <- raster_to_points(current_landscape)

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
                                   what = NULL,
                                   level = NULL,
                                   metric = NULL,
                                   name = NULL,
                                   type = NULL,
                                   directions = 8,
                                   count_boundary = FALSE,
                                   consider_boundary = FALSE,
                                   edge_depth = 1,
                                   classes_max = NULL,
                                   neighbourhood = 4,
                                   ordered = TRUE,
                                   base = "log2",
                                   verbose = TRUE) {

    # get list of metrics to calculate
    metrics_list <- landscapemetrics::list_lsm(level = level,
                                               metric = metric,
                                               name = name,
                                               type = type,
                                               what = what,
                                               simplify = TRUE,
                                               verbose = FALSE)

    # check if non-landscape-level metrics are selected
    if (!all(metrics_list %in% landscapemetrics::list_lsm(level = "landscape",
                                                          simplify = TRUE))) {

        stop("'window_lsm()' is only able to calculate landscape level metrics.",
             call. = FALSE)
    }

    result <- lapply(raster::as.list(landscape), function(current_landscape) {

        # get coordinates of cells
        points <- raster_to_points(current_landscape)

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
                                   what = NULL,
                                   level = NULL,
                                   metric = NULL,
                                   name = NULL,
                                   type = NULL,
                                   directions = 8,
                                   count_boundary = FALSE,
                                   consider_boundary = FALSE,
                                   edge_depth = 1,
                                   classes_max = NULL,
                                   neighbourhood = 4,
                                   ordered = TRUE,
                                   base = "log2",
                                   verbose = TRUE) {

    # get list of metrics to calculate
    metrics_list <- landscapemetrics::list_lsm(level = level,
                                               metric = metric,
                                               name = name,
                                               type = type,
                                               what = what,
                                               simplify = TRUE,
                                               verbose = FALSE)

    # check if non-landscape-level metrics are selected
    if (!all(metrics_list %in% landscapemetrics::list_lsm(level = "landscape",
                                                          simplify = TRUE))) {

        stop("'window_lsm()' is only able to calculate landscape level metrics.",
             call. = FALSE)
    }

    result <- lapply(raster::as.list(landscape), function(current_landscape) {

        # get coordinates of cells
        points <- raster_to_points(current_landscape)

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
                             what = NULL,
                             level = NULL,
                             metric = NULL,
                             name = NULL,
                             type = NULL,
                             directions = 8,
                             count_boundary = FALSE,
                             consider_boundary = FALSE,
                             edge_depth = 1,
                             classes_max = NULL,
                             neighbourhood = 4,
                             ordered = TRUE,
                             base = "log2",
                             verbose = TRUE) {

    # get list of metrics to calculate
    metrics_list <- landscapemetrics::list_lsm(level = level,
                                               metric = metric,
                                               name = name,
                                               type = type,
                                               what = what,
                                               simplify = TRUE,
                                               verbose = FALSE)

    # check if non-landscape-level metrics are selected
    if (!any(metrics_list %in% landscapemetrics::list_lsm(level = "landscape",
                                                          simplify = TRUE))) {
        stop("extract_lsm only takes landscape level metrics as what argument.",
             call. = FALSE)
    }

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(raster::as.list(landscape), function(current_landscape) {

        # get coordinates of cells
        points <- raster_to_points(current_landscape)

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
                            what = NULL,
                            level = NULL,
                            metric = NULL,
                            name = NULL,
                            type = NULL,
                            directions = 8,
                            count_boundary = FALSE,
                            consider_boundary = FALSE,
                            edge_depth = 1,
                            classes_max = NULL,
                            neighbourhood = 4,
                            ordered = TRUE,
                            base = "log2",
                            verbose = TRUE) {

    # get list of metrics to calculate
    metrics_list <- landscapemetrics::list_lsm(level = level,
                                               metric = metric,
                                               name = name,
                                               type = type,
                                               what = what,
                                               simplify = TRUE,
                                               verbose = FALSE)

    # check if non-landscape-level metrics are selected
    if (!all(metrics_list %in% landscapemetrics::list_lsm(level = "landscape",
                                                          simplify = TRUE))) {

        stop("'window_lsm()' is only able to calculate landscape level metrics.",
             call. = FALSE)
    }

    result <- lapply(landscape, function(current_landscape) {

        # get coordinates of cells
        points <- raster_to_points(current_landscape)

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
                                n_row, n_col,
                                resolution,
                                points,
                                what,
                                directions,
                                count_boundary,
                                consider_boundary,
                                edge_depth,
                                classes_max,
                                neighbourhood,
                                ordered,
                                base,
                                verbose) {

    # convert focal window to matrix
    raster_window <- matrix(landscape, n_row, n_col)

    what <- paste0(what, "_calc")

    # match function name
    foo <- get(what, mode = "function")

    # get argument
    arguments <- names(formals(foo))[-1]

    arguments <- mget(arguments)

    arguments$landscape <- raster_window

    # run function
    result <- do.call(what = foo,
                      args = arguments)

    return(result$value)
}

