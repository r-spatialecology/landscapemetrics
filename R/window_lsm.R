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
#'
#' @details
#' < ADD TEXT HERE >
#'
#' @seealso
#' \code{\link{list_lsm}}
#'
#' @return tibble
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
                           base) UseMethod("window_lsm")


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
                                       base = "log2") {

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

    result <- lapply(raster::as.list(landscape), function(current_landscape) {

        result_layer <- lapply(metrics_list, function(current_metric) {

            raster::focal(x = current_landscape, w = window, fun = function(x) {

                calculate_lsm_focal(landscape = x,
                                        nrow = nrow(window),
                                        ncol = ncol(window),
                                        what = current_metric,
                                        directions = directions,
                                        count_boundary = count_boundary,
                                        consider_boundary = consider_boundary,
                                        edge_depth = edge_depth,
                                        classes_max = classes_max,
                                        neighbourhood = neighbourhood,
                                        ordered = ordered,
                                        base = base)},
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
                                       base = "log2") {

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

    result <- lapply(raster::as.list(landscape), function(current_landscape) {

        result_layer <- lapply(metrics_list, function(current_metric) {

            raster::focal(x = current_landscape, w = window, fun = function(x) {

                calculate_lsm_focal(landscape = x,
                                    nrow = nrow(window),
                                    ncol = ncol(window),
                                    what = current_metric,
                                    directions = directions,
                                    count_boundary = count_boundary,
                                    consider_boundary = consider_boundary,
                                    edge_depth = edge_depth,
                                    classes_max = classes_max,
                                    neighbourhood = neighbourhood,
                                    ordered = ordered,
                                    base = base)},
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
                                       base = "log2") {

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

    result <- lapply(raster::as.list(landscape), function(current_landscape) {

        result_layer <- lapply(metrics_list, function(current_metric) {

            raster::focal(x = current_landscape, w = window, fun = function(x) {

                calculate_lsm_focal(landscape = x,
                                    nrow = nrow(window),
                                    ncol = ncol(window),
                                    what = current_metric,
                                    directions = directions,
                                    count_boundary = count_boundary,
                                    consider_boundary = consider_boundary,
                                    edge_depth = edge_depth,
                                    classes_max = classes_max,
                                    neighbourhood = neighbourhood,
                                    ordered = ordered,
                                    base = base)},
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
                                 base = "log2") {

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

    result <- lapply(metrics_list, function(current_metric) {

        raster::focal(x = landscape, w = window, fun = function(x) {

            calculate_lsm_focal(landscape = x,
                                nrow = nrow(window),
                                ncol = ncol(window),
                                what = current_metric,
                                directions = directions,
                                count_boundary = count_boundary,
                                consider_boundary = consider_boundary,
                                edge_depth = edge_depth,
                                classes_max = classes_max,
                                neighbourhood = neighbourhood,
                                ordered = ordered,
                                base = base)},
            pad = TRUE, padValue = NA)
    })

    names(result) <- metrics_list

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
                                base = "log2") {

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

    result <- lapply(landscape, function(current_landscape) {

        result_layer <- lapply(metrics_list, function(current_metric) {

            raster::focal(x = current_landscape, w = window, fun = function(x) {

                calculate_lsm_focal(landscape = x,
                                    nrow = nrow(window),
                                    ncol = ncol(window),
                                    what = current_metric,
                                    directions = directions,
                                    count_boundary = count_boundary,
                                    consider_boundary = consider_boundary,
                                    edge_depth = edge_depth,
                                    classes_max = classes_max,
                                    neighbourhood = neighbourhood,
                                    ordered = ordered,
                                    base = base)},
                pad = TRUE, padValue = NA)
        })

        names(result_layer) <- metrics_list

        return(result_layer)
    })

    return(result)
}

calculate_lsm_focal <- function(landscape, nrow, ncol, what,
                                    directions,
                                    count_boundary,
                                    consider_boundary,
                                    edge_depth,
                                    classes_max,
                                    neighbourhood,
                                    ordered,
                                    base) {

    raster_window <- matrix(landscape, nrow, ncol)

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

