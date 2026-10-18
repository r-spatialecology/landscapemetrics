#' window_lsm
#'
#' @description Moving window
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
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
#' details, see \code{?terra::focal()}. The result will be a \code{RasterLayer} in which each focal cell includes
#' the value of its neighbourhood and thereby allows to show gradients and variability in the landscape (Hagen-Zanker 2016).
#' To be type stable, the actual result is always a nested list (first level for \code{RasterStack} layers, second level
#' for selected landscape metrics).
#'
#' Note, that in situations when the moving window does not contain any patches, the result is NA.
#'
#' @seealso
#' \code{\link{list_lsm}} \cr
#' \code{\link{calculate_lsm}} \cr
#' \code{\link[terra]{focal}}
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' landscape <- terra::rast(landscapemetrics::landscape)
#' landscape_stack <- c(landscape, landscape)
#' window <- matrix(1, nrow = 5,ncol = 5)
#' window_lsm(landscape, window = window, what = c("lsm_l_pr", "lsm_l_joinent"))
#' window_lsm(landscape_stack, window = window, what = c("lsm_l_pr", "lsm_l_joinent"))
#'
#' window_circular <- matrix(c(NA, 1, NA, 1, 1, 1, NA, 1, NA), nrow = 3, ncol = 3)
#' window_lsm(landscape, window = window_circular, what = c("lsm_l_pr", "lsm_l_joinent"))
#' }
#'
#' @references
#' Fletcher, R., Fortin, M.-J. 2018. Spatial Ecology and Conservation Modeling: Applications
#' with R. Springer International Publishing. 523 pages
#'
#' Hagen-Zanker, A. (2016). A computational framework for generalized moving windows
#' and its application to landscape pattern analysis. International journal of applied
#' earth observation and geoinformation, 44, 205-216.
#'
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
window_lsm <- function(landscape,
                            window,
                            level = "landscape",
                            metric = NULL,
                            name = NULL,
                            type = NULL,
                            what = NULL,
                            progress = FALSE,
                            ...) {

    landscape <- landscape_as_list(landscape)

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

    names(result) <- paste0("layer_", seq_along(result))

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

    resolution <- terra::res(landscape)

    arguments_values <- list(directions = 8,
                            count_boundary = FALSE,
                            consider_boundary = FALSE,
                            edge_depth = 1,
                            classes_max = NULL,
                            neighbourhood = 4,
                            ordered = TRUE,
                            base = "log2",
                            resolution = resolution,
                            verbose = TRUE)

    input_arguments <- list(...)
    arguments_values[names(input_arguments)] <- input_arguments

    # create object for warning messages
    warning_messages <- character(0)

    # get internal calculation functions and their arguments
    metric_functions <- lapply(paste0(metrics_list, "_calc"), function(what) {

        get(what, mode = "function")
    })

    metric_arguments <- lapply(metric_functions, function(foo) {

        arguments <- setdiff(names(formals(foo)), c("landscape", "extras"))
        arguments_values[names(arguments_values) %in% arguments]
    })

    metric_uses_extras <- vapply(metric_functions, function(foo) {

        "extras" %in% names(formals(foo))
    }, FUN.VALUE = logical(1))

    window_cells <- !is.na(window)

    focal_fun <- function(values) {

        raster_window <- window
        raster_window[window_cells] <- values[window_cells]

        extras <- prepare_extras(
            metrics = metrics_list,
            landscape_mat = raster_window,
            directions = arguments_values$directions,
            neighbourhood = arguments_values$neighbourhood,
            ordered = arguments_values$ordered,
            base = arguments_values$base,
            resolution = arguments_values$resolution
        )

        vapply(seq_along(metric_functions), function(i) {

            arguments <- c(list(landscape = raster_window),
                           metric_arguments[[i]])

            if (metric_uses_extras[[i]]) {

                arguments$extras <- extras
            }

            do.call(what = metric_functions[[i]], args = arguments)$value
        }, FUN.VALUE = numeric(1))
    }

    result <- withCallingHandlers(expr = {
        result_raster <- terra::focal(
            x = landscape,
            w = dim(window),
            fun = focal_fun,
            fillvalue = NA
        )
        names(result_raster) <- rep(names(landscape), number_metrics)

        lapply(seq_len(number_metrics), function(x) result_raster[[x]])
    },
        warning = function(cond) {

            warning_messages <<- c(warning_messages, conditionMessage(cond))

            invokeRestart("muffleWarning")})

    names(result) <- metrics_list

    if (progress) {cat("\n")}

    # warnings present
    if (length(warning_messages) > 0) {

        # only unique warnings
        warning_messages <- unique(warning_messages)

        # print warnings
        lapply(warning_messages, function(x){warning(x, call. = FALSE)})
    }

    return(result)
}
