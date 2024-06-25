#' sample_lsm
#'
#' @description Sample metrics
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param y 2-column matrix with coordinates or sf point geometries.
#' @param plot_id Vector with id of sample points. If not provided, sample
#' points will be labelled 1...n.
#' @param shape String specifying plot shape. Either "circle" or "square"
#' @param size Approximated size of sample plot. Equals the radius for circles or half of
#' the side-length for squares in map units. For lines size equals the width of the buffer.
#' @param all_classes Logical if NA should be returned for classes not present in some sample plots.
#' @param return_raster Logical if the clipped raster of the sample plot should
#' be returned
#' @param verbose Print warning messages.
#' @param progress Print progress report.
#' @param ... Arguments passed on to \code{calculate_lsm()}.
#'
#' @details
#' This function samples the selected metrics in a buffer area (sample plot)
#' around sample points, sample lines or within provided polygons. The size of the actual
#' sampled landscape can be different to the provided size due to two reasons.
#' Firstly, because clipping raster cells using a circle or a sample plot not directly
#' at a cell center lead to inaccuracies. Secondly, sample plots can exceed the
#' landscape boundary. Therefore, we report the actual clipped sample plot area relative
#' in relation to the theoretical, maximum sample plot area e.g. a sample plot only half
#' within the landscape will have a `percentage_inside = 50`. Additionally, if the polygon
#' representing the sample plot is smaller than the cell size of the raster, 
#' the `percentage_inside` may exceed 100%.Please be aware that the
#' output is slightly different to all other `lsm`-function of `landscapemetrics`.
#'
#' Please be aware that the function behaves differently for POLYGONS and MULTIPOLYGONS.
#' In the first case, each polygon is used as a singular sample area, while in the second
#' case all polygons are used as one sample area.
#'
#' The metrics can be specified by the arguments `what`, `level`, `metric`, `name`
#' and/or `type` (combinations of different arguments are possible (e.g.
#' `level = "class", type = "aggregation metric"`). If an argument is not provided,
#' automatically all possibilities are selected. Therefore, to get **all**
#' available metrics, don't specify any of the above arguments.
#'
#' For all metrics based on distances or areas please make sure your data is valid
#' using \code{\link{check_landscape}}.
#'
#' @seealso
#' \code{\link{list_lsm}} \cr
#' \code{\link{calculate_lsm}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#'
#' # use a matrix
#' sample_points <- matrix(c(10, 5, 25, 15, 5, 25), ncol = 2, byrow = TRUE)
#' sample_lsm(landscape, y = sample_points, size = 15, what = "lsm_l_np")
#'
#' @export
sample_lsm <- function(landscape, y, plot_id = NULL, shape = "square", size = NULL,
                       all_classes = FALSE, return_raster = FALSE,
                       verbose = TRUE, progress = FALSE, ...) {

    landscape <- landscape_as_list(landscape)

    result <- lapply(X = seq_along(landscape), FUN = function(x) {

        if (progress) {

            cat("\r> Progress nlayers: ", x , "/", length(landscape))
        }

        sample_lsm_int(landscape = landscape[[x]],
                       y = y,
                       plot_id = plot_id,
                       shape = shape,
                       size = size,
                       all_classes = all_classes,
                       verbose = verbose,
                       progress = FALSE,
                       ...)
    })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    if (!return_raster) {
        result  <- result[, -9]
    }

    if (progress) {cat("\n")}

    result[with(result, order(layer, plot_id, level, metric, class, id)), ]
}

sample_lsm_int <- function(landscape, y, plot_id, shape, size,
                           all_classes, verbose, progress, ...) {

    # check if size argument is only one number
    if (!is.null(size) & (length(size) != 1 | any(size < 0))) {

        stop("Please provide only one value as size argument.", call. = FALSE)

    }

    # check if y is sf object
    if (inherits(x = y, what = "sf") | inherits(x = y, what = "sfc") | inherits(x = y, what = "sfg") |
        inherits(x = y, what = "SpatialPolygons") | inherits(x = y, what = "SpatVector")) {

        # convert to terra
        y <- methods::as(y, "SpatVector")

        # get crs
        crs <- terra::crs(y)

        if (terra::geomtype(y) == "points") {

            if (is.null(size) | size == 0) stop("Please provide size argument size > 0.", call. = FALSE)

            y <- construct_buffer(coords = y, shape = shape, size = size,
                                  return_vec = TRUE, crs = crs, verbose = verbose)

        }

    # y should be matrix or points
    } else if (inherits(x = y, what = "matrix")) {

        if (is.null(size)) stop("Please provide size argument.", call. = FALSE)

        y <- construct_buffer(coords = y, shape = shape, size = size,
                              return_vec = TRUE, verbose = verbose)

    } else {

        stop("Please provide a matrix with coords, points or polygons object.", call. = FALSE)

    }

    # check if y is a polygon
    if (terra::geomtype(y) != "polygons") stop("Please provide polygon object.", call. = FALSE)

    # check if length is identical if ids are provided
    if (!is.null(plot_id)) {

        if (length(plot_id) != nrow(y)) {

            if (verbose) {
                warning("Length of plot_id is not identical to length of y. Using 1...n as plot_id.",
                        call. = FALSE)
            }

            plot_id <- NULL
        }
    }

    # get area of all polygons
    maximum_area <- suppressWarnings(terra::expanse(y)) / 10000

    number_plots <- nrow(y)

    # create object for warning messages
    warning_messages <- character(0)

    # loop through each sample point and calculate metrics
    result <- withCallingHandlers(expr = {do.call(rbind, lapply(X = 1:number_plots,
                                                                FUN = function(current_plot) {

        # print progess using the non-internal name
        if (progress) {

            cat("\r> Progress sample plots: ", current_plot, "/", number_plots)
        }

        # crop sample plot
        landscape_mask <- terra::crop(x = landscape, y = y[current_plot, ], mask = TRUE)

        # calculate actual area of sample plot
        area <- lsm_l_ta_calc(landscape_mask, directions = 8)

        # calculate lsm
        result_current_plot <- calculate_lsm(landscape = landscape_mask,
                                             verbose = verbose,
                                             progress = FALSE,
                                             ...)

        # add plot id 1...n
        if (is.null(plot_id)) {

            result_current_plot$plot_id <- current_plot

        # add plot_id
        } else {
            result_current_plot$plot_id <- plot_id[current_plot]
        }

        # all cells are NA
        if (all(is.na(terra::values(landscape_mask, mat = FALSE)))) {

            # calculate ratio between actual area and theoretical area
            result_current_plot$percentage_inside <- 0
        } else {

            # calculate ratio between actual area and theoretical area
            result_current_plot$percentage_inside <- area$value /
                maximum_area[[current_plot]] * 100
        }

        # add sample plot raster
        result_current_plot$raster_sample_plots <- terra::as.list(landscape_mask)

        return(result_current_plot)}))}, warning = function(cond) {

            warning_messages <<- c(warning_messages, conditionMessage(cond))

            invokeRestart("muffleWarning")}
    )

    if (progress) {

        cat("\n")
    }

    # add all_classes if class is present in tibble
    if (all_classes && "class" %in% result$level) {

        # get all present classes
        all_classes <- unique(terra::values(landscape, mat = FALSE))

        # only results on class level are needed
        result_class <- result[result$level == "class", ]

        # get all possible combination of all metrics and classes in each plot
        all_combinations <- expand.grid(class = all_classes,
                                        metric = unique(result_class$metric),
                                        plot_id = unique(result_class$plot_id),
                                        stringsAsFactors = FALSE)

        # add NA values for classes not present in certain plots
        all_combinations <- merge(x = all_combinations,
                                  y = result_class[, c("class", "metric",
                                                       "value", "plot_id")],
                                  by = c("class", "metric", "plot_id"),
                                  all.x = TRUE)

        # add information about unique study plots
        all_combinations <- merge(x = all_combinations,
                                  y = unique(result_class[, c("layer", "level", "id",
                                                              "plot_id",
                                                              "percentage_inside",
                                                              "raster_sample_plots")]),
                                  by = "plot_id", all.x = TRUE)

        # reorder cols
        all_combinations <- all_combinations[, names(result)]

        # remove all class level results
        result <- result[!result$level == "class", ]

        # exchange with all combinations
        result <- tibble::as_tibble(rbind(result, all_combinations))
    }

    # return warning of only 3/4 of sample plot are in landscape
    if (verbose) {
        if (any(result$percentage_inside < 90)) {

            warning("The 'perecentage_inside' is below 90% for at least one buffer.",
                    call. = FALSE)
        }
    }

    # warnings present
    if (length(warning_messages) > 0) {

        # only unique warnings
        warning_messages <- unique(warning_messages)

        # print warnings
        lapply(warning_messages, function(x){ warning(x, call. = FALSE)})
    }

    return(result)
}
